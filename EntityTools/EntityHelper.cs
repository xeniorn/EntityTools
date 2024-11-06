using System.Collections;
using System.Diagnostics;
using System.Linq.Expressions;
using System.Reflection;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata;

namespace EntityTools;

public static class EntityHelper
{
    /// <summary>
    /// Check if DbContext contains any entity of the given Clr type
    /// </summary>
    /// <param name="db"></param>
    /// <param name="entityType"></param>
    /// <returns></returns>
    public static bool DatabaseContainsType(DbContext db, Type entityType)
    {
        return db.Model.FindEntityType(entityType) is not null;
    }

    /// <summary>
    /// Get entities from the db for which the target property matches one of the provided inputs.
    /// Returns also the unmatched values for convenience.
    /// </summary>
    /// <typeparam name="TEntity"></typeparam>
    /// <typeparam name="TValue"></typeparam>
    /// <param name="source"></param>
    /// <param name="values"></param>
    /// <param name="matchProperty"></param>
    /// <param name="maxComparisonsPerQuery"></param>
    /// <param name="mustIncludeProperties"></param>
    /// <param name="maxComparisonsPerBatch"></param>
    /// <param name="token"></param>
    /// <returns></returns>
    public static async Task<(IReadOnlyCollection<TEntity> Existing, IReadOnlyCollection<TValue> Unmatched)>
        FetchEntitiesByValue<TEntity, TValue>(
            IQueryable<TEntity> source,
            IEnumerable<TValue> values,
            PropertyInfo matchProperty,
            int maxComparisonsPerQuery,
            IEnumerable<PropertyInfo>? mustIncludeProperties = null,
            int maxComparisonsPerBatch = 10000,
            CancellationToken token = default)
        where TEntity : class
        where TValue : struct, IEquatable<TValue>
    {
        if (matchProperty.PropertyType != typeof(TValue))
            throw new ArgumentException(
                $"Type mismatch between the provided property {matchProperty.Name} ({matchProperty.PropertyType}) and the public references ({typeof(TValue)})");

        // I have to .Include all the Navigations for Value properties from the database
        var sourceWithIncludes = source;
        foreach (var prop in mustIncludeProperties ?? [])
        {
            sourceWithIncludes = ApplyInclude(sourceWithIncludes, prop);
        }

        var uniqueSearchValues = values.Distinct().ToList();

        var allExisting = new List<TEntity>();

        await foreach (var (filterExpression, batch) in GetExpressionFor_MatchAnyValue_Async<TEntity, TValue>(
                               uniqueSearchValues,
                               matchProperty,
                               maxComparisonsPerQuery)
                           .WithCancellation(token))
        {
            var existing = await sourceWithIncludes
                .Where(filterExpression)
                .ToListAsync(token);

            allExisting.AddRange(existing);
        }

        var matchedVals = allExisting
            .Select(entity => (TValue)matchProperty.GetValue(entity)!)
            .ToList();

        var unmatchedVals = uniqueSearchValues.Except(matchedVals).ToList();

        return (allExisting, unmatchedVals);
    }

    /// <summary>
    /// Get properties considered to be defining for value equality, according to <see cref="UsedInEntityByValueComparisonAttribute"/>
    /// </summary>
    /// <typeparam name="T">Type for which equality props are being obtained</typeparam>
    /// <returns></returns>
    /// <exception cref="ArgumentException">If the type does not have any properties with the attribute</exception>
    /// <exception cref="UnreachableException"></exception>
    public static IEnumerable<PropertyInfo> GetEqualityProperties<T>() where T : class =>
        GetEqualityProperties(typeof(T));

    /// <summary>
    /// Get properties considered to be defining for value equality, according to <see cref="UsedInEntityByValueComparisonAttribute"/>
    /// </summary>
    /// <param name="type">Type for which equality props are being obtained</param>
    /// <returns></returns>
    /// <exception cref="ArgumentException"></exception>
    /// <exception cref="UnreachableException"></exception>
    public static IEnumerable<PropertyInfo> GetEqualityProperties(Type type)
    {
        if (!type.IsClass) throw new ArgumentException($"Input type must be a class; {type.Name} is not a class.");

        var equalityProperties = type
            .GetProperties()
            .Where(IsEqualityPropertyAccordingToAttributes)
            .ToList();

        if (equalityProperties.Count == 0)
            throw new ArgumentException($"Cannot get equality properties for the provided type {type.Name}," +
                                        $" as it does not contain any properties with {nameof(UsedInEntityByValueComparisonAttribute)}");

        return equalityProperties;

        #region helpers
        throw new UnreachableException();

        bool IsEqualityPropertyAccordingToAttributes(PropertyInfo p)
        {
            return Attribute.IsDefined(p, typeof(UsedInEntityByValueComparisonAttribute));
        }
        #endregion helpers
    }

    /// <summary>
    /// Given a DbContext source and a list of target entities, test which targets exist in the source according to attribute-based value equality.
    /// first-order nav properties will be automatically included.
    /// <see cref="UsedInEntityByValueComparisonAttribute"/>
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="db"></param>
    /// <param name="targets"></param>
    /// <param name="maxComparisonsPerBatch"></param>
    /// <param name="token"></param>
    /// <returns></returns>
    /// <exception cref="ArgumentException"></exception>
    public static Task<(IReadOnlyCollection<T> Existing, IReadOnlyCollection<T> Missing)>
            GetExistingAndMissingEntitiesByValue<T>(
                DbContext db,
                IEnumerable<T> targets,
                int maxComparisonsPerBatch = 10000,
                CancellationToken token = default)
            where T : class
    {
        var entityType = typeof(T);

        if (!DatabaseContainsType(db, entityType))
            throw new ArgumentException(
                $"Provided DbContext of type {db.GetType()} does not contain an entity of type {entityType}");

        if (!IsValueLikeEntity<T>())
            throw new ArgumentException($"Entity must be a Value-like entity (see {typeof(UsedInEntityByValueComparisonAttribute)})");

        var comparisonProps = GetEqualityProperties<T>().ToList();

        var navigationComparisonProps = comparisonProps.Where(x => PropertyIsNavigation<T>(db, x)).ToList();

        var navWithIds = navigationComparisonProps.Select(x => (NavProperty: x, IdProperty: GetIdPropertyOfNavigationTarget<T>(db, x))).ToList();

        var source = db.Set<T>().AsQueryable();

        return GetExistingAndMissingEntitiesByValue(source, targets, navWithIds, maxComparisonsPerBatch, token);
    }

    /// <summary>
    /// Given a source as IQueryable and a list of target entities, test which targets exist in the source.
    /// If nav properties included, must include those in input so that they can be .Include()-d
    /// </summary>
    /// <typeparam name="T">Type of the entity being tested</typeparam>
    /// <param name="source">Source being searched. Typically a DbSet. Designed for a postgres-based DbContext</param>
    /// <param name="items">Entities existence of which is being tested. Result will contain this set split into two groups (existing, missing)</param>
    /// <param name="navsWithIds">Nav properties have to be treated differently and therefore need to be specified</param>
    /// <param name="maxComparisonsPerQuery">Force the query to run in batches no larger than the provided parameter. Set to null to automatically select batch size.</param>
    /// <param name="token"></param>
    /// <returns>List of targets that exist in the source and a list of targets that are missing in the source.</returns>
    /// <exception cref="ArgumentException"></exception>
    /// <exception cref="UnreachableException"></exception>
    /// <summary>
    public static async Task<(IReadOnlyCollection<T> Existing, IReadOnlyCollection<T> Missing)> GetExistingAndMissingEntitiesByValue<T>(
        IQueryable<T> source,
        IEnumerable<T> items,
        IEnumerable<(PropertyInfo NavProperty, PropertyInfo IdProperty)>? navsWithIds = null,
        int maxComparisonsPerQuery = 10_000,
        CancellationToken token = default)
        where T : class
    {
        var equalityProperties = GetEqualityProperties<T>().ToList();
        if (!equalityProperties.Any())
            throw new ArgumentException($"No equality properties on {typeof(T)}");

        // I have to .Include all the Navigations for Value properties from the database
        var sourceWithIncludes = source;
        foreach (var prop in navsWithIds?.Distinct().Select(x=>x.NavProperty) ?? [])
        {
            sourceWithIncludes = ApplyInclude(sourceWithIncludes, prop);
        }

        var uniqueSearchItems = items.Distinct().ToList();

        var allExisting = new List<T>();
        var allMissing = new List<T>();

        await foreach (var (filterExpression, batch) in GetExpressionFor_MatchAnyOfItemsByValue_Async(
                           uniqueSearchItems,
                           equalityProperties,
                           navsWithIds ?? [],
                           maxComparisonsPerQuery)
                           .WithCancellation(token))
        {
            var existing = await sourceWithIncludes
                .Where(filterExpression)
                .ToListAsync(token);

            // Normally it would be a problem to compare a navigation property, but here it would have already failed in the first query, i.e. we don't need to check again
            // this is done in-memory, no need for batching and stuff (anyhow we are working on a batch already)
            var filterMatchedExpression = GetExpressionFor_MatchAnyOfItemsByValue(existing, equalityProperties, navsWithIds ?? []);

            var filterNotMatchedExpression = InvertBooleanExpression(filterMatchedExpression);
            // for normal LINQ to Enumerable
            var filterNotMatchedPredicate = filterNotMatchedExpression.Compile();

            var missing = batch.Where(filterNotMatchedPredicate).ToList();

            allExisting.AddRange(existing);
            allMissing.AddRange(missing);
        }

        return (allExisting, allMissing);
    }

    /// <summary>
    /// Each Expression will match items in source where the property is equal to one of the values in the batch.
    /// Multiple batches can be returned if the number of items exceeds the allowable comparisons per batch.
    /// </summary>
    /// <typeparam name="TEntity"></typeparam>
    /// <typeparam name="TValue"></typeparam>
    /// <param name="equalityComparisonValues"></param>
    /// <param name="matchProperty"></param>
    /// <param name="maxComparisonsPerBatch"></param>
    /// <returns></returns>
    /// <exception cref="ArgumentException"></exception>
    public static async IAsyncEnumerable<(Expression<Func<TEntity, bool>> ExpressionForBatch, IReadOnlyCollection<TValue> ValuesInBatch)>
        GetExpressionFor_MatchAnyValue_Async<TEntity, TValue>(
            IEnumerable<TValue> equalityComparisonValues,
            PropertyInfo matchProperty,
            int maxComparisonsPerBatch)
        where TEntity : class
        where TValue : IEquatable<TValue>
    {
        if (matchProperty.PropertyType != typeof(TValue))
            throw new ArgumentException(
                $"Type mismatch between the provided property {matchProperty.Name} ({matchProperty.PropertyType}) and the public references ({typeof(TValue)})");

        var valuesToPlowThrough = equalityComparisonValues.Distinct().ToList();

        if (valuesToPlowThrough.Count == 0)
        {
            yield break;
        }

        var allDone = false;
        while (true)
        {
            var (batchExpression, batchItems) =
                GetPartialExpressionFor_MatchAnyValue<TEntity, TValue>(valuesToPlowThrough, matchProperty,
                    maxComparisonsPerBatch);

            if (batchItems.Count == 0)
                throw new ArgumentException($"Unable to get expression for batch equality of {nameof(TEntity)} items by property {matchProperty.Name}," +
                                            $" as not even a single item fits in the given comparison count constraints ({maxComparisonsPerBatch})." +
                                            $" Attempting to do so would lead to an infinite loop. Either adjust the constraints or eliminate the problematic item(s).");

            yield return (batchExpression, batchItems);

            allDone = (batchItems.Count == valuesToPlowThrough.Count);

            if (allDone)
            {
                yield break;
            }

            // remove done stuff prior to next iteration
            foreach (var batchItem in batchItems)
            {
                valuesToPlowThrough.Remove(batchItem);
            }
        }
    }

    public static bool IsValueLikeEntity<T>() where T : class => IsValueLikeEntity(typeof(T));

    public static bool IsValueLikeEntity(Type type) => GetEqualityProperties(type).Any();

    /// <summary>
    /// Dynamically adds an ".Include" to an IQueryable, to be used for DbSets from a DbContext
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="query"></param>
    /// <param name="property"></param>
    /// <returns></returns>
    private static IQueryable<T> ApplyInclude<T>(IQueryable<T> query, System.Reflection.PropertyInfo property)
    {
        // Generate the expression for the property to be included
        var parameter = Expression.Parameter(typeof(T), "x");
        var propertyAccess = Expression.Property(parameter, property.Name);
        var lambda = Expression.Lambda(propertyAccess, parameter);

        // Call the generic Include method dynamically
        var includeMethod = typeof(EntityFrameworkQueryableExtensions)
            .GetMethods()
            .First(m => m.Name == "Include" && m.GetParameters().Length == 2)
            .MakeGenericMethod(typeof(T), property.PropertyType);

        return (IQueryable<T>)includeMethod.Invoke(null, new object[] { query, lambda });
    }

    /// <summary>
    /// Generates equality expression that tests for entries that have all the same Value-defining properties as one of the inputs
    /// works both for nav and non nav properties, including collection properties
    ///
    /// For simple props,
    /// 
    /// For collections, will generate an expression along the lines of:
    /// source.Where(x =>
    ///     (x.Prop1.Contains(testVal1[0]) && x.Prop1.Contains(testVal1[1]))
    /// || (x.Prop1.Contains(testVal1[0]) && x.Prop1.Contains(testVal1[1]) && x.Prop1.Contains(testVal1[2]))
    /// 
    /// Can be called repeatedly on leftover items which did not end up in "processed",based on return value
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="equalityComparisonItems"></param>
    /// <param name="equalityProperties"></param>
    /// <param name="maxComparisonsPerBatch"></param>
    /// <returns>Expressions for comparing to input items, batched so that they always fit into the max comparisons per batch limit</returns>
    /// <exception cref="ArgumentException"></exception>
    private static IEnumerable<(Expression<Func<T, bool>> ExpressionForBatch, IReadOnlyCollection<T> ItemsInBatch)>
        GetExpressionFor_MatchAnyOfItemsByValue<T>(
            IEnumerable<T> equalityComparisonItems,
            IEnumerable<PropertyInfo> equalityProperties,
            IEnumerable<(PropertyInfo NavProperty, PropertyInfo IdProperty)> navsWithIds,
            int maxComparisonsPerBatch)
        where T : class =>
        GetExpressionFor_MatchAnyOfItemsByValue_Async(
            equalityComparisonItems,
            equalityProperties,
            navsWithIds,
            maxComparisonsPerBatch)
            .ToBlockingEnumerable();

    /// <summary>
    /// Gets the expression for comparison of items. No limit in number of comparisons, therefore a single batch only.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="equalityComparisonItems"></param>
    /// <param name="equalityProperties"></param>
    /// <returns></returns>
    private static Expression<Func<T, bool>>
        GetExpressionFor_MatchAnyOfItemsByValue<T>(
            IEnumerable<T> equalityComparisonItems,
            IEnumerable<PropertyInfo> equalityProperties,
            IEnumerable<(PropertyInfo NavProperty, PropertyInfo IdProperty)> navsWithIds) where T : class
        => GetExpressionFor_MatchAnyOfItemsByValue(
                equalityComparisonItems,
                equalityProperties,
                navsWithIds,
                Int32.MaxValue)
            .Single().ExpressionForBatch;

    /// <summary>
    /// Each Expression will match items in source which match one of the input items.
    /// Multiple batches can be returned if the number of items exceeds the allowable comparisons per batch.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="equalityComparisonItems"></param>
    /// <param name="equalityProperties"></param>
    /// <param name="maxComparisonsPerBatch"></param>
    /// <returns></returns>
    /// <exception cref="ArgumentException"></exception>
    private static async IAsyncEnumerable<(Expression<Func<T, bool>> ExpressionForBatch, IReadOnlyCollection<T> ItemsInBatch)>
        GetExpressionFor_MatchAnyOfItemsByValue_Async<T>(
            IEnumerable<T> equalityComparisonItems,
            IEnumerable<PropertyInfo> equalityProperties,
            IEnumerable<(PropertyInfo NavProperty, PropertyInfo IdProperty)> navsWithIds,
            int maxComparisonsPerBatch)
        where T : class
    {
        var itemsToPlowThrough = equalityComparisonItems.Distinct().ToList();

        if (itemsToPlowThrough.Count == 0)
        {
            yield break;
        }

        var allDone = false;
        while (true)
        {
            var (batchExpression, batchItems) =
                GetPartialExpressionFor_MatchAnyOfItemsByValue(itemsToPlowThrough, equalityProperties, navsWithIds, maxComparisonsPerBatch);

            if (batchItems.Count == 0)
                throw new ArgumentException($"Unable to get expression for batch equality of {nameof(T)} items," +
                                            $" as not even a single item fits in the given comparison count constraints ({maxComparisonsPerBatch})." +
                                            $" Attempting to do so would lead to an infinite loop. Either adjust the constraints or eliminate the problematic item(s).");

            yield return (batchExpression, batchItems);

            allDone = (batchItems.Count == itemsToPlowThrough.Count);

            if (allDone)
            {
                yield break;
            }

            // remove done stuff prior to next iteration
            foreach (var batchItem in batchItems)
            {
                itemsToPlowThrough.Remove(batchItem);
            }
        }
    }
    /// <summary>
    /// Get a batch without knowing the batch size in advance (need to be flexible due to variable number of child elements in collection entities.
    ///
    /// Supports nav properties, but only to entities with an identifier that's a single property (no composite keys) that has to be provided on input.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="equalityComparisonItems"></param>
    /// <param name="equalityProperties"></param>
    /// <param name="maxComparisons"></param>
    /// <returns></returns>
    /// <exception cref="InvalidOperationException"></exception>
    private static (Expression<Func<T, bool>> Expression, IReadOnlyCollection<T> UsedInputItems) GetPartialExpressionFor_MatchAnyOfItemsByValue<T>(
        IEnumerable<T> equalityComparisonItems,
        IEnumerable<PropertyInfo> equalityProperties,
        IEnumerable<(PropertyInfo NavProperty, PropertyInfo IdProperty)> navsWithIds,
        int maxComparisons)
        where T : class
    {
        var itemsToUseForComparison = equalityComparisonItems.Distinct().ToList();
        var propertiesToUseForComparison = equalityProperties.Distinct().ToList();
        var navPropertyMap = navsWithIds
            .Distinct()
            .ToDictionary(
                x => x.NavProperty, 
                x => x.IdProperty
                );

        if (!itemsToUseForComparison.Any())
        {
            // throw new ArgumentException("Can't do equality without equality items");
            // well, can.
            return ((x) => false, itemsToUseForComparison.ToList());
        }

        var collectionProperties = new List<PropertyInfo>();
        foreach (var pi in propertiesToUseForComparison)
        {
            var isGeneric = pi.PropertyType.IsGenericType;
            if (!isGeneric) continue;

            var isICollection = pi.PropertyType.GetGenericTypeDefinition() == typeof( ICollection<>);
            if (!isICollection) continue;

            var genericArgs = pi.PropertyType.GetGenericArguments();
            var argIsOk = genericArgs.Length == 1 && genericArgs.Single().IsClass;

            if (!argIsOk) throw new ArgumentException($"Something wrong with the type of property {pi}, generic args not matching expectations");
            collectionProperties.Add(pi);
        }
        
        // TODO: check all edge cases...
        var simpleNavProperties = navPropertyMap.Keys.Except(collectionProperties).ToList();

        // TODO: not everything that's not an ICollection is simple...
        var simpleProperties = propertiesToUseForComparison.Except(collectionProperties).Except(simpleNavProperties).ToList();

        Dictionary<PropertyInfo, Type> collectionInnerItemTypeMap = new();
        foreach (var propertyInfo in collectionProperties)
        {
            // obviously only simple generic props are supported (e.g. ICollection<SomeType>)
            var itemType = propertyInfo.PropertyType.GetGenericArguments()[0];
            collectionInnerItemTypeMap.Add(propertyInfo, itemType);
        }

        // Get the parameter expression for the query (e.g., x => x.Property)
        const string propertySymbol = "entity";
        var type = typeof(T);
        var parameter = Expression.Parameter(type, propertySymbol);

        // Start with a "false" expression (no matches)
        Expression predicate = Expression.Constant(false);

        var totalCount = 0;
        var cannotFitMoreItems = false;
        var processedItems = new List<T>();

        // Loop over each item in the batch
        foreach (var item in itemsToUseForComparison)
        {
            // Create an "AND" expression for comparing all properties of this item
            // true && item.MyCollection.Count == x && item.MyCollection.Contains(...)
            Expression itemPredicate = Expression.Constant(true);

            foreach (var property in collectionProperties)
            {
                var itemTypeInsideCollection = collectionInnerItemTypeMap[property];

                // property to get the id from the nav entity
                var idProperty = navPropertyMap[property];
                var idTypeInsideCollection = idProperty.PropertyType;
                
                var itemValuesCollection = (ICollection)property.GetValue(item);
                var count = itemValuesCollection.Count;

                // + N for each item and + 1 for the "count" comparison itself
                if (totalCount + count + 1 > maxComparisons)
                {
                    cannotFitMoreItems = true;
                    break;
                }
                totalCount += count;
                
                // Build the property expression for the query (e.g., entity.MyItems)
                var collectionPropertyExpression = Expression.Property(parameter, property);

                // confirm first the number of items is the same
                var countProperty = property.PropertyType.GetProperty(
                    "Count",
                    BindingFlags.Public | BindingFlags.Instance | BindingFlags.FlattenHierarchy);
                
                {
                    if (countProperty is null)
                        throw new InvalidOperationException("Target collection property must have a Count property");
                }

                // build equality, e.g. entity.MyItems.Count == 5
                var countEqualityExpression = Expression.Equal
                (
                    // entity.MyItems.Count
                    Expression.Property(collectionPropertyExpression, countProperty),
                    // 5
                    Expression.Constant(count)
                );

                // combine with prior equality expression
                // (...) && entity.MyItems.Count == 5
                itemPredicate = Expression.AndAlso(itemPredicate, countEqualityExpression);

                //  now check whether all of the tested items are contained in the collection
                // entity.MyCollectionProperty.All(element => testCollection.Contains(element))
                {
                    //TODO: hot paths for when item type is int or long, so I don't have to use object

                    // Get the value of the property from the item
                    var itemIds = new List<object>();
                    foreach (var valueInCollection in itemValuesCollection)
                    {
                        itemIds.Add(idProperty.GetValue(valueInCollection));
                    }

                    // [...ids...]
                    var itemIdsExpression = Expression.Constant(itemIds);

                    const string subParameterSymbol = "element";
                    // element, which will be used in: element => ...
                    var subParameterExpression = Expression.Parameter(itemTypeInsideCollection, subParameterSymbol);
                    
                    // element.Id
                    var innerIdExpression = Expression.Property(subParameterExpression, idProperty);
                    // (object)element.Id
                    var innerIdAsObjectExpression = Expression.Convert(innerIdExpression, typeof(object));

                    var containsMethod = typeof(ICollection<>).MakeGenericType(typeof(object))
                        .GetMethod("Contains", new[] { typeof(object) });

                    //var containsMethod = typeof(ICollection<>).MakeGenericType(idTypeInsideCollection)
                    //    .GetMethod("Contains", new[] { idTypeInsideCollection });
                    
                    // [myIds].Contains(element.Id)
                    var containsCallExpression =
                        Expression.Call(itemIdsExpression, containsMethod, innerIdAsObjectExpression);

                    // element => [myIds].Contains(element.Id)
                    var containsLambdaExpression = Expression.Lambda(containsCallExpression, subParameterExpression);

                    var allMethod = typeof(Enumerable)
                        .GetMethods(BindingFlags.Static | BindingFlags.Public)
                        .First(m => m.Name == nameof(Enumerable.All) && m.GetParameters().Length == 2)
                        .MakeGenericMethod(itemTypeInsideCollection);

                    // Construct the "entity.NavCollection.All(element => [myIds].Contains(element.Id))" expression
                    var allCallExpression = Expression.Call(allMethod, collectionPropertyExpression, containsLambdaExpression);

                    // (...) AndAlso entity.NavCollection.All(element => [myIds].Contains(element.Id))
                    itemPredicate = Expression.AndAlso(itemPredicate, allCallExpression);
                }
            }

            // for nav properties, do entity.Property.Id == item.Property.Id
            // actual relational DB handlers will translate entity.Property == item.Property to the correct expression automatically,
            // but LINQ-to-Enumerable will not, and will use .Equals instead, leading likely to ReferenceEquals, which is unsuitable
            foreach (var property in simpleNavProperties)
            {
                if (totalCount + 1 > maxComparisons)
                {
                    cannotFitMoreItems = true;
                    break;
                }
                totalCount++;

                // property to get the id from the nav entity
                var idProperty = navPropertyMap[property];

                // Get the value of the property from the item
                var idValue = Expression.Constant(idProperty.GetValue(property.GetValue(item)), idProperty.PropertyType);

                // Build the property expression for the database query (e.g., x.Property)
                var propertyExpression = Expression.Property(parameter, property);
                // x.Property.Id
                var propertyIdExpression = Expression.Property(propertyExpression, idProperty);

                // don't use operator since this is not overloaded everywhere, use Equals expression
                // ... actually, do use operator, because it goes to IQueryable
                Expression equalityExpression;
                var useOperatorForEquality = true;
                if (useOperatorForEquality)
                {
                    // Build the equality expression (e.g., x.Property == item.Property)
                    equalityExpression = Expression.Equal(propertyIdExpression, idValue);
                }
                else // use Equals method
                {
                    var equalsMethod = type.GetMethod(
                        "Equals",
                        BindingFlags.Public | BindingFlags.Instance | BindingFlags.FlattenHierarchy,
                        null,
                        new[] { property.PropertyType },
                        null);

                    equalityExpression = Expression.Call(propertyExpression, equalsMethod, idValue);
                }

                // Combine the equality expression with the itemPredicate using "AND"
                itemPredicate = Expression.AndAlso(itemPredicate, equalityExpression);
            }

            // each simple property is just entity.Property == item.Property
            foreach (var property in simpleProperties)
            {
                if (totalCount + 1 > maxComparisons)
                {
                    cannotFitMoreItems = true;
                    break;
                }
                totalCount++;

                // Get the value of the property from the item
                var itemValue = Expression.Constant(property.GetValue(item), property.PropertyType);

                // Build the property expression for the database query (e.g., x.Property)
                var propertyExpression = Expression.Property(parameter, property);

                // don't use operator since this is not overloaded everywhere, use Equals expression
                // ... actually, do use operator, because it goes to IQueryable
                Expression equalityExpression;
                var useOperatorForEquality = true;
                if (useOperatorForEquality)
                {
                    // Build the equality expression (e.g., x.Property == item.Property)
                    equalityExpression = Expression.Equal(propertyExpression, itemValue);
                }
                else // use Equals method
                {
                    var equalsMethod = type.GetMethod(
                        "Equals",
                        BindingFlags.Public | BindingFlags.Instance | BindingFlags.FlattenHierarchy,
                        null,
                        new[] { property.PropertyType },
                        null);

                    equalityExpression = Expression.Call(propertyExpression, equalsMethod, itemValue);
                }

                // Combine the equality expression with the itemPredicate using "AND"
                itemPredicate = Expression.AndAlso(itemPredicate, equalityExpression);
            }

            if (cannotFitMoreItems) break;

            processedItems.Add(item);

            // Combine the itemPredicate with the main predicate using "OR"
            predicate = Expression.OrElse(predicate, itemPredicate);
        }

        // Build the final lambda expression (e.g., x => (x.Property1 == item1.Property1 && ...) || (x.Property1 == item2.Property1 && ...))
        var typedExpression = Expression.Lambda<Func<T, bool>>(predicate, parameter);
        return (typedExpression, processedItems);
    }

    private static (Expression<Func<TEntity, bool>> Expression, IReadOnlyCollection<TValue> UsedValues) GetPartialExpressionFor_MatchAnyValue<TEntity, TValue>(
            IEnumerable<TValue> values,
            PropertyInfo matchProperty,
            int maxComparisons)
            where TEntity : class
            where TValue : IEquatable<TValue>
    {
        if (matchProperty.PropertyType != typeof(TValue))
            throw new ArgumentException(
                $"Type mismatch between the provided property {matchProperty.Name} ({matchProperty.PropertyType}) and the public references ({typeof(TValue)})");

        var valuesForComparison = values.Distinct().ToList();

        if (!valuesForComparison.Any() || maxComparisons <= 0)
        {
            // throw new ArgumentException("Can't do equality without equality items");
            // well, can.
            return ((x) => false, valuesForComparison.ToList());
        }

        var usedValues = valuesForComparison.Take(maxComparisons).ToList();

        // Parameter expression representing each entity in the DbSet (e.g., x => x.Property)
        var parameter = Expression.Parameter(typeof(TEntity), "entity");

        // Property access expression (e.g., x.Property)
        var propertyAccess = Expression.Property(parameter, matchProperty);

        // Create a constant expression for the list of values
        var constant = Expression.Constant(usedValues);

        // Get the Contains method for List<T>
        var containsMethod = typeof(List<TValue>).GetMethod("Contains", new[] { typeof(TValue) })!;

        // Create the Contains expression: values.Contains(x.Property)
        var containsExpression = Expression.Call(constant, containsMethod, propertyAccess);

        // Build the final lambda expression: x => values.Contains(x.Property)
        var lambda = Expression.Lambda<Func<TEntity, bool>>(containsExpression, parameter);
        return (lambda, usedValues);
    }

    /// <summary>
    /// Get expression giving an inverted result from the given boolean expression
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="original"></param>
    /// <returns></returns>
    private static Expression<Func<T, bool>> InvertBooleanExpression<T>(Expression<Func<T, bool>> original)
    {
        var negated = Expression.Not(original.Body);
        return Expression.Lambda<Func<T, bool>>(negated, original.Parameters);
    }
    /// <summary>
    /// Check if the propvided property of the provided entity type is a navigation property
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="db"></param>
    /// <param name="propertyInfo"></param>
    /// <returns></returns>
    /// <exception cref="ArgumentException"></exception>
    private static bool PropertyIsNavigation<T>(DbContext db, PropertyInfo propertyInfo) where T: class
    {
        var entityType = typeof(T);

        if (!DatabaseContainsType(db, entityType)) 
            throw new ArgumentException($"Provided DbContext of type {db.GetType()} does not contain an entity of type {entityType}");

        var entTypeInModel = db.Model.FindEntityType(entityType)!;

        // TODO: do I need to check SkipNavigation properties for M2M?
        var directNav = entTypeInModel.FindNavigation(propertyInfo);
        var skipNav = entTypeInModel.FindSkipNavigation(propertyInfo);
        var isNavigation = directNav is not null || skipNav is not null;
        return isNavigation;
    }

    /// <summary>
    /// For the given input type T and navigational property, finds the PropertyInfo for the entity mapped by that navigation property.
    /// Maps via PrimaryKey, which must map to a single property.
    /// 
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="db"></param>
    /// <param name="propertyInfo"></param>
    /// <returns></returns>
    /// <exception cref="ArgumentException"></exception>
    /// <exception cref="NotSupportedException"></exception>
    private static PropertyInfo GetIdPropertyOfNavigationTarget<T>(DbContext db, PropertyInfo propertyInfo)
        where T : class
    {
        var entityType = typeof(T);

        if (!PropertyIsNavigation<T>(db, propertyInfo))
            throw new ArgumentException($"The provided property info ({propertyInfo.DeclaringType?.Name}:{propertyInfo.Name}) must link to a navigational property of provided type ({entityType})");

        var propType = propertyInfo.PropertyType;
        
        var isCollection = propType.IsGenericType 
                           && (false 
                               || propType.GetGenericTypeDefinition() == typeof(ICollection<>)
                               || propType.GetGenericTypeDefinition() == typeof(IEnumerable<>));

        Type GetSingleGenericArgument(Type type)
        {
            var args = type.GetGenericArguments();
            if (args.Length == 1) return args.Single();
            throw new ArgumentException(
                $"Provided type {type} has multiple generic arguments, but only one is allowed");
        } 

        var navEntityClrType = isCollection
            ? GetSingleGenericArgument(propType) 
            : propType;

        var navEntityType = db.Model.FindEntityType(navEntityClrType)!;

        var primaryKeyForNavType = navEntityType.FindPrimaryKey();

        if (primaryKeyForNavType == null)
            throw new ArgumentException(
                $"Navigation target is of type {navEntityType.ClrType} but does not have a primary key");

        if (primaryKeyForNavType!.Properties.Count != 1)
            throw new NotSupportedException(
                $"Navigation target is of type {navEntityType.ClrType} and has a composite primary key. Only single-property keys are supported.");

        var keyProperty = primaryKeyForNavType.Properties.Single();

        var res = keyProperty.PropertyInfo;

        if (res is null)
            throw new NotSupportedException(
                $"Navigation target is of type {navEntityType.ClrType} and has a key {keyProperty.Name}, but it does not seems to be attached to a Clr Property. Shadow/implicit keys are not supported.");

        return res!;
    }

}