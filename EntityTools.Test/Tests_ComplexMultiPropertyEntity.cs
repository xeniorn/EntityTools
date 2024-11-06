using EntityTools.Test.Base;
using Microsoft.EntityFrameworkCore;

namespace EntityTools.Test;

/// <summary>
/// Includes both a collection, navigation, and a simple property, all value-defining.
/// Includes additionally three interfering non-value properties of equivalent types.
/// </summary>
public class Tests_ComplexMultiPropertyEntity : PostgresBasedTest<Tests_ComplexMultiPropertyEntity.Db_ComplexMultiPropertyEntity>
{
    public class Db_ComplexMultiPropertyEntity(DbContextOptions options) : DbContext(options)
    {
        public class ComplexMultiPropertyEntity : EntityBase
        {
            private ComplexMultiPropertyEntity() { }

            public ComplexMultiPropertyEntity(
                IEnumerable<SimpleValueEntity> collectionValues,
                SimpleValueEntity navValue, 
                long simpleValue,
                IEnumerable<NonValueSimpleValueEntity> nonValueCollectionValues,
                NonValueSimpleValueEntity nonValueNavValue,
                long nonValueSimpleValue)
            {
                Collection = collectionValues.Distinct().ToList();
                NavValue = navValue;
                SimpleValue = simpleValue;

                NonValueCollection = nonValueCollectionValues.Distinct().ToList();
                NonValueNavValue = nonValueNavValue;
                NonValueSimpleValue = nonValueSimpleValue;
            }

            [UsedInEntityByValueComparison]
            public ICollection<SimpleValueEntity> Collection { get; private set; } = [];

            [UsedInEntityByValueComparison]
            public SimpleValueEntity NavValue { get; private set; }

            [UsedInEntityByValueComparison]
            public long SimpleValue { get; private set; }
            
            public ICollection<NonValueSimpleValueEntity> NonValueCollection { get; private set; } = [];
            public NonValueSimpleValueEntity NonValueNavValue { get; private set; }
            public long NonValueSimpleValue { get; private set; }
        }

        public class NonValueSimpleValueEntity : EntityBase
        {
            private NonValueSimpleValueEntity() { }
            public NonValueSimpleValueEntity(long value) { Value = value; }

            [UsedInEntityByValueComparison]
            public long Value { get; private set; }
        }

        public class SimpleValueEntity : EntityBase
        {
            private SimpleValueEntity() { }
            public SimpleValueEntity(long value) { Value = value; }

            [UsedInEntityByValueComparison]
            public long Value { get; private set; }
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);
            modelBuilder.Entity<ComplexMultiPropertyEntity>()
                .HasMany(x => x.Collection)
                .WithMany();

            modelBuilder.Entity<ComplexMultiPropertyEntity>()
                .HasOne(x => x.NavValue)
                .WithMany();

            modelBuilder.Entity<ComplexMultiPropertyEntity>()
                .HasMany(x => x.NonValueCollection)
                .WithMany();

            modelBuilder.Entity<ComplexMultiPropertyEntity>()
                .HasOne(x => x.NonValueNavValue)
                .WithMany();
        }
    }


    [Fact]
    public async Task CorrectlyFindsExistingAndMissing()
    {
        var sv = Enumerable.Range(1000, 1015)
            .Select((x, i) => (i, new Db_ComplexMultiPropertyEntity.SimpleValueEntity(x)))
            .ToDictionary(x => x.i, x => x.Item2);

        var nvsv = Enumerable.Range(2000, 2015)
            .Select((x, i) => (i, new Db_ComplexMultiPropertyEntity.NonValueSimpleValueEntity(x)))
            .ToDictionary(x => x.i, x => x.Item2);

        var cv = Enumerable.Range(100, 110)
            .Select((x, i) => 
                (i, 
                    new Db_ComplexMultiPropertyEntity.ComplexMultiPropertyEntity(
                        [sv[i], sv[i+1]],
                        sv[i],
                        x,
                        [nvsv[i], nvsv[i+2]],
                        nvsv[i+2],
                        x+13
                        )))
            .ToDictionary(x => x.i, x => x.Item2);

        var search = new List<Db_ComplexMultiPropertyEntity.ComplexMultiPropertyEntity>()
        {
            cv[1], cv[2],
            cv[8], cv[9]
        };

        var inDb = new List<Db_ComplexMultiPropertyEntity.ComplexMultiPropertyEntity>()
        {
            cv[1], cv[2], cv[3], cv[4], cv[5]
        };

        var exist = new List<Db_ComplexMultiPropertyEntity.ComplexMultiPropertyEntity>()
        {
            cv[1], cv[2]
        };

        var miss = new List<Db_ComplexMultiPropertyEntity.ComplexMultiPropertyEntity>()
        {
            cv[8], cv[9]
        };
        
        await _db.AddRangeAsync(inDb);
        await _db.SaveChangesAsync();

        ResetState();
        var res = await EntityHelper.GetExistingAndMissingEntitiesByValue(
            _db,
            search,
            int.MaxValue);

        Assert.Equal(exist.OrderedIds(), res.Existing.OrderedIds());

        Assert.Equal(miss, res.Missing);
    }
}