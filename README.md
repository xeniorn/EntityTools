![Status](https://github.com/xeniorn/EntityTools/actions/workflows/dotnet.yml/badge.svg?branch=develop)
![NuGet Version](https://img.shields.io/nuget/v/EntityTools)


Helps handling Entities, in particular matching by value instead of by ref/id.

With a class like:

```cs
public abstract class EntityBase
{
    [Key]
    [DatabaseGenerated(DatabaseGeneratedOption.Identity)]
    public int Id { get; set; }
}

public class SimpleValueEntity : EntityBase
{
    private SimpleValueEntity() {}
    public SimpleValueEntity(long value) { Value = value; }
    
    [UsedInEntityByValueComparison]
    public long Value { get; private set; }
}
```

Can do stuff like:

```cs
DbContext myDb = GetDb();
IEnumerable<SimpleValueEntity> input = GetDummySearchEntitiesWithValues([1, 5, 23]);

var (existing, missing) = await EntityHelper.GetExistingAndMissingEntitiesByValue(
    db: myDb,
    targets: input,
    maxComparisonsPerBatch: int.MaxValue);
```
