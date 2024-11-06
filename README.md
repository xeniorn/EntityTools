![Status](https://github.com/xeniorn/EntityHelper/actions/workflows/dotnet.yml/badge.svg?branch=develop)

Helps handling Entities, in particular matching by value instead of by ref/id.

With a class like:

'''C#
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
'''

Can do stuff like:

'''C#
DbContext myDb = GetDb();
var (existing, missing) = await EntityHelper.GetExistingAndMissingEntitiesByValue(
    db: myDb,
    targets: input,
    maxComparisonsPerBatch: int.MaxValue);
'''