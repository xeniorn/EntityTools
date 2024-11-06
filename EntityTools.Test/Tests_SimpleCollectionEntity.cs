using EntityTools.Test.Base;
using Microsoft.EntityFrameworkCore;

namespace EntityTools.Test;

public class Tests_SimpleCollectionEntity : PostgresBasedTest<Tests_SimpleCollectionEntity.Db_SimpleCollectionEntity>
{
    public class Db_SimpleCollectionEntity(DbContextOptions options) : DbContext(options)
    {
        public class SimpleCollectionEntity : EntityBase
        {
            private SimpleCollectionEntity() { }
            public SimpleCollectionEntity(IEnumerable<SimpleValueEntity> values) { Collection = values.Distinct().ToList(); }

            [UsedInEntityByValueComparison] 
            public ICollection<SimpleValueEntity> Collection { get; private set; } = [];
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
            modelBuilder.Entity<SimpleCollectionEntity>()
                .HasMany(x => x.Collection)
                .WithMany();
        }
    }


    [Fact]
    public async Task CorrectlyFindsExistingAndMissing()
    {
        var v1 = new Db_SimpleCollectionEntity.SimpleValueEntity(11);
        var v2 = new Db_SimpleCollectionEntity.SimpleValueEntity(22);
        var v3 = new Db_SimpleCollectionEntity.SimpleValueEntity(33);
        
        var nv1 = new Db_SimpleCollectionEntity.SimpleCollectionEntity([v1,v2]);
        var nv2 = new Db_SimpleCollectionEntity.SimpleCollectionEntity([v1,v2,v3]);
        var nv3 = new Db_SimpleCollectionEntity.SimpleCollectionEntity([v1]);
        var nv4 = new Db_SimpleCollectionEntity.SimpleCollectionEntity([v2,v3]);
        var nv5 = new Db_SimpleCollectionEntity.SimpleCollectionEntity([v3]);

        var inDb = new[] { nv1, nv3, nv4 };

        var exist = new[] { nv1, nv3 };
        var miss = new[] { nv2, nv5 };

        var search = new[] { nv1, nv2, nv3, nv5 };

        var trashInDb = Enumerable.Range(1,50).Select(x => new Db_SimpleCollectionEntity.SimpleCollectionEntity([])).ToList();

        await _db.AddRangeAsync(trashInDb);
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