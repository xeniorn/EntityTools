using EntityTools.Test.Base;
using Microsoft.EntityFrameworkCore;

namespace EntityTools.Test;

public class Tests_SimpleNavEntity : PostgresBasedTest<Tests_SimpleNavEntity.Db_SimpleNavEntity>
{
    public class Db_SimpleNavEntity(DbContextOptions options) : DbContext(options)
    {
        public class SimpleNavEntity : EntityBase
        {
            private SimpleNavEntity() { }
            public SimpleNavEntity(SimpleValueEntity value) { NavValue = value; }

            [UsedInEntityByValueComparison]
            public SimpleValueEntity NavValue { get; private set; }
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
            modelBuilder.Entity<SimpleNavEntity>()
                .HasOne(x=>x.NavValue)
                .WithMany();
        }
    }


    [Fact]
    public async Task CorrectlyFindsExistingAndMissing()
    {
        var v1 = new Db_SimpleNavEntity.SimpleValueEntity(1);
        var v2 = new Db_SimpleNavEntity.SimpleValueEntity(2);
        var v3 = new Db_SimpleNavEntity.SimpleValueEntity(3);

        var nv1 = new Db_SimpleNavEntity.SimpleNavEntity(v1);
        var nv2 = new Db_SimpleNavEntity.SimpleNavEntity(v2);
        var nv3 = new Db_SimpleNavEntity.SimpleNavEntity(v3);

        var exist = new[] {nv1 };
        var miss = new[] { nv2, nv3 };

        var search = new[] { nv1, nv2, nv3 };

        await _db.AddRangeAsync(exist);
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