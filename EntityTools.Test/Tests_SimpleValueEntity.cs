using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using EntityTools.Test.Base;
using Microsoft.EntityFrameworkCore;

namespace EntityTools.Test;

public class Tests_SimpleValueEntity : PostgresBasedTest<Tests_SimpleValueEntity.Db_SimpleValueEntity>
{
    public class Db_SimpleValueEntity(DbContextOptions options) : DbContext(options)
    {
        public class SimpleValueEntity : EntityBase
        {
            private SimpleValueEntity() {}
            public SimpleValueEntity(long value) { Value = value; }

            [UsedInEntityByValueComparison]
            public long Value { get; private set; }
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);
            modelBuilder.Entity<SimpleValueEntity>();
        }
    }


    [Fact]
    public async Task CorrectlyFindsExistingAndMissing()
    {
        var input = Enumerable.Range(1, 3)
            .Select(x => new Db_SimpleValueEntity.SimpleValueEntity(x))
            .ToList();

        var exist = input.Take(1).ToList();
        var miss = input.Except(exist).ToList();

        await _db.AddRangeAsync(exist);
        await _db.SaveChangesAsync();

        ResetState();
        var res = await EntityHelper.GetExistingAndMissingEntitiesByValue(
            _db,
            input,
            int.MaxValue);

        Assert.Equal(exist.OrderedIds(), res.Existing.OrderedIds());
        Assert.Equal(miss, res.Missing);
    }
}