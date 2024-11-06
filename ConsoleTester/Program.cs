using System.Linq.Expressions;
using System.Runtime.CompilerServices;
using Microsoft.EntityFrameworkCore;

namespace ConsoleTester;

public class TestContext : PostgresContext<MyDb>
{
    public TestContext()
    {
        this.InitializeAsync().GetAwaiter().GetResult();
    }

    public async Task Test()
    {
        var se = Enumerable.Range(0, 10)
            .ToDictionary(
                x => x,
                x => new SimpleValueEntity((long)x)
            );

        var ce = new List<SimpleCollectionEntity>()
        {
            new([se[1], se[2]]),
            new([se[1], se[3]]),
            new([se[2], se[3]]),
            new([se[4]]),
            new([se[5]]),
        }.Select((x,i)=>(x,i))
        .ToDictionary(x=>x.i, x=>x.x);


        await _db.AddRangeAsync(ce.Values.ToList());
        await _db.SaveChangesAsync();

        var colEnt = new SimpleCollectionEntity([se[1], se[2]]);

        var colEnts = new[]
        {
            new SimpleCollectionEntity([se[1], se[2]]),
            new SimpleCollectionEntity([se[2], se[3]])
        };

        var searchEnts = new[]
        {
            se[1], se[2]
        };

        var searchIds = searchEnts.Select(x => x.Id).ToList();

        var expressions = new List<Expression<Func<SimpleCollectionEntity, bool>>>()
        {
            x => x.Collection.Count == 1,

            x=> x.Collection.Contains(se[1]),

            x=>x.Collection.All(e=>
                searchEnts.Contains(e)
                ),

            x=>x.Collection.All(e=>
                searchIds.Contains(e.Id)
            ),

            x=>this.fail.Any(),

            x=>AmIGood(x)
        };
        
        var counter = 0;
        foreach (var expression in expressions)
        {
            counter++;
            var res = await TryRun(expression);
            Console.WriteLine($"{counter}: {res.Value} :: {expression}\n\n{res.Text}");
        }

        await TryRun<SimpleCollectionEntity>(x => x.Collection.Count == 1);



    }

    private bool AmIGood<Τ>(Τ input) => false;

    private List<Expression> success = [];
    private List<Expression> fail = [];

    public async Task<(bool Value, string? Text)> TryRun<T>(Expression<Func<T, bool>> expression) where T: class
    {
        try
        {
            var query = _db.Set<T>().Where(expression);
            
            await query.ToListAsync();
            success.Add(expression);
            return (true, null);
        }
        catch (Exception ex)
        {
            fail.Add(expression);
            return (false, ex.Message);
        }
    }

}

internal class Program
{
    static async Task Main(string[] args)
    {
        Console.WriteLine("Hello, World!");
        
        var x = new TestContext();
        await x.Test();
    }
   
}

public class MyDb(DbContextOptions options) : DbContext(options)
{
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);
        modelBuilder.Entity<SimpleCollectionEntity>()
            .HasMany(x => x.Collection)
            .WithMany();

        modelBuilder.Entity<SimpleNavEntity>()
            .HasOne(x => x.NavValue)
            .WithMany();
    }
}


public class SimpleNavEntity : EntityBase
{
    private SimpleNavEntity() { }
    public SimpleNavEntity(SimpleValueEntity value) { NavValue = value; }
    
    public SimpleValueEntity NavValue { get; private set; }
}

public class SimpleCollectionEntity : EntityBase
{
    private SimpleCollectionEntity() { }
    public SimpleCollectionEntity(IEnumerable<SimpleValueEntity> values) { Collection = values.Distinct().ToList(); }

    public ICollection<SimpleValueEntity> Collection { get; private set; } = [];
}

public class SimpleValueEntity : EntityBase
{
    private SimpleValueEntity() { }
    public SimpleValueEntity(long value) { Value = value; }

    public long Value { get; private set; }
}
