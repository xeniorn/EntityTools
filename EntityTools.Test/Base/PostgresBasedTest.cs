using DotNet.Testcontainers.Builders;
using Microsoft.EntityFrameworkCore;
using Testcontainers.PostgreSql;

namespace EntityTools.Test.Base;

public abstract class PostgresBasedTest<TDatabase> : IAsyncLifetime where TDatabase : DbContext
{
    private readonly PostgreSqlContainer _postgresContainer;
    protected TDatabase _db;
    private DbContextOptions<TDatabase> _options;

    protected virtual void ResetState() => ResetDbContext();

    protected void ResetDbContext()
    {
        //Task.Run(_db.Dispose);
        _db = CreateNewDbContext(_options);
    } 

    private TDatabase CreateNewDbContext(DbContextOptions<TDatabase> options)
    {
        return (TDatabase)(Activator.CreateInstance(typeof(TDatabase), [_options]) ?? throw new NullReferenceException());
    }


    protected PostgresBasedTest(string containerImage = "postgres:latest", ushort port = 5432)
    {
        // Create a new instance of a container.
        _postgresContainer = new PostgreSqlBuilder()
            // Set the image for the container to "testcontainers/helloworld:1.1.0".
            .WithImage(containerImage)
            // Bind port 8080 of the container to a random port on the host.
            .WithPortBinding(port, true)
            // Wait until the HTTP endpoint of the container is available.
            .WithWaitStrategy(Wait.ForUnixContainer().UntilPortIsAvailable(port))
            //.WithWaitStrategy(Wait.ForUnixContainer().UntilHttpRequestIsSucceeded(r => r.ForPort(port)))
            // Cleanup
            .WithCleanUp(true)
            // Build the container configuration.
            .Build();
    }

    public virtual async Task InitializeAsync()
    {
        await _postgresContainer.StartAsync();

        _options = new DbContextOptionsBuilder<TDatabase>()
            .UseNpgsql(connectionString: _postgresContainer.GetConnectionString())
            .Options;

        ResetDbContext();

        // Optional: Apply migrations or seed data
        await _db.Database.EnsureCreatedAsync();
    }

    public virtual async Task DisposeAsync()
    {
        await _postgresContainer.StopAsync();
        await _postgresContainer.DisposeAsync();
    }
}