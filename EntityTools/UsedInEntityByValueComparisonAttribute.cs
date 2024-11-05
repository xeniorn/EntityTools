namespace EntityTools;

/// <summary>
/// Marks properties in entities that are value-defining for that entity, i.e. they should be considered equal
/// and persisted under the same reference and same ID, and there should never be two persisted copies with all values matching.
///
/// Works both on normal and Navigation properties, including ICollection properties.
/// </summary>
[AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = true)]
public class UsedInEntityByValueComparisonAttribute : Attribute
{
}