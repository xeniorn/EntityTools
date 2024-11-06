namespace EntityTools.Test.Base;

public static class EnumerableExtensions
{
    public static int[] Ids<T>(this IEnumerable<T> input) where T : EntityBase => input.Select(x => x.Id).ToArray();
    public static int[] OrderedIds<T>(this IEnumerable<T> input) where T : EntityBase => input.Select(x => x.Id).Order().ToArray();
}