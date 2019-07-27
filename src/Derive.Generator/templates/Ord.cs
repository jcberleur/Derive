partial class Ord : System.IComparable, System.IComparable<Ord>
{
    public string Field;
    public string Prop { get; }

    public int CompareTo(Ord other) => Compare(this, other);

    public int CompareTo(object obj)
    {
        if (obj == null) return 1;
        if (!(obj is Ord other))
        {
            throw new System.ArgumentException($"Incorrect Type, expected {GetType()}, received {obj.GetType()}", nameof(obj));
        }
        return Compare(this, other);
    }

    public static int Compare(Ord left, Ord right)
    {
        if (left is null)
        {
            return right is null ? 0 : 1;
        }
        var c = System.Collections.Generic.Comparer<string>.Default.Compare(left.Field, right.Field);
        if (c != 0) return c;
        c = System.Collections.Generic.Comparer<string>.Default.Compare(left.Field, right.Field);
        if (c != 0) return c;
        return System.Collections.Generic.Comparer<string>.Default.Compare(left.Prop, right.Prop);
    }

    public static bool operator <(Ord left, Ord right) => Compare(left, right) < 0;
    public static bool operator >(Ord left, Ord right) => Compare(left, right) > 0;
    public static bool operator <=(Ord left, Ord right) => Compare(left, right) <= 0;
    public static bool operator >=(Ord left, Ord right) => Compare(left, right) >= 0;
}