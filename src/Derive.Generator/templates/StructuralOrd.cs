partial class StructuralOrd : System.Collections.IStructuralComparable
{
    public int Item1 { get; set; }
    public int Item2 { get; set; }

    int IStructuralComparable.CompareTo(object other, System.Collections.IComparer comparer)
    {
        if (other == null) return 1;

        if (!(other is StructuralOrd objOther))
        {
            throw new System.ArgumentException($"Argument must be of type {GetType()}", nameof(other));
        }

        int c = comparer.Compare(Item1, objOther.Item1);
        if (c != 0) return c;
        return comparer.Compare(Item2, objOther.Item2);
    }
}