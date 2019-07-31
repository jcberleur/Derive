partial class StructuralEq : System.Collections.IStructuralEquatable
{
    public int Item1 { get; set; }
    public int Item2 { get; set; }

    bool System.Collections.IStructuralEquatable.Equals(object other, System.Collections.IEqualityComparer comparer)
    {
        if (other == null || !(other is StructuralEq objOther)) return false;
        return comparer.Equals(Item1, objOther.Item1)
            && comparer.Equals(Item2, objOther.Item2);
    }

    int System.Collections.IStructuralEquatable.GetHashCode(System.Collections.IEqualityComparer comparer)
    {
        System.Collections.IStructuralEquatable tuple = (Item1, Item2);
        return tuple.GetHashCode(comparer);
        return comparer.GetHashCode(Item1);
    }
}