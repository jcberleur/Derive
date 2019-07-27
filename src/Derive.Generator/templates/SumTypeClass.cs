//[SumTypeConverter]
abstract partial class SumTypeClass
{
    private SumTypeClass() { }

    public enum Discriminant
    {
        A
    }

    public abstract Discriminant Case { get; }

    public T Match<T>(System.Func<A, T> a)
    {
        switch (this)
        {
            case A self: return a(self);
            default:
                throw new System.ArgumentOutOfRangeException(nameof(Case), this, @"Unreachable");
        }
    }

    public void Switch(System.Action<A> a)
    {
        switch (this)
        {
            case A self: a(self); return;
            default:
                throw new System.ArgumentOutOfRangeException(nameof(Case), this, @"Unreachable");
        }
    }

    public sealed partial class A : SumTypeClass
    {
        public override Discriminant Case => Discriminant.A;
    }
}