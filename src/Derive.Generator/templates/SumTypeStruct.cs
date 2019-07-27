
partial struct SumType
{
    partial struct A { }

    public enum Discriminant
    {
        A = 1
    }

    public Discriminant Case { get; }
    private Union union;

    public SumType(in A @a) { union = new Union { Type1 = @a }; Case = Discriminant.A; }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public T Match<T>(FuncA<T> a)
    {
        switch (Case)
        {
            case Discriminant.A: return a(union.Type1);
            default:
                throw new System.ArgumentOutOfRangeException(nameof(Case), Case, $@"Uninitialized {GetType()}");
        }
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public void Switch(ActionA a)
    {
        switch (Case)
        {
            case Discriminant.A: a(union.Type1); return;
            default:
                throw new System.ArgumentOutOfRangeException(nameof(Case), Case, $@"Uninitialized {GetType()}");
        }
    }

    [System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)]
    struct Union
    {
        [System.Runtime.InteropServices.FieldOffset(0)] public A Type1;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static implicit operator SumType(in A value) => new SumType(a: value);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static SumType CreateA(in A value) => new SumType(a: value);

    public delegate void ActionA(in A @a);
    public delegate T FuncA<T>(in A @a);
}