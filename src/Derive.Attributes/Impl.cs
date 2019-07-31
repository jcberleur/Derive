using System;

[Flags]
public enum Impl
{
    //None = 0,

    Equatable = 1 << 0,
    IStructuralEquatable = 1 << 1,

    Comparable = 1 << 2,
    IStructuralComparable = 1 << 3,

    Deconstruct = 1 << 4,
}
