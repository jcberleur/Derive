using System;

[Flags]
public enum Impl
{
    //None = 0,

    Equatable = 1 << 0,
    IStructuralEquatable = 1 << 1,

    Comparable = 1 << 2,
    IStructuralComparable = 1 << 3,

    /// <summary>
    /// ToString
    /// </summary>
    Debug = 1 << 4,

    Deconstruct = 1 << 5,
}
