using System;

[Flags]
public enum Interfaces
{
    //None = 0,
    Equatable = 1 << 0,
    Comparable = 1 << 1,
}
