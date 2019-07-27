using System;
using System.Diagnostics;
using CodeGeneration.Roslyn;

[AttributeUsage(AttributeTargets.Struct | AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
[CodeGenerationAttribute("Derive.Generator.DeriveGenerator, Derive.Generator")]
[Conditional("CodeGeneration")]
public class DeriveAttribute : Attribute
{
    public DeriveAttribute(Impl which)
    {
        Interfaces = which;
    }

    public Impl Interfaces { get; }
}
