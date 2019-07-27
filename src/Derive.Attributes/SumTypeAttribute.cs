using System;
using System.Diagnostics;
using CodeGeneration.Roslyn;

[AttributeUsage(AttributeTargets.Struct | AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
[CodeGenerationAttribute("Derive.Generator.SumTypeGenerator, Derive.Generator")]
[Conditional("CodeGeneration")]
public class SumTypeAttribute : Attribute
{
    /// <summary>
    /// Name of the property to use as a discriminant enum.
    /// </summary>
    public string DiscriminantName { get; set; }

    /// <summary>
    /// Enable generatation of a Newtonsoft.Json.JsonConverter for this type.
    /// </summary>
    public bool JsonConverter { get; set; }
}
