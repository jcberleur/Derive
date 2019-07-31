using System;
using System.Diagnostics;
using CodeGeneration.Roslyn;

[AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
[CodeGenerationAttribute("Derive.Generator.SumTypeGenerator, Derive.Generator")]
[Conditional("CodeGeneration")]
public class SumTypeAttribute : Attribute
{
    /// <summary>
    /// Name of the property to use as a discriminant enum.
    /// </summary>
    public string DiscriminantName { get; set; }

    /// <summary>
    /// Options to customize the code generation.
    /// </summary>
    public SumTypeOptions Options { get; set; }
}

/// <summary>
/// Possible Options to customize the code generation.
/// </summary>
[Flags]
public enum SumTypeOptions
{
    None = 0,

    /// <summary>
    /// Enable generatation of a private constructor to disable non-nested extends.
    /// </summary>
    DisableConstructor = 1 << 0,

    /// <summary>
    /// Enable generatation of a Newtonsoft.Json.JsonConverter for this type.
    /// </summary>
    EnableJsonConverter = 1 << 1,
}

