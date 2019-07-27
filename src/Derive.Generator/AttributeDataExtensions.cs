using System.Linq;
using Microsoft.CodeAnalysis;

namespace Derive.Generator
{
    internal static class AttributeDataExtensions
    {
        public static object GetNamedArgumentValue(this AttributeData attributeData, string propertyName)
        {
            var constant = attributeData.NamedArguments.FirstOrDefault(pair => pair.Key == propertyName).Value;
            return constant.Value;
        }
    }
}
