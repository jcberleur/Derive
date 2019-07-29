using System;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using CodeGeneration.Roslyn;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Derive.Generator
{
    public class SumTypeGenerator : IRichCodeGenerator
    {
        private readonly string _discriminantName;
        private readonly SumTypeOptions _options;

        public SumTypeGenerator(AttributeData attributeData)
        {
            if (attributeData == null) throw new ArgumentNullException(nameof(attributeData));
            _discriminantName = (string)attributeData.GetNamedArgumentValue(nameof(SumTypeAttribute.DiscriminantName)) ?? "Case";
            var optionsValue = (int)attributeData.GetNamedArgumentValue(nameof(SumTypeAttribute.Options));
            _options = (SumTypeOptions)optionsValue;
        }

        public Task<SyntaxList<MemberDeclarationSyntax>> GenerateAsync(TransformationContext context, IProgress<Diagnostic> progress, CancellationToken cancellationToken)
        {
            throw new NotImplementedException();
        }

        public Task<RichGenerationResult> GenerateRichAsync(TransformationContext context, IProgress<Diagnostic> progress, CancellationToken cancellationToken)
        {
            var applyToType = (TypeDeclarationSyntax)context.ProcessingNode;


            SyntaxList<MemberDeclarationSyntax> derive;
            switch (applyToType)
            {
                case ClassDeclarationSyntax classDeclaration:
                    derive = SumTypeClassGenerator.CreateSyntax(classDeclaration, _discriminantName, _options);
                    break;
                case StructDeclarationSyntax structDeclaration:
                    derive = SumTypeStructGenerator.CreateSyntax(structDeclaration, _discriminantName);
                    break;
                default:
                    throw new NotSupportedException();
            }

            // Figure out ancestry for the generated type, including nesting types and namespaces.
            var wrappedMembers = SyntaxFactory.List(derive.WrapWithAncestors(context.ProcessingNode).Select(n => n.NormalizeWhitespace()));

            return Task.FromResult(new RichGenerationResult
            {
                Members = wrappedMembers,
            });
        }
    }
}
