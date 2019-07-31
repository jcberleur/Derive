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
            _options = (SumTypeOptions) ((int?)attributeData.GetNamedArgumentValue(nameof(SumTypeAttribute.Options)) ?? 0);
        }

        public Task<SyntaxList<MemberDeclarationSyntax>> GenerateAsync(TransformationContext context, IProgress<Diagnostic> progress, CancellationToken cancellationToken)
        {
            throw new NotImplementedException();
        }

        public Task<RichGenerationResult> GenerateRichAsync(TransformationContext context, IProgress<Diagnostic> progress, CancellationToken cancellationToken)
        {
            SyntaxList<MemberDeclarationSyntax> derive;
            switch (context.ProcessingNode)
            {
                case ClassDeclarationSyntax classDeclaration:
                    derive = SumTypeClassGenerator.CreateSyntax(context, _discriminantName, _options);
                    break;
                case StructDeclarationSyntax structDeclaration:
                    derive = SumTypeStructGenerator.CreateSyntax(context, _discriminantName);
                    break;
                default:
                    throw new NotSupportedException();
            }

            // Figure out ancestry for the generated type, including nesting types and namespaces.
            var wrappedMembers = SyntaxFactory.List(derive.WrapWithAncestors(context.ProcessingNode).Select(n => n.NormalizeWhitespace()));

            var usings = _options.HasFlag(SumTypeOptions.EnableJsonConverter)
                ? SyntaxFactory.List(new[]
                {
                    SyntaxFactory.UsingDirective(
                        SyntaxFactory.QualifiedName(
                            SyntaxFactory.IdentifierName("System"),
                            SyntaxFactory.IdentifierName("Reflection"))),
                })
                : SyntaxFactory.List<UsingDirectiveSyntax>();

            return Task.FromResult(new RichGenerationResult
            {
                Members = wrappedMembers,
                Usings = usings
            });
        }
    }
}
