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
    public class DeriveGenerator : IRichCodeGenerator
    {
        private readonly Impl _which;

        public DeriveGenerator(AttributeData attributeData)
        {
            if (attributeData == null) throw new ArgumentNullException(nameof(attributeData));
            _which = (Impl)attributeData.ConstructorArguments[0].Value;
        }

        public Task<SyntaxList<MemberDeclarationSyntax>> GenerateAsync(TransformationContext context, IProgress<Diagnostic> progress, CancellationToken cancellationToken)
        {
            throw new NotImplementedException();
        }

        public Task<RichGenerationResult> GenerateRichAsync(TransformationContext context, IProgress<Diagnostic> progress, CancellationToken cancellationToken)
        {
            var applyToType = (TypeDeclarationSyntax)context.ProcessingNode;

            var derive = DeriveSyntaxGenerator.CreateSyntax(applyToType, _which);

            // Figure out ancestry for the generated type, including nesting types and namespaces.
            var wrappedMembers = SyntaxFactory.List(derive.WrapWithAncestors(context.ProcessingNode).Select(n => n.NormalizeWhitespace()));

            return Task.FromResult(new RichGenerationResult
            {
                Members = wrappedMembers,
            });
        }
    }
}
