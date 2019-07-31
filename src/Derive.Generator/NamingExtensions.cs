using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Linq;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Derive.Generator
{
    internal static class NamingExtensions
    {
        public static SimpleNameSyntax Identifier(this INamedTypeSymbol namedTypeSymbol, bool ignoreGenerics = false)
        {
            return !ignoreGenerics && namedTypeSymbol.TypeArguments.Length > 0
                ? GenericName(SyntaxFactory.Identifier(namedTypeSymbol.Name),
                    TypeArgumentList(
                        SeparatedList<TypeSyntax>(
                            namedTypeSymbol.TypeArguments
                                .Select(argument => IdentifierName(argument.Name)))))
                : (SimpleNameSyntax)IdentifierName(namedTypeSymbol.Name);
        }

        internal static SimpleNameSyntax Identifier(this TypeDeclarationSyntax typeDeclaration, bool ignoreGenerics = false)
        {
            if (!ignoreGenerics && typeDeclaration.TypeParameterList?.Parameters != null && typeDeclaration.TypeParameterList.Parameters.Count > 0)
            {
                return GenericName(SyntaxFactory.Identifier(typeDeclaration.Identifier.Text),
                    TypeArgumentList(
                        SeparatedList<TypeSyntax>(
                            typeDeclaration.TypeParameterList.Parameters
                                .Select(p => IdentifierName(p.Identifier.Text)))));
            }
            else
            {
                return IdentifierName(typeDeclaration.Identifier.Text);
            }
        }

        internal static NameSyntax QualifiedName(this SyntaxNode node, bool ignoreGenerics = false, bool stopAtType = false)
        {
            var builder = new List<SimpleNameSyntax>();
            var loop = true;
            for (; loop && node != null; node = node.Parent)
            {
                switch (node)
                {
                    case TypeDeclarationSyntax typeDeclaration:
                        builder.Add(typeDeclaration.Identifier(ignoreGenerics));
                        break;
                    case BaseTypeDeclarationSyntax typeDeclaration:
                        builder.Add(IdentifierName(typeDeclaration.Identifier.Text));
                        break;
                    case NamespaceDeclarationSyntax namespaceDeclaration:
                        if (stopAtType)
                        {
                            loop = false;
                            break;
                        }
                        NameSyntax name = namespaceDeclaration.Name;
                        loop:;
                        switch (name)
                        {
                            case QualifiedNameSyntax qualifiedName:
                                name = qualifiedName.Left;
                                builder.Add(qualifiedName.Right.WithoutTrivia());
                                goto loop;

                            case SimpleNameSyntax simpleNameSyntax:
                                builder.Add(simpleNameSyntax.WithoutTrivia());
                                break;
                        }
                        break;
                }
            }
            builder.Reverse();
            return builder
                .Cast<NameSyntax>()
                .Aggregate((acc, current) => SyntaxFactory.QualifiedName(acc, (SimpleNameSyntax)current));
        }

        public static NameSyntax FullyQualifiedName(this ITypeSymbol type, bool ignoreGenerics = false)
        {
            var builder = new List<SimpleNameSyntax>();

            if (type is ITypeParameterSymbol typeParameter)
            {
                return IdentifierName(typeParameter.Name);
            }

            if (type is INamedTypeSymbol named)
            {
                builder.Add(named.Identifier(ignoreGenerics));
            }
            else
            {
                builder.Add(IdentifierName(type.Name));
            }
            for (var node = type.ContainingType; node != null; node = node.ContainingType)
            {
                builder.Add(node.Identifier(ignoreGenerics));
            }
            for (var ns = type.ContainingNamespace; ns != null; ns = ns.ContainingNamespace)
            {
                if (ns.Name != "")
                {
                    builder.Add(IdentifierName(ns.Name));
                }
            }
            builder.Reverse();
            return builder
                .Cast<NameSyntax>()
                .Aggregate((acc, current) => SyntaxFactory.QualifiedName(acc, (SimpleNameSyntax)current));
        }

    }


}
