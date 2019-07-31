using CodeGeneration.Roslyn;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Derive.Generator
{
    public static class DeriveSyntaxGenerator
    {
        public static SyntaxList<MemberDeclarationSyntax> CreateSyntax(TransformationContext context, Impl interfaces)
        {

            return List<MemberDeclarationSyntax>(EnumerateSyntax(context, interfaces));
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
                        if (!ignoreGenerics && typeDeclaration.TypeParameterList?.Parameters != null && typeDeclaration.TypeParameterList.Parameters.Count > 0)
                        {
                            builder.Add(GenericName(Identifier(typeDeclaration.Identifier.Text),
                                TypeArgumentList(
                                    SeparatedList<TypeSyntax>(
                                        typeDeclaration.TypeParameterList.Parameters
                                            .Select(p => IdentifierName(p.Identifier.Text))))));
                        }
                        else
                        {
                            builder.Add(IdentifierName(typeDeclaration.Identifier.Text));
                        }
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
        public static SimpleNameSyntax Name(this INamedTypeSymbol namedTypeSymbol, bool ignoreGenerics = false)
        {
            return !ignoreGenerics && namedTypeSymbol.TypeArguments.Length > 0
                ? GenericName(Identifier(namedTypeSymbol.Name),
                    TypeArgumentList(
                        SeparatedList<TypeSyntax>(
                            namedTypeSymbol.TypeArguments
                                .Select(argument => IdentifierName(argument.Name)))))
                : (SimpleNameSyntax)IdentifierName(namedTypeSymbol.Name);
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
                builder.Add(named.Name(ignoreGenerics));
            }
            else
            {
                builder.Add(IdentifierName(type.Name));
            }
            for (var node = type.ContainingType; node != null; node = node.ContainingType)
            {
                builder.Add(node.Name(ignoreGenerics));
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

        private static IEnumerable<MemberDeclarationSyntax> EnumerateSyntax(TransformationContext context, Impl interfaces)
        {
            var original = (TypeDeclarationSyntax)context.ProcessingNode;
            var typeName = original.Identifier.Text;
            var qualifiedTypeName = original.QualifiedName(ignoreGenerics: true, stopAtType: true);
            var qualifiedName = original.QualifiedName();
            TypeDeclarationSyntax newDeclaration;
            switch (original)
            {
                case StructDeclarationSyntax structDeclarationSyntax:
                    newDeclaration = StructDeclaration(typeName);
                    break;
                case ClassDeclarationSyntax classDeclarationSyntax:
                    newDeclaration = ClassDeclaration(typeName);
                    break;
                default:
                    throw new NotSupportedException();
            }
            newDeclaration = newDeclaration
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.PartialKeyword)));

            if (original.TypeParameterList != null)
                newDeclaration = newDeclaration.WithTypeParameterList(original.TypeParameterList);

            var self = context.Compilation
                .GetSymbolsWithName(typeName, SymbolFilter.Type)
                .OfType<INamedTypeSymbol>()
                .SingleOrDefault(type => !type.IsImplicitlyDeclared && type.FullyQualifiedName().ToString() == qualifiedName.ToString())
                ?? throw new Exception($"Could not find type {qualifiedName}");

            var members = self.IterateMembers().ToList();

            if (interfaces.HasFlag(Impl.Equatable))
            {
                yield return CreateEquatableSyntax(newDeclaration, members);
            }
            if (interfaces.HasFlag(Impl.Comparable))
            {
                yield return CreateComparableSyntax(newDeclaration, members);
            }

            if (interfaces.HasFlag(Impl.Deconstruct) && members.Count > 0)
            {
                yield return CreateDeconstructSyntax(newDeclaration, members);
            }
        }

        internal static IEnumerable<(TypeSyntax Type, string Identifier)> IterateMembers(this INamedTypeSymbol namedType)
        {
            for (var type = namedType; type != null; type = type.BaseType)
            {
                foreach (var member in type.GetMembers())
                {
                    if (member.IsStatic || member.IsImplicitlyDeclared) continue;

                    switch (member)
                    {
                        case IFieldSymbol field:
                            yield return (field.Type.FullyQualifiedName(), field.Name);
                            break;
                        case IPropertySymbol property:
                            yield return (property.Type.FullyQualifiedName(), property.Name);
                            break;
                    }
                }
            }
        }

        // Generated using https://roslynquoter.azurewebsites.net/

        internal static MemberDeclarationSyntax CreateDeconstructSyntax(TypeDeclarationSyntax typeDeclaration, List<(TypeSyntax Type, string Identifier)> members)
        {
            if (members.Count == 0) return typeDeclaration;

            string Camel(string name) => $"@{name[0].ToString().ToLower()}{name.Substring(1)}";

            var parameters = members
                .Select(member => Parameter(
                        Identifier(Camel(member.Identifier)))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.OutKeyword)))
                    .WithType(member.Type))
                .Aggregate(new List<SyntaxNodeOrToken>(members.Count * 2), (a, current) =>
                {
                    if (a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                    }
                    a.Add(current);
                    return a;
                });

            var statements = members
                .Select(member => ExpressionStatement(
                                AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    IdentifierName(Camel(member.Identifier)),
                                    IdentifierName(member.Identifier))))
                .ToArray();

            return typeDeclaration
           .WithMembers(
               List<MemberDeclarationSyntax>(
                   new MemberDeclarationSyntax[] {
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.VoidKeyword)),
                        Identifier("Deconstruct"))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(parameters)))
                    .WithBody(
                        Block(statements))
                   }));
           }

        private static MemberDeclarationSyntax CreateEquatableSyntax(TypeDeclarationSyntax typeDeclaration, List<(TypeSyntax Type, string Identifier)> equatableMembers)
        {
            var typeIdentifier = typeDeclaration.TypeParameterList != null && typeDeclaration.TypeParameterList.Parameters.Count > 0
                ? (TypeSyntax)GenericName(
                    Identifier(typeDeclaration.Identifier.ValueText),
                    TypeArgumentList(
                        SeparatedList<TypeSyntax>(
                            typeDeclaration.TypeParameterList.Parameters.Select(x => IdentifierName(x.Identifier.Text)))
                    ))
                : IdentifierName(typeDeclaration.Identifier.ValueText);

            ExpressionSyntax eqExpr;
            switch (equatableMembers.Count)
            {
                case 0:
                    eqExpr = LiteralExpression(SyntaxKind.TrueLiteralExpression);
                    break;
                default:
                    eqExpr = equatableMembers
                        .Select(member => (ExpressionSyntax) InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            MemberAccessExpression(
                                                SyntaxKind.SimpleMemberAccessExpression,
                                                IdentifierName("System"),
                                                IdentifierName("Collections")),
                                            IdentifierName("Generic")),
                                        GenericName(
                                            Identifier("EqualityComparer"))
                                        .WithTypeArgumentList(
                                            TypeArgumentList(
                                                SingletonSeparatedList<TypeSyntax>(member.Type)))),
                                    IdentifierName("Default")),
                                IdentifierName("Equals")))
                            .WithArgumentList(
                                ArgumentList(
                                    SeparatedList<ArgumentSyntax>(
                                        new SyntaxNodeOrToken[]{
                                            Argument(
                                                IdentifierName(member.Identifier)),
                                            Token(SyntaxKind.CommaToken),
                                            Argument(
                                                MemberAccessExpression(
                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                    IdentifierName("other"),
                                                    IdentifierName(member.Identifier)))}))))
                        .Aggregate((accumulator, current) => BinaryExpression(SyntaxKind.LogicalAndExpression, accumulator, current));
                    break;
            }

            var syntaxNodeOrTokenList = equatableMembers
                .Select(member => Argument(IdentifierName(member.Identifier)))
                .Aggregate(new List<SyntaxNodeOrToken>(equatableMembers.Count * 2), (a, current) =>
                {
                    if (a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                    }
                    a.Add(current);
                    return a;
                });
            var hashCodeExpr = syntaxNodeOrTokenList.Count > 0
                ? (ExpressionSyntax) InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        TupleExpression(
                            SeparatedList<ArgumentSyntax>(syntaxNodeOrTokenList)),
                        IdentifierName("GetHashCode")))
                : LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(0));

            var eqOpExpr = typeDeclaration.IsKind(SyntaxKind.StructDeclaration)
                ? (ExpressionSyntax) InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        IdentifierName("left"),
                        IdentifierName("Equals")))
                    .WithArgumentList(
                        ArgumentList(
                            SingletonSeparatedList<ArgumentSyntax>(
                                Argument(
                                    IdentifierName("right")))))
                : BinaryExpression(
                    SyntaxKind.CoalesceExpression,
                    ConditionalAccessExpression(
                        IdentifierName("left"),
                        InvocationExpression(
                            MemberBindingExpression(
                                IdentifierName("Equals")))
                        .WithArgumentList(
                            ArgumentList(
                                SingletonSeparatedList<ArgumentSyntax>(
                                    Argument(
                                        IdentifierName("right")))))),
                    IsPatternExpression(
                        IdentifierName("right"),
                        ConstantPattern(
                            LiteralExpression(
                                SyntaxKind.NullLiteralExpression))));

            return typeDeclaration
        .WithBaseList(
            BaseList(
                SingletonSeparatedList(
(BaseTypeSyntax)SimpleBaseType(
                        SyntaxFactory.QualifiedName(
                            IdentifierName("System"),
                            GenericName(
                                Identifier("IEquatable"))
                            .WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList(
                                        typeIdentifier))))))))
        .WithMembers(
            List(
                new MemberDeclarationSyntax[]{
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Identifier("Equals"))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList(
                                Parameter(
                                    Identifier("other"))
                                .WithType(
                                    typeIdentifier))))
                    .WithExpressionBody(
                        ArrowExpressionClause(eqExpr))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Identifier("Equals"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList(
                                Parameter(
                                    Identifier("obj"))
                                .WithType(
                                    PredefinedType(
                                        Token(SyntaxKind.ObjectKeyword))))))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            BinaryExpression(
                                SyntaxKind.LogicalAndExpression,
                                BinaryExpression(
                                    SyntaxKind.LogicalAndExpression,
                                    PrefixUnaryExpression(
                                        SyntaxKind.LogicalNotExpression,
                                        ParenthesizedExpression(
                                            IsPatternExpression(
                                                IdentifierName("obj"),
                                                ConstantPattern(
                                                    LiteralExpression(
                                                        SyntaxKind.NullLiteralExpression))))),
                                    IsPatternExpression(
                                        IdentifierName("obj"),
                                        DeclarationPattern(
                                            typeIdentifier,
                                            SingleVariableDesignation(
                                                Identifier("other"))))),
                                InvocationExpression(
                                    IdentifierName("Equals"))
                                .WithArgumentList(
                                    ArgumentList(
                                        SingletonSeparatedList(
                                            Argument(
                                                IdentifierName("other"))))))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.IntKeyword)),
                        Identifier("GetHashCode"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithExpressionBody(
                        ArrowExpressionClause(hashCodeExpr))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    OperatorDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Token(SyntaxKind.EqualsEqualsToken))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.StaticKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("left"))
                                    .WithType(
                                        typeIdentifier),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        typeIdentifier)})))
                    .WithExpressionBody(
                        ArrowExpressionClause(eqOpExpr))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    OperatorDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Token(SyntaxKind.ExclamationEqualsToken))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.StaticKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("left"))
                                    .WithType(
                                        typeIdentifier),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        typeIdentifier)})))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            PrefixUnaryExpression(
                                SyntaxKind.LogicalNotExpression,
                                ParenthesizedExpression(
                                    BinaryExpression(
                                        SyntaxKind.EqualsExpression,
                                        IdentifierName("left"),
                                        IdentifierName("right"))))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken))}));
        }

        private static MemberDeclarationSyntax CreateComparableSyntax(TypeDeclarationSyntax typeDeclaration, List<(TypeSyntax Type, string Identifier)> comparableMembers)
        {
            var typeIdentifier = typeDeclaration.TypeParameterList != null && typeDeclaration.TypeParameterList.Parameters.Count > 0
                ? (TypeSyntax)GenericName(
                    Identifier(typeDeclaration.Identifier.ValueText),
                    TypeArgumentList(
                        SeparatedList<TypeSyntax>(
                            typeDeclaration.TypeParameterList.Parameters.Select(x => IdentifierName(x.Identifier.Text)))
                    ))
                : IdentifierName(typeDeclaration.Identifier.ValueText);

            var compareStatements = new List<StatementSyntax>();

            ExpressionSyntax Compare(TypeSyntax type, string identifier)
            {
                return InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName("System"),
                                        IdentifierName("Collections")),
                                    IdentifierName("Generic")),
                                GenericName(
                                    Identifier("Comparer"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(
                                        SingletonSeparatedList<TypeSyntax>(type)))),
                            IdentifierName("Default")),
                        IdentifierName("Compare")))
                    .WithArgumentList(
                        ArgumentList(
                            SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Argument(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("left"),
                                            IdentifierName(identifier))),
                                    Token(SyntaxKind.CommaToken),
                                    Argument(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("right"),
                                            IdentifierName(identifier)))})));
            }

            (TypeSyntax Type, string Identifier) member;
            switch (comparableMembers.Count)
            {
                case 0:
                    compareStatements.Add(ReturnStatement(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(0))));
                    break;
                case 1:
                    member = comparableMembers.Single();
                    compareStatements.Add(ReturnStatement(Compare(member.Type, member.Identifier)));
                    break;
                default:

                    member = comparableMembers[0];
                    compareStatements.Add(LocalDeclarationStatement(
                        VariableDeclaration(
                            IdentifierName("var"))
                        .WithVariables(
                            SingletonSeparatedList<VariableDeclaratorSyntax>(
                                VariableDeclarator(
                                    Identifier("c"))
                                .WithInitializer(
                                    EqualsValueClause(
                                        Compare(member.Type, member.Identifier)))))));

                    compareStatements.Add(
                        IfStatement(
                            BinaryExpression(
                                SyntaxKind.NotEqualsExpression,
                                IdentifierName("c"),
                                LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    Literal(0))),
                            ReturnStatement(IdentifierName("c"))));

                    for(var i = 1; i < comparableMembers.Count - 1; i++)
                    {
                        member = comparableMembers[i];
                        compareStatements.Add(
                            ExpressionStatement(
                                AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    IdentifierName("c"),
                                    Compare(member.Type, member.Identifier))));

                        compareStatements.Add(
                            IfStatement(
                                BinaryExpression(
                                    SyntaxKind.NotEqualsExpression,
                                    IdentifierName("c"),
                                    LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        Literal(0))),
                                ReturnStatement(IdentifierName("c"))));
                    }

                    member = comparableMembers[comparableMembers.Count - 1];
                    compareStatements.Add(ReturnStatement(Compare(member.Type, member.Identifier)));
                    break;
            }

            return typeDeclaration
        .WithBaseList(
            BaseList(
                SeparatedList<BaseTypeSyntax>(
                    new SyntaxNodeOrToken[]{
                        SimpleBaseType(
                            SyntaxFactory.QualifiedName(
                                IdentifierName("System"),
                                IdentifierName("IComparable"))),
                        Token(SyntaxKind.CommaToken),
                        SimpleBaseType(
                            SyntaxFactory.QualifiedName(
                                IdentifierName("System"),
                                GenericName(
                                    Identifier("IComparable"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(
                                        SingletonSeparatedList(
                                            typeIdentifier)))))})))
        .WithMembers(
            List(
                new MemberDeclarationSyntax[]{
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.IntKeyword)),
                        Identifier("CompareTo"))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList(
                                Parameter(
                                    Identifier("other"))
                                .WithType(
                                    typeIdentifier))))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            InvocationExpression(
                                IdentifierName("Compare"))
                            .WithArgumentList(
                                ArgumentList(
                                    SeparatedList<ArgumentSyntax>(
                                        new SyntaxNodeOrToken[]{
                                            Argument(
                                                ThisExpression()),
                                            Token(SyntaxKind.CommaToken),
                                            Argument(
                                                IdentifierName("other"))})))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.IntKeyword)),
                        Identifier("CompareTo"))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList(
                                Parameter(
                                    Identifier("obj"))
                                .WithType(
                                    PredefinedType(
                                        Token(SyntaxKind.ObjectKeyword))))))
                    .WithBody(
                        Block(
                            IfStatement(
                                BinaryExpression(
                                    SyntaxKind.EqualsExpression,
                                    IdentifierName("obj"),
                                    LiteralExpression(
                                        SyntaxKind.NullLiteralExpression)),
                                ReturnStatement(
                                    LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        Literal(1)))),
                            IfStatement(
                                PrefixUnaryExpression(
                                    SyntaxKind.LogicalNotExpression,
                                    ParenthesizedExpression(
                                        IsPatternExpression(
                                            IdentifierName("obj"),
                                            DeclarationPattern(
                                                typeIdentifier,
                                                SingleVariableDesignation(
                                                    Identifier("other")))))),
                                Block(
                                    SingletonList(
(StatementSyntax)ThrowStatement(
                                            ObjectCreationExpression(
                                                SyntaxFactory.QualifiedName(
                                                    IdentifierName("System"),
                                                    IdentifierName("ArgumentException")))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SeparatedList<ArgumentSyntax>(
                                                        new SyntaxNodeOrToken[]{
                                                            Argument(
                                                                InterpolatedStringExpression(
                                                                    Token(SyntaxKind.InterpolatedStringStartToken))
                                                                .WithContents(
                                                                    List(
                                                                        new InterpolatedStringContentSyntax[]{
                                                                            InterpolatedStringText()
                                                                            .WithTextToken(
                                                                                Token(
                                                                                    TriviaList(),
                                                                                    SyntaxKind.InterpolatedStringTextToken,
                                                                                    "Incorrect Type, expected ",
                                                                                    "Incorrect Type, expected ",
                                                                                    TriviaList())),
                                                                            Interpolation(
                                                                                InvocationExpression(
                                                                                    IdentifierName("GetType"))),
                                                                            InterpolatedStringText()
                                                                            .WithTextToken(
                                                                                Token(
                                                                                    TriviaList(),
                                                                                    SyntaxKind.InterpolatedStringTextToken,
                                                                                    ", received ",
                                                                                    ", received ",
                                                                                    TriviaList())),
                                                                            Interpolation(
                                                                                InvocationExpression(
                                                                                    MemberAccessExpression(
                                                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                                                        IdentifierName("obj"),
                                                                                        IdentifierName("GetType"))))}))),
                                                            Token(SyntaxKind.CommaToken),
                                                            Argument(
                                                                InvocationExpression(
                                                                    IdentifierName("nameof"))
                                                                .WithArgumentList(
                                                                    ArgumentList(
                                                                        SingletonSeparatedList(
                                                                            Argument(
                                                                                IdentifierName("obj"))))))}))))))),
                            ReturnStatement(
                                InvocationExpression(
                                    IdentifierName("CompareTo"))
                                .WithArgumentList(
                                    ArgumentList(
                                        SingletonSeparatedList(
                                            Argument(
                                                IdentifierName("other")))))))),
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.IntKeyword)),
                        Identifier("Compare"))
                    .WithModifiers(
                        TokenList(
                            new[]{
                                    Token(SyntaxKind.PublicKeyword),
                                    Token(SyntaxKind.StaticKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                        Parameter(
                                            Identifier("left"))
                                        .WithType(
                                            typeIdentifier),
                                        Token(SyntaxKind.CommaToken),
                                        Parameter(
                                            Identifier("right"))
                                        .WithType(
                                            typeIdentifier)})))
                    .WithBody(
                        Block(compareStatements)),
                    OperatorDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Token(SyntaxKind.LessThanToken))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.StaticKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("left"))
                                    .WithType(
                                        typeIdentifier),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        typeIdentifier)})))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            BinaryExpression(
                                SyntaxKind.GreaterThanExpression,
                                InvocationExpression(
                                    IdentifierName("Compare"))
                                .WithArgumentList(
                                    ArgumentList(
                                        SeparatedList<ArgumentSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                Argument(
                                                    IdentifierName("left")),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    IdentifierName("right"))}))),
                                LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    Literal(0)))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    OperatorDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Token(SyntaxKind.GreaterThanToken))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.StaticKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("left"))
                                    .WithType(
                                        typeIdentifier),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        typeIdentifier)})))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            BinaryExpression(
                                SyntaxKind.GreaterThanExpression,
                                InvocationExpression(
                                    IdentifierName("Compare"))
                                .WithArgumentList(
                                    ArgumentList(
                                        SeparatedList<ArgumentSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                Argument(
                                                    IdentifierName("left")),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    IdentifierName("right"))}))),
                                LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    Literal(0)))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    OperatorDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Token(SyntaxKind.LessThanEqualsToken))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.StaticKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("left"))
                                    .WithType(
                                        typeIdentifier),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        typeIdentifier)})))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            BinaryExpression(
                                SyntaxKind.LessThanOrEqualExpression,
                                InvocationExpression(
                                    IdentifierName("Compare"))
                                .WithArgumentList(
                                    ArgumentList(
                                        SeparatedList<ArgumentSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                Argument(
                                                    IdentifierName("left")),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    IdentifierName("right"))}))),
                                LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    Literal(0)))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    OperatorDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Token(SyntaxKind.GreaterThanEqualsToken))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.StaticKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("left"))
                                    .WithType(
                                        typeIdentifier),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        typeIdentifier)})))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            BinaryExpression(
                                SyntaxKind.GreaterThanOrEqualExpression,
                                InvocationExpression(
                                    IdentifierName("Compare"))
                                .WithArgumentList(
                                    ArgumentList(
                                        SeparatedList<ArgumentSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                Argument(
                                                    IdentifierName("left")),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    IdentifierName("right"))}))),
                                LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    Literal(0)))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken))
                }));
        }

    }


}
