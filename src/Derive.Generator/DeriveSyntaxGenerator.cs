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
        public static SyntaxList<MemberDeclarationSyntax> CreateSyntax(TypeDeclarationSyntax original, Interfaces interfaces)
        {
            return List<MemberDeclarationSyntax>(EnumerateSyntax(original, interfaces));
        }

        private static IEnumerable<MemberDeclarationSyntax> EnumerateSyntax(TypeDeclarationSyntax original, Interfaces interfaces)
        {
            var typeName = original.Identifier.ValueText;
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
            var members = original.Members.SelectMany(IterateMembers).ToList();

            if (interfaces.HasFlag(Interfaces.Equatable))
            {
                yield return CreateEquatableSyntax(newDeclaration, members);
            }
            if (interfaces.HasFlag(Interfaces.Comparable))
            {
                yield return CreateComparableSyntax(newDeclaration, members);
            }
        }

        private static IEnumerable<(TypeSyntax Type, string Identifier)> IterateMembers(MemberDeclarationSyntax memberDeclarationSyntax)
        {
            switch (memberDeclarationSyntax)
            {
                case FieldDeclarationSyntax fieldDeclarationSyntax:
                    foreach (var variableDeclaratorSyntax in fieldDeclarationSyntax.Declaration.Variables)
                    {
                        yield return (fieldDeclarationSyntax.Declaration.Type, variableDeclaratorSyntax.Identifier.ValueText);
                    }
                    break;
                case PropertyDeclarationSyntax propertyDeclarationSyntax:
                    yield return (propertyDeclarationSyntax.Type, propertyDeclarationSyntax.Identifier.ValueText);
                    break;
            }
        }

        // Generated using https://roslynquoter.azurewebsites.net/
        private static MemberDeclarationSyntax CreateEquatableSyntax(TypeDeclarationSyntax typeDeclaration, List<(TypeSyntax Type, string Identifier)> equatableMembers)
        {
            var typeName = typeDeclaration.Identifier.ValueText;
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
        .WithModifiers(
            TokenList(
                Token(SyntaxKind.PartialKeyword)))
        .WithBaseList(
            BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        QualifiedName(
                            IdentifierName("System"),
                            GenericName(
                                Identifier("IEquatable"))
                            .WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList<TypeSyntax>(
                                        IdentifierName(typeName)))))))))
        .WithMembers(
            List<MemberDeclarationSyntax>(
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
                            SingletonSeparatedList<ParameterSyntax>(
                                Parameter(
                                    Identifier("other"))
                                .WithType(
                                    IdentifierName(typeName)))))
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
                            SingletonSeparatedList<ParameterSyntax>(
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
                                            IdentifierName(typeName),
                                            SingleVariableDesignation(
                                                Identifier("other"))))),
                                InvocationExpression(
                                    IdentifierName("Equals"))
                                .WithArgumentList(
                                    ArgumentList(
                                        SingletonSeparatedList<ArgumentSyntax>(
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
                                        IdentifierName(typeName)),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        IdentifierName(typeName))})))
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
                                        IdentifierName(typeName)),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        IdentifierName(typeName))})))
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
            var typeName = typeDeclaration.Identifier.ValueText;
            var compareStatements = new List<StatementSyntax>();
            if(typeDeclaration.IsKind(SyntaxKind.ClassDeclaration))
                compareStatements.Add(IfStatement(
                    BinaryExpression(
                        SyntaxKind.EqualsExpression,
                        IdentifierName("other"),
                        LiteralExpression(SyntaxKind.NullLiteralExpression)),
                    ReturnStatement(
                        LiteralExpression(SyntaxKind.NumericLiteralExpression,
                        Literal(1)))));

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
        .WithModifiers(
            TokenList(
                Token(SyntaxKind.PartialKeyword)))
        .WithBaseList(
            BaseList(
                SeparatedList<BaseTypeSyntax>(
                    new SyntaxNodeOrToken[]{
                        SimpleBaseType(
                            QualifiedName(
                                IdentifierName("System"),
                                IdentifierName("IComparable"))),
                        Token(SyntaxKind.CommaToken),
                        SimpleBaseType(
                            QualifiedName(
                                IdentifierName("System"),
                                GenericName(
                                    Identifier("IComparable"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(
                                        SingletonSeparatedList<TypeSyntax>(
                                            IdentifierName(typeName))))))})))
        .WithMembers(
            List<MemberDeclarationSyntax>(
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
                            SingletonSeparatedList<ParameterSyntax>(
                                Parameter(
                                    Identifier("other"))
                                .WithType(
                                    IdentifierName(typeName)))))
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
                            SingletonSeparatedList<ParameterSyntax>(
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
                                                IdentifierName(typeName),
                                                SingleVariableDesignation(
                                                    Identifier("other")))))),
                                Block(
                                    SingletonList<StatementSyntax>(
                                        ThrowStatement(
                                            ObjectCreationExpression(
                                                QualifiedName(
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
                                                                    List<InterpolatedStringContentSyntax>(
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
                                                                        SingletonSeparatedList<ArgumentSyntax>(
                                                                            Argument(
                                                                                IdentifierName("obj"))))))}))))))),
                            ReturnStatement(
                                InvocationExpression(
                                    IdentifierName("CompareTo"))
                                .WithArgumentList(
                                    ArgumentList(
                                        SingletonSeparatedList<ArgumentSyntax>(
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
                                            IdentifierName(typeName)),
                                        Token(SyntaxKind.CommaToken),
                                        Parameter(
                                            Identifier("right"))
                                        .WithType(
                                            IdentifierName(typeName))})))
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
                                        IdentifierName(typeName)),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        IdentifierName(typeName))})))
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
                                        IdentifierName(typeName)),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        IdentifierName(typeName))})))
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
                                        IdentifierName(typeName)),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        IdentifierName(typeName))})))
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
                                        IdentifierName(typeName)),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("right"))
                                    .WithType(
                                        IdentifierName(typeName))})))
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
