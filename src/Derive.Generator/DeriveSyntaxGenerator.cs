﻿using CodeGeneration.Roslyn;
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
            if (interfaces.HasFlag(Impl.IStructuralEquatable))
            {
                yield return CreateStructuralEquatableSyntax(newDeclaration, members);
            }

            if (interfaces.HasFlag(Impl.Comparable))
            {
                yield return CreateComparableSyntax(newDeclaration, members);
            }
            if (interfaces.HasFlag(Impl.IStructuralComparable))
            {
                yield return CreateStructuralComparableSyntax(newDeclaration, members);
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
            var typeIdentifier = typeDeclaration.Identifier();

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
                                                    IdentifierName(member.Identifier)))
                                        }))))
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
                        QualifiedName(
                            IdentifierName("System"),
                            GenericName(
                                Identifier("IEquatable"))
                            .WithTypeArgumentList(
                                TypeArgumentList(
                                    SingletonSeparatedList<TypeSyntax>(
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

        private static MemberDeclarationSyntax CreateStructuralEquatableSyntax(TypeDeclarationSyntax typeDeclaration, List<(TypeSyntax Type, string Identifier)> members)
        {
            var typeIdentifier = typeDeclaration.Identifier();

            ExpressionSyntax eqExpr;
            BlockSyntax hashCodeBlock;
            switch (members.Count)
            {
                case 0:
                    eqExpr = LiteralExpression(SyntaxKind.TrueLiteralExpression);
                    hashCodeBlock = Block(
                        ReturnStatement(
                            LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(0))));
                    break;
                case 1:
                    hashCodeBlock = Block(
                        ReturnStatement(
                            InvocationExpression(
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("comparer"),
                                    IdentifierName("GetHashCode")))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList<ArgumentSyntax>(
                                        Argument(
                                            IdentifierName(members[0].Identifier)))))));
                    goto eqDefault;
                default:

                    var arguments = members
                        .Select(member => (SyntaxNodeOrToken)Argument(IdentifierName(member.Identifier)))
                        .Aggregate(new List<SyntaxNodeOrToken>(members.Count * 2), (a, current) =>
                        {
                            if (a.Count != 0)
                            {
                                a.Add(Token(SyntaxKind.CommaToken));
                            }
                            a.Add(current);
                            return a;
                        });
                    hashCodeBlock = Block(
                        LocalDeclarationStatement(
                            VariableDeclaration(
                                QualifiedName(
                                    QualifiedName(
                                        IdentifierName("System"),
                                        IdentifierName("Collections")),
                                    IdentifierName("IStructuralEquatable")))
                            .WithVariables(
                                SingletonSeparatedList<VariableDeclaratorSyntax>(
                                    VariableDeclarator(
                                        Identifier("tuple"))
                                    .WithInitializer(
                                        EqualsValueClause(
                                            TupleExpression(
                                                SeparatedList<ArgumentSyntax>(arguments))))))),
                        ReturnStatement(
                            InvocationExpression(
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("tuple"),
                                    IdentifierName("GetHashCode")))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList<ArgumentSyntax>(
                                        Argument(
                                            IdentifierName("comparer")))))));

                    eqDefault:;
                    eqExpr = members
                        .Select(member => (ExpressionSyntax)InvocationExpression(
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName("comparer"),
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
                                                        IdentifierName("objOther"),
                                                        IdentifierName(member.Identifier)))
                                            }))))
                        .Aggregate((accumulator, current) => BinaryExpression(SyntaxKind.LogicalAndExpression, accumulator, current));
                    break;
            }

            return typeDeclaration
        .WithBaseList(
            BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        QualifiedName(
                            QualifiedName(
                                IdentifierName("System"),
                                IdentifierName("Collections")),
                            IdentifierName("IStructuralEquatable"))))))
        .WithMembers(
            List<MemberDeclarationSyntax>(new[]
            {
                MethodDeclaration(
                    PredefinedType(
                        Token(SyntaxKind.BoolKeyword)),
                    Identifier("Equals"))
                .WithExplicitInterfaceSpecifier(
                    ExplicitInterfaceSpecifier(
                        QualifiedName(
                            QualifiedName(
                                IdentifierName("System"),
                                IdentifierName("Collections")),
                            IdentifierName("IStructuralEquatable"))))
                .WithParameterList(
                    ParameterList(
                        SeparatedList<ParameterSyntax>(
                            new SyntaxNodeOrToken[]{
                                Parameter(
                                    Identifier("other"))
                                .WithType(
                                    PredefinedType(
                                        Token(SyntaxKind.ObjectKeyword))),
                                Token(SyntaxKind.CommaToken),
                                Parameter(
                                    Identifier("comparer"))
                                .WithType(
                                    QualifiedName(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("Collections")),
                                        IdentifierName("IEqualityComparer")))})))
                .WithBody(
                    Block(
                        IfStatement(
                            BinaryExpression(
                                SyntaxKind.LogicalOrExpression,
                                BinaryExpression(
                                    SyntaxKind.EqualsExpression,
                                    IdentifierName("other"),
                                    LiteralExpression(
                                        SyntaxKind.NullLiteralExpression)),
                                PrefixUnaryExpression(
                                    SyntaxKind.LogicalNotExpression,
                                    ParenthesizedExpression(
                                        IsPatternExpression(
                                            IdentifierName("other"),
                                            DeclarationPattern(
                                                typeIdentifier,
                                                SingleVariableDesignation(
                                                    Identifier("objOther"))))))),
                            ReturnStatement(
                                LiteralExpression(
                                    SyntaxKind.FalseLiteralExpression))),
                        ReturnStatement(eqExpr))),

                MethodDeclaration(
                    PredefinedType(
                        Token(SyntaxKind.IntKeyword)),
                    Identifier("GetHashCode"))
                .WithExplicitInterfaceSpecifier(
                    ExplicitInterfaceSpecifier(
                        QualifiedName(
                            QualifiedName(
                                IdentifierName("System"),
                                IdentifierName("Collections")),
                            IdentifierName("IStructuralEquatable"))))
                .WithParameterList(
                    ParameterList(
                        SingletonSeparatedList<ParameterSyntax>(
                            Parameter(
                                Identifier("comparer"))
                            .WithType(
                                QualifiedName(
                                    QualifiedName(
                                        IdentifierName("System"),
                                        IdentifierName("Collections")),
                                    IdentifierName("IEqualityComparer"))))))
                .WithBody(hashCodeBlock)
            }));
        }

        private static MemberDeclarationSyntax CreateComparableSyntax(TypeDeclarationSyntax typeDeclaration, List<(TypeSyntax Type, string Identifier)> comparableMembers)
        {
            var typeIdentifier = typeDeclaration.Identifier();

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
        private static MemberDeclarationSyntax CreateStructuralComparableSyntax(TypeDeclarationSyntax typeDeclaration, List<(TypeSyntax Type, string Identifier)> members)
        {
            var typeIdentifier = typeDeclaration.Identifier();

            var compareStatements = new List<StatementSyntax>
            {
                IfStatement(
                    BinaryExpression(
                        SyntaxKind.EqualsExpression,
                        IdentifierName("other"),
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
                                IdentifierName("other"),
                                DeclarationPattern(
                                    typeIdentifier,
                                    SingleVariableDesignation(
                                        Identifier("objOther")))))),
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
                                                                        "Argument must be of type ",
                                                                        "Argument must be of type ",
                                                                        TriviaList())),
                                                                Interpolation(
                                                                    InvocationExpression(
                                                                        IdentifierName("GetType")))}))),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    InvocationExpression(
                                                        IdentifierName("nameof"))
                                                    .WithArgumentList(
                                                        ArgumentList(
                                                            SingletonSeparatedList<ArgumentSyntax>(
                                                                Argument(
                                                                    IdentifierName("other"))))))}))))))),
            };

            ExpressionSyntax Compare(string identifier)
            {
                return InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                IdentifierName("comparer"),
                                IdentifierName("Compare")))
                        .WithArgumentList(
                            ArgumentList(
                                SeparatedList<ArgumentSyntax>(
                                    new SyntaxNodeOrToken[]{
                                        Argument(
                                            IdentifierName(identifier)),
                                        Token(SyntaxKind.CommaToken),
                                        Argument(
                                            MemberAccessExpression(
                                                SyntaxKind.SimpleMemberAccessExpression,
                                                IdentifierName("objOther"),
                                                IdentifierName(identifier)))})));
            }

            (TypeSyntax Type, string Identifier) member;
            switch (members.Count)
            {
                case 0:
                    compareStatements.Add(ReturnStatement(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(0))));
                    break;
                case 1:
                    member = members.Single();
                    compareStatements.Add(ReturnStatement(Compare(member.Identifier)));
                    break;
                default:

                    member = members[0];
                    compareStatements.Add(LocalDeclarationStatement(
                        VariableDeclaration(
                            IdentifierName("var"))
                        .WithVariables(
                            SingletonSeparatedList<VariableDeclaratorSyntax>(
                                VariableDeclarator(
                                    Identifier("c"))
                                .WithInitializer(
                                    EqualsValueClause(
                                        Compare(member.Identifier)))))));

                    compareStatements.Add(
                        IfStatement(
                            BinaryExpression(
                                SyntaxKind.NotEqualsExpression,
                                IdentifierName("c"),
                                LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    Literal(0))),
                            ReturnStatement(IdentifierName("c"))));

                    for (var i = 1; i < members.Count - 1; i++)
                    {
                        member = members[i];
                        compareStatements.Add(
                            ExpressionStatement(
                                AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    IdentifierName("c"),
                                    Compare(member.Identifier))));

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

                    member = members[members.Count - 1];
                    compareStatements.Add(ReturnStatement(Compare(member.Identifier)));
                    break;
            }

            return typeDeclaration
        .WithBaseList(
            BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        QualifiedName(
                            QualifiedName(
                                IdentifierName("System"),
                                IdentifierName("Collections")),
                            IdentifierName("IStructuralComparable"))))))
        .WithMembers(
            List<MemberDeclarationSyntax>(new[]
            {
                MethodDeclaration(
                    PredefinedType(
                        Token(SyntaxKind.IntKeyword)),
                    Identifier("CompareTo"))
                .WithExplicitInterfaceSpecifier(
                    ExplicitInterfaceSpecifier(
                        QualifiedName(
                            QualifiedName(
                                IdentifierName("System"),
                                IdentifierName("Collections")),
                            IdentifierName("IStructuralComparable"))))
                .WithParameterList(
                    ParameterList(
                        SeparatedList<ParameterSyntax>(
                            new SyntaxNodeOrToken[]{
                                Parameter(
                                    Identifier("other"))
                                .WithType(
                                    PredefinedType(
                                        Token(SyntaxKind.ObjectKeyword))),
                                Token(SyntaxKind.CommaToken),
                                Parameter(
                                    Identifier("comparer"))
                                .WithType(
                                    QualifiedName(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("Collections")),
                                        IdentifierName("IComparer")))})))
                .WithBody(
                    Block(compareStatements))
            }));
        }

    }


}
