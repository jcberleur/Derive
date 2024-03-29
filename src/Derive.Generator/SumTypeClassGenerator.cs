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
    public static class SumTypeClassGenerator
    {
        internal static SyntaxList<MemberDeclarationSyntax> CreateSyntax(TransformationContext context, string discriminantName, SumTypeOptions options)
        {
            return List(CreateSyntaxInternal(context, discriminantName, options));
        }

        internal static IEnumerable<MemberDeclarationSyntax> CreateSyntaxInternal(TransformationContext context, string discriminantName, SumTypeOptions options)
        {
            var original = (ClassDeclarationSyntax)context.ProcessingNode;
            var typeName = original.Identifier.Text;
            var qualifiedName = original.QualifiedName();
            SimpleNameSyntax TypeIdentifier(string name, bool omitTypeArgs = false)
            {
                if (original.TypeParameterList == null || original.TypeParameterList.Parameters.Count == 0)
                    return IdentifierName(name);

                return GenericName(
                    Identifier(name),
                    TypeArgumentList(
                        SeparatedList<TypeSyntax>(
                            original.TypeParameterList.Parameters.Select(x => omitTypeArgs ? (TypeSyntax)OmittedTypeArgument() : IdentifierName(x.Identifier.Text)))
                        ));

            }
            var typeIdentifier = TypeIdentifier(typeName);

            var self = context.Compilation
                .GetSymbolsWithName(typeName, SymbolFilter.Type)
                .OfType<INamedTypeSymbol>()
                .SingleOrDefault(type => !type.IsImplicitlyDeclared && type.FullyQualifiedName().ToString() == qualifiedName.ToString())
                ?? throw new Exception($"Could not find type {qualifiedName}");

            var baseMembers = self.IterateMembers().ToList();

            var cases = original.Members
                .OfType<ClassDeclarationSyntax>()
                .Select(c => c.Identifier.Text)
                .ToList();

            string Camel(string name) => $"@{name[0].ToString().ToLower()}{name.Substring(1)}";

            IEnumerable<MemberDeclarationSyntax> Members()
            {
                if (!options.HasFlag(SumTypeOptions.DisableConstructor))
                {
                    yield return ConstructorDeclaration(
                                Identifier(typeName))
                            .WithModifiers(
                                TokenList(
                                    Token(SyntaxKind.PrivateKeyword),
                                    Token(SyntaxKind.ProtectedKeyword)))
                            .WithBody(Block());
                }

                yield return PropertyDeclaration(
                        IdentifierName("Discriminant"),
                        Identifier(discriminantName))
                    .WithModifiers(
                        TokenList(
                            new[]{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.AbstractKeyword)}))
                    .WithAccessorList(
                        AccessorList(
                            SingletonList<AccessorDeclarationSyntax>(
                                AccessorDeclaration(
                                    SyntaxKind.GetAccessorDeclaration)
                                .WithSemicolonToken(
                                    Token(SyntaxKind.SemicolonToken)))));

                var enumMembers = cases.Aggregate(new List<SyntaxNodeOrToken>(), (a, memberName) =>
                {
                    if (a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                        a.Add(EnumMemberDeclaration(Identifier(memberName)));
                    }
                    else
                    {
                        a.Add(EnumMemberDeclaration(Identifier(memberName))
                            .WithEqualsValue(
                                EqualsValueClause(
                                    LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        Literal(1)))));
                    }
                    return a;
                });
                yield return EnumDeclaration("Discriminant")
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithMembers(
                        SeparatedList<EnumMemberDeclarationSyntax>(enumMembers));

                var matchParameters = cases.Aggregate(new List<SyntaxNodeOrToken>(), (a, memberName) =>
                {
                    if (a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                    }
                    a.Add(Parameter(Identifier(Camel(memberName)))
                        .WithType(
                            QualifiedName(
                                IdentifierName("System"),
                                GenericName(
                                    Identifier("Func"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(
                                        SeparatedList<TypeSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                IdentifierName(memberName),
                                                Token(SyntaxKind.CommaToken),
                                                IdentifierName("TReturn")
                                            }))))));
                    return a;
                });

                var matchCases = cases.Aggregate(new List<SwitchSectionSyntax>(), (a, memberName) =>
                {
                    a.Add(
                        SwitchSection()
                            .WithLabels(
                                SingletonList<SwitchLabelSyntax>(
                                    CasePatternSwitchLabel(
                                        DeclarationPattern(
                                            IdentifierName(memberName),
                                            SingleVariableDesignation(
                                                Identifier("self"))),
                                        Token(SyntaxKind.ColonToken))))
                            .WithStatements(
                                SingletonList<StatementSyntax>(
                                    ReturnStatement(
                                        InvocationExpression(
                                            IdentifierName(Camel(memberName)))
                                        .WithArgumentList(
                                            ArgumentList(
                                                SingletonSeparatedList<ArgumentSyntax>(
                                                    Argument(
                                                        IdentifierName("self"))))))))
                    );
                    return a;
                });
                matchCases.Add(
                    SwitchSection()
                        .WithLabels(
                            SingletonList<SwitchLabelSyntax>(
                                DefaultSwitchLabel()))
                        .WithStatements(
                            SingletonList<StatementSyntax>(
                                ThrowStatement(
                                    ObjectCreationExpression(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("ArgumentOutOfRangeException")))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SeparatedList<ArgumentSyntax>(
                                                new SyntaxNodeOrToken[]{
                                                    Argument(
                                                        InvocationExpression(
                                                            IdentifierName("nameof"))
                                                        .WithArgumentList(
                                                            ArgumentList(
                                                                SingletonSeparatedList<ArgumentSyntax>(
                                                                    Argument(
                                                                        IdentifierName(discriminantName)))))),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        ThisExpression()),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        LiteralExpression(
                                                            SyntaxKind.StringLiteralExpression,
                                                            Literal("@\"Unreachable\"", "Unreachable")))})))))));

                yield return MethodDeclaration(
                        IdentifierName("TReturn"),
                        Identifier("Match"))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithTypeParameterList(
                        TypeParameterList(
                            SingletonSeparatedList<TypeParameterSyntax>(
                                TypeParameter(
                                    Identifier("TReturn")))))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(matchParameters)))
                    .WithBody(
                        Block(
                            SingletonList<StatementSyntax>(
                                SwitchStatement(
                                    ThisExpression())
                                .WithOpenParenToken(
                                    Token(SyntaxKind.OpenParenToken))
                                .WithCloseParenToken(
                                    Token(SyntaxKind.CloseParenToken))
                                .WithSections(
                                    List<SwitchSectionSyntax>(matchCases)))))
                    .WithAttributeLists(
                        SingletonList<AttributeListSyntax>(
                            AttributeList(
                                SingletonSeparatedList<AttributeSyntax>(
                                    Attribute(
                                        QualifiedName(
                                            QualifiedName(
                                                QualifiedName(
                                                    IdentifierName("System"),
                                                    IdentifierName("Runtime")),
                                                IdentifierName("CompilerServices")),
                                            IdentifierName("MethodImpl")))
                                    .WithArgumentList(
                                        AttributeArgumentList(
                                            SingletonSeparatedList<AttributeArgumentSyntax>(
                                                AttributeArgument(
                                                    MemberAccessExpression(
                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                        MemberAccessExpression(
                                                            SyntaxKind.SimpleMemberAccessExpression,
                                                            MemberAccessExpression(
                                                                SyntaxKind.SimpleMemberAccessExpression,
                                                                MemberAccessExpression(
                                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                                    IdentifierName("System"),
                                                                    IdentifierName("Runtime")),
                                                                IdentifierName("CompilerServices")),
                                                            IdentifierName("MethodImplOptions")),
                                                        IdentifierName("AggressiveInlining"))))))))));

                var switchParameters = cases.Aggregate(new List<SyntaxNodeOrToken>(), (a, memberName) =>
                {
                    if (a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                    }
                    a.Add(Parameter(Identifier(Camel(memberName)))
                        .WithType(
                            QualifiedName(
                                IdentifierName("System"),
                                GenericName(
                                    Identifier("Action"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(
                                        SingletonSeparatedList<TypeSyntax>(
                                            IdentifierName(memberName)))))));
                    return a;
                });

                var switchCases = cases.Aggregate(new List<SwitchSectionSyntax>(), (a, memberName) =>
                {
                    a.Add(
                        SwitchSection()
                            .WithLabels(
                                SingletonList<SwitchLabelSyntax>(
                                    CasePatternSwitchLabel(
                                        DeclarationPattern(
                                            IdentifierName(memberName),
                                            SingleVariableDesignation(
                                                Identifier("self"))),
                                        Token(SyntaxKind.ColonToken))))
                            .WithStatements(
                                List<StatementSyntax>(
                                    new StatementSyntax[]{
                                        ExpressionStatement(
                                            InvocationExpression(
                                                IdentifierName(Camel(memberName)))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SingletonSeparatedList<ArgumentSyntax>(
                                                        Argument(
                                                            IdentifierName("self")))))),
                                        ReturnStatement()}))
                    );
                    return a;
                });
                switchCases.Add(
                    SwitchSection()
                        .WithLabels(
                            SingletonList<SwitchLabelSyntax>(
                                DefaultSwitchLabel()))
                        .WithStatements(
                            SingletonList<StatementSyntax>(
                                ThrowStatement(
                                    ObjectCreationExpression(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("ArgumentOutOfRangeException")))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SeparatedList<ArgumentSyntax>(
                                                new SyntaxNodeOrToken[]{
                                                    Argument(
                                                        InvocationExpression(
                                                            IdentifierName("nameof"))
                                                        .WithArgumentList(
                                                            ArgumentList(
                                                                SingletonSeparatedList<ArgumentSyntax>(
                                                                    Argument(
                                                                        IdentifierName(discriminantName)))))),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        ThisExpression()),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        LiteralExpression(
                                                            SyntaxKind.StringLiteralExpression,
                                                            Literal("@\"Unreachable\"", "Unreachable")))})))))));

                yield return MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.VoidKeyword)),
                        Identifier("Switch"))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(switchParameters)))
                    .WithBody(
                        Block(
                            SingletonList<StatementSyntax>(
                                SwitchStatement(
                                    ThisExpression())
                                .WithOpenParenToken(
                                    Token(SyntaxKind.OpenParenToken))
                                .WithCloseParenToken(
                                    Token(SyntaxKind.CloseParenToken))
                                .WithSections(
                                    List<SwitchSectionSyntax>(switchCases)))))
                    .WithAttributeLists(
                        SingletonList<AttributeListSyntax>(
                            AttributeList(
                                SingletonSeparatedList<AttributeSyntax>(
                                    Attribute(
                                        QualifiedName(
                                            QualifiedName(
                                                QualifiedName(
                                                    IdentifierName("System"),
                                                    IdentifierName("Runtime")),
                                                IdentifierName("CompilerServices")),
                                            IdentifierName("MethodImpl")))
                                    .WithArgumentList(
                                        AttributeArgumentList(
                                            SingletonSeparatedList<AttributeArgumentSyntax>(
                                                AttributeArgument(
                                                    MemberAccessExpression(
                                                        SyntaxKind.SimpleMemberAccessExpression,
                                                        MemberAccessExpression(
                                                            SyntaxKind.SimpleMemberAccessExpression,
                                                            MemberAccessExpression(
                                                                SyntaxKind.SimpleMemberAccessExpression,
                                                                MemberAccessExpression(
                                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                                    IdentifierName("System"),
                                                                    IdentifierName("Runtime")),
                                                                IdentifierName("CompilerServices")),
                                                            IdentifierName("MethodImplOptions")),
                                                        IdentifierName("AggressiveInlining"))))))))));

                foreach(var caseClass in original.Members.OfType<ClassDeclarationSyntax>())
                {
                    var caseClassAttributes = new List<AttributeListSyntax>();

                    if (options.HasFlag(SumTypeOptions.EnableJsonConverter))
                    {
                        caseClassAttributes.Add(
                            AttributeList(
                                SingletonSeparatedList(
                                    Attribute(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonConverter")))
                                    .WithArgumentList(
                                        AttributeArgumentList(
                                            SingletonSeparatedList<AttributeArgumentSyntax>(
                                                AttributeArgument(
                                                    TypeOfExpression(
                                                        IdentifierName($"{typeName}DefaultJsonConverter"))))))))
                        );
                    }

                    var caseName = caseClass.Identifier.Text;
                    //var caseNestedNamed = caseClass.QualifiedName(ignoreGenerics: true, stopAtType: true).ToString();
                    var caseQualifiedName = caseClass.QualifiedName();

                    var caseType = context.Compilation
                        .GetSymbolsWithName(caseName, SymbolFilter.Type)
                        .OfType<INamedTypeSymbol>()
                        .SingleOrDefault(type => !type.IsImplicitlyDeclared && type.FullyQualifiedName().ToString() == caseQualifiedName.ToString())
                        ?? throw new Exception($"Could not find case type {caseQualifiedName}");

                    var caseClassDef = (ClassDeclarationSyntax)DeriveSyntaxGenerator.CreateDeconstructSyntax(
                        ClassDeclaration(caseName)
                            .WithAttributeLists(
                                List<AttributeListSyntax>(caseClassAttributes))
                            .WithModifiers(
                                TokenList(
                                    new[] {
                                        Token(SyntaxKind.PublicKeyword),
                                        Token(SyntaxKind.SealedKeyword),
                                        Token(SyntaxKind.PartialKeyword) }))
                            .WithBaseList(
                                BaseList(
                                    SingletonSeparatedList<BaseTypeSyntax>(
                                        SimpleBaseType(
                                            typeIdentifier)))),
                        caseType.IterateMembers().ToList());

                    yield return caseClassDef.WithMembers(
                        caseClassDef.Members.Add(
                            PropertyDeclaration(
                                IdentifierName("Discriminant"),
                                Identifier(discriminantName))
                            .WithModifiers(
                                TokenList(
                                    new[]{
                                        Token(SyntaxKind.PublicKeyword),
                                        Token(SyntaxKind.OverrideKeyword)}))
                            .WithExpressionBody(
                                ArrowExpressionClause(
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName("Discriminant"),
                                        IdentifierName(caseName))))
                            .WithSemicolonToken(
                                Token(SyntaxKind.SemicolonToken))
                        ));
                }

            }

            var attributes = new List<AttributeListSyntax>();
            if (options.HasFlag(SumTypeOptions.EnableJsonConverter))
            {
                attributes.Add(
                    AttributeList(
                        SingletonSeparatedList<AttributeSyntax>(
                            Attribute(
                                QualifiedName(
                                    QualifiedName(
                                        IdentifierName("Newtonsoft"),
                                        IdentifierName("Json")),
                                    IdentifierName("JsonConverter")))
                            .WithArgumentList(
                                AttributeArgumentList(
                                    SingletonSeparatedList<AttributeArgumentSyntax>(
                                        AttributeArgument(
                                            TypeOfExpression(
                                                IdentifierName($"{typeName}JsonConverter"))))))))
                );
            }

            yield return ClassDeclaration(typeName)
                .WithTypeParameterList(original.TypeParameterList)
                .WithAttributeLists(
                    List<AttributeListSyntax>(
                        List(attributes)))
                .WithModifiers(
                    TokenList(
                        new[]{
                            Token(SyntaxKind.AbstractKeyword),
                            Token(SyntaxKind.PartialKeyword)}))
                .WithMembers(
                    List<MemberDeclarationSyntax>(Members()));

            if (!options.HasFlag(SumTypeOptions.EnableJsonConverter))
            {
                yield break;
            }

            var dictionaryCases = cases.Aggregate(new List<SyntaxNodeOrToken>(), (a, caseName) =>
            {
                if (a.Count != 0)
                {
                    a.Add(Token(SyntaxKind.CommaToken));
                }
                a.Add(
                    InitializerExpression(
                        SyntaxKind.ComplexElementInitializerExpression,
                        SeparatedList<ExpressionSyntax>(
                            new SyntaxNodeOrToken[]{
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            QualifiedName(typeIdentifier, IdentifierName("Discriminant")),
                                            IdentifierName(caseName)),
                                        Token(SyntaxKind.CommaToken),
                                        TypeOfExpression(
                                            QualifiedName(typeIdentifier, IdentifierName(caseName)))}))
                );
                return a;
            });
            yield return ClassDeclaration($"{typeName}JsonConverter")
                .WithTypeParameterList(original.TypeParameterList)
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.InternalKeyword)))
                .WithBaseList(
                    BaseList(
                        SingletonSeparatedList<BaseTypeSyntax>(
                            SimpleBaseType(
                                QualifiedName(
                                    QualifiedName(
                                        IdentifierName("Newtonsoft"),
                                        IdentifierName("Json")),
                                    IdentifierName("JsonConverter"))))))
                .WithMembers(
                    List<MemberDeclarationSyntax>(
                        new MemberDeclarationSyntax[]{
                                    FieldDeclaration(
                                        VariableDeclaration(
                                            QualifiedName(
                                                QualifiedName(
                                                    QualifiedName(
                                                        IdentifierName("System"),
                                                        IdentifierName("Collections")),
                                                    IdentifierName("Generic")),
                                                GenericName(
                                                    Identifier("Dictionary"))
                                                .WithTypeArgumentList(
                                                    TypeArgumentList(
                                                        SeparatedList<TypeSyntax>(
                                                            new SyntaxNodeOrToken[]{
                                                                QualifiedName(typeIdentifier, IdentifierName("Discriminant")),
                                                                Token(SyntaxKind.CommaToken),
                                                                QualifiedName(
                                                                    IdentifierName("System"),
                                                                    IdentifierName("Type"))})))))
                                        .WithVariables(
                                            SingletonSeparatedList<VariableDeclaratorSyntax>(
                                                VariableDeclarator(
                                                    Identifier("_cases"))
                                                .WithInitializer(
                                                    EqualsValueClause(
                                                        ObjectCreationExpression(
                                                            QualifiedName(
                                                                QualifiedName(
                                                                    QualifiedName(
                                                                        IdentifierName("System"),
                                                                        IdentifierName("Collections")),
                                                                    IdentifierName("Generic")),
                                                                GenericName(
                                                                    Identifier("Dictionary"))
                                                                .WithTypeArgumentList(
                                                                    TypeArgumentList(
                                                                        SeparatedList<TypeSyntax>(
                                                                            new SyntaxNodeOrToken[]{
                                                                                QualifiedName(typeIdentifier, IdentifierName("Discriminant")),
                                                                                Token(SyntaxKind.CommaToken),
                                                                                QualifiedName(
                                                                                    IdentifierName("System"),
                                                                                    IdentifierName("Type"))})))))
                                                        .WithInitializer(
                                                            InitializerExpression(
                                                                SyntaxKind.CollectionInitializerExpression,
                                                                SeparatedList<ExpressionSyntax>(dictionaryCases))))))))
                                    .WithModifiers(
                                        TokenList(
                                            new []{
                                                Token(SyntaxKind.PrivateKeyword),
                                                Token(SyntaxKind.ReadOnlyKeyword)})),

                                    PropertyDeclaration(
                                        PredefinedType(
                                            Token(SyntaxKind.BoolKeyword)),
                                        Identifier("CanWrite"))
                                    .WithModifiers(
                                        TokenList(
                                            new []{
                                                Token(SyntaxKind.PublicKeyword),
                                                Token(SyntaxKind.OverrideKeyword)}))
                                    .WithExpressionBody(
                                        ArrowExpressionClause(
                                            LiteralExpression(
                                                SyntaxKind.FalseLiteralExpression)))
                                    .WithSemicolonToken(
                                        Token(SyntaxKind.SemicolonToken)),

                                    MethodDeclaration(
                                        PredefinedType(
                                            Token(SyntaxKind.VoidKeyword)),
                                        Identifier("WriteJson"))
                                    .WithModifiers(
                                        TokenList(
                                            new []{
                                                Token(SyntaxKind.PublicKeyword),
                                                Token(SyntaxKind.OverrideKeyword)}))
                                    .WithParameterList(
                                        ParameterList(
                                            SeparatedList<ParameterSyntax>(
                                                new SyntaxNodeOrToken[]{
                                                    Parameter(
                                                        Identifier("writer"))
                                                    .WithType(
                                                        QualifiedName(
                                                            QualifiedName(
                                                                IdentifierName("Newtonsoft"),
                                                                IdentifierName("Json")),
                                                            IdentifierName("JsonWriter"))),
                                                    Token(SyntaxKind.CommaToken),
                                                    Parameter(
                                                        Identifier("value"))
                                                    .WithType(
                                                        PredefinedType(
                                                            Token(SyntaxKind.ObjectKeyword))),
                                                    Token(SyntaxKind.CommaToken),
                                                    Parameter(
                                                        Identifier("serializer"))
                                                    .WithType(
                                                        QualifiedName(
                                                            QualifiedName(
                                                                IdentifierName("Newtonsoft"),
                                                                IdentifierName("Json")),
                                                            IdentifierName("JsonSerializer")))})))
                                    .WithBody(
                                        Block(
                                            SingletonList<StatementSyntax>(
                                                ThrowStatement(
                                                    ObjectCreationExpression(
                                                        QualifiedName(
                                                            IdentifierName("System"),
                                                            IdentifierName("NotSupportedException")))
                                                    .WithArgumentList(
                                                        ArgumentList(
                                                            SingletonSeparatedList<ArgumentSyntax>(
                                                                Argument(
                                                                    LiteralExpression(
                                                                        SyntaxKind.StringLiteralExpression,
                                                                        Literal("Unnecessary because CanWrite is false. The type will skip the converter.")))))))))),

        MethodDeclaration(
            PredefinedType(
                Token(SyntaxKind.ObjectKeyword)),
            Identifier("ReadJson"))
        .WithModifiers(
            TokenList(
                new []{
                    Token(SyntaxKind.PublicKeyword),
                    Token(SyntaxKind.OverrideKeyword)}))
        .WithParameterList(
            ParameterList(
                SeparatedList<ParameterSyntax>(
                    new SyntaxNodeOrToken[]{
                        Parameter(
                            Identifier("reader"))
                        .WithType(
                            QualifiedName(
                                QualifiedName(
                                    IdentifierName("Newtonsoft"),
                                    IdentifierName("Json")),
                                IdentifierName("JsonReader"))),
                        Token(SyntaxKind.CommaToken),
                        Parameter(
                            Identifier("objectType"))
                        .WithType(
                            QualifiedName(
                                IdentifierName("System"),
                                IdentifierName("Type"))),
                        Token(SyntaxKind.CommaToken),
                        Parameter(
                            Identifier("existingValue"))
                        .WithType(
                            PredefinedType(
                                Token(SyntaxKind.ObjectKeyword))),
                        Token(SyntaxKind.CommaToken),
                        Parameter(
                            Identifier("serializer"))
                        .WithType(
                            QualifiedName(
                                QualifiedName(
                                    IdentifierName("Newtonsoft"),
                                    IdentifierName("Json")),
                                IdentifierName("JsonSerializer")))})))
        .WithBody(
            Block(
                LocalDeclarationStatement(
                    VariableDeclaration(
                        IdentifierName("var"))
                    .WithVariables(
                        SingletonSeparatedList<VariableDeclaratorSyntax>(
                            VariableDeclarator(
                                Identifier("jObject"))
                            .WithInitializer(
                                EqualsValueClause(
                                    InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("serializer"),
                                            GenericName(
                                                Identifier("Deserialize"))
                                            .WithTypeArgumentList(
                                                TypeArgumentList(
                                                    SingletonSeparatedList<TypeSyntax>(
                                                        QualifiedName(
                                                            QualifiedName(
                                                                QualifiedName(
                                                                    IdentifierName("Newtonsoft"),
                                                                    IdentifierName("Json")),
                                                                IdentifierName("Linq")),
                                                            IdentifierName("JObject")))))))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SingletonSeparatedList<ArgumentSyntax>(
                                                Argument(
                                                    IdentifierName("reader")))))))))),
                IfStatement(
                    PrefixUnaryExpression(
                        SyntaxKind.LogicalNotExpression,
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                IdentifierName("jObject"),
                                IdentifierName("TryGetValue")))
                        .WithArgumentList(
                            ArgumentList(
                                SeparatedList<ArgumentSyntax>(
                                    new SyntaxNodeOrToken[]{
                                        Argument(
                                            InvocationExpression(
                                                IdentifierName("nameof"))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SingletonSeparatedList<ArgumentSyntax>(
                                                        Argument(
                                                            MemberAccessExpression(
                                                                SyntaxKind.SimpleMemberAccessExpression,
                                                                typeIdentifier,
                                                                IdentifierName(discriminantName))))))),
                                        Token(SyntaxKind.CommaToken),
                                        Argument(
                                            DeclarationExpression(
                                                IdentifierName("var"),
                                                SingleVariableDesignation(
                                                    Identifier("discriminantToken"))))
                                        .WithRefKindKeyword(
                                            Token(SyntaxKind.OutKeyword))})))),
                    Block(
                        SingletonList<StatementSyntax>(
                            ThrowStatement(
                                ObjectCreationExpression(
                                    QualifiedName(
                                        QualifiedName(
                                            IdentifierName("Newtonsoft"),
                                            IdentifierName("Json")),
                                        IdentifierName("JsonSerializationException")))
                                .WithArgumentList(
                                    ArgumentList(
                                        SingletonSeparatedList<ArgumentSyntax>(
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
                                                                    "Missing required property ",
                                                                    "Missing required property ",
                                                                    TriviaList())),
                                                            Interpolation(
                                                                InvocationExpression(
                                                                    IdentifierName("nameof"))
                                                                .WithArgumentList(
                                                                    ArgumentList(
                                                                        SingletonSeparatedList<ArgumentSyntax>(
                                                                            Argument(
                                                                                MemberAccessExpression(
                                                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                                                    typeIdentifier,
                                                                                    IdentifierName(discriminantName))))))),
                                                            InterpolatedStringText()
                                                            .WithTextToken(
                                                                Token(
                                                                    TriviaList(),
                                                                    SyntaxKind.InterpolatedStringTextToken,
                                                                    " on Type ",
                                                                    " on Type ",
                                                                    TriviaList())),
                                                            Interpolation(
                                                                TypeOfExpression(
                                                                    typeIdentifier))})))))))))),
                LocalDeclarationStatement(
                    VariableDeclaration(
                        IdentifierName("var"))
                    .WithVariables(
                        SingletonSeparatedList<VariableDeclaratorSyntax>(
                            VariableDeclarator(
                                Identifier("discriminant"))
                            .WithInitializer(
                                EqualsValueClause(
                                    InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("discriminantToken"),
                                            GenericName(
                                                Identifier("ToObject"))
                                            .WithTypeArgumentList(
                                                TypeArgumentList(
                                                    SingletonSeparatedList<TypeSyntax>(
                                                        QualifiedName(typeIdentifier, IdentifierName("Discriminant")))))))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SingletonSeparatedList<ArgumentSyntax>(
                                                Argument(
                                                    IdentifierName("serializer")))))))))),
                ReturnStatement(
                    InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("jObject"),
                            IdentifierName("ToObject")))
                    .WithArgumentList(
                        ArgumentList(
                            SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Argument(
                                        ElementAccessExpression(
                                            IdentifierName("_cases"))
                                        .WithArgumentList(
                                            BracketedArgumentList(
                                                SingletonSeparatedList<ArgumentSyntax>(
                                                    Argument(
                                                        IdentifierName("discriminant")))))),
                                    Token(SyntaxKind.CommaToken),
                                    Argument(
                                        IdentifierName("serializer"))})))))),
                                    MethodDeclaration(
                                        PredefinedType(
                                            Token(SyntaxKind.BoolKeyword)),
                                        Identifier("CanConvert"))
                                    .WithModifiers(
                                        TokenList(
                                            new []{
                                                Token(SyntaxKind.PublicKeyword),
                                                Token(SyntaxKind.OverrideKeyword)}))
                                    .WithParameterList(
                                        ParameterList(
                                            SingletonSeparatedList<ParameterSyntax>(
                                                Parameter(
                                                    Identifier("objectType"))
                                                .WithType(
                                                    QualifiedName(
                                                        IdentifierName("System"),
                                                        IdentifierName("Type"))))))
                                    .WithExpressionBody(
                                        ArrowExpressionClause(
                                            InvocationExpression(
                                                MemberAccessExpression(
                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                    TypeOfExpression(
                                                        typeIdentifier),
                                                    IdentifierName("IsAssignableFrom")))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SingletonSeparatedList<ArgumentSyntax>(
                                                        Argument(
                                                            IdentifierName("objectType")))))))
                                    .WithSemicolonToken(
                                        Token(SyntaxKind.SemicolonToken)),

                        }));

                yield return
ClassDeclaration($"{typeName}DefaultJsonConverter")
.WithModifiers(
    TokenList(
        Token(SyntaxKind.InternalKeyword)))
.WithBaseList(
    BaseList(
        SingletonSeparatedList<BaseTypeSyntax>(
            SimpleBaseType(
                QualifiedName(
                    QualifiedName(
                        IdentifierName("Newtonsoft"),
                        IdentifierName("Json")),
                    IdentifierName("JsonConverter"))))))
.WithMembers(
    List<MemberDeclarationSyntax>(
        new MemberDeclarationSyntax[]{
                    PropertyDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Identifier("CanWrite"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            LiteralExpression(
                                SyntaxKind.FalseLiteralExpression)))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.VoidKeyword)),
                        Identifier("WriteJson"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("writer"))
                                    .WithType(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonWriter"))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("value"))
                                    .WithType(
                                        PredefinedType(
                                            Token(SyntaxKind.ObjectKeyword))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("serializer"))
                                    .WithType(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonSerializer")))})))
                    .WithBody(
                        Block(
                            SingletonList<StatementSyntax>(
                                ThrowStatement(
                                    ObjectCreationExpression(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("NotSupportedException")))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SingletonSeparatedList<ArgumentSyntax>(
                                                Argument(
                                                    LiteralExpression(
                                                        SyntaxKind.StringLiteralExpression,
                                                        Literal("Unnecessary because CanWrite is false. The type will skip the converter.")))))))))),
                    PropertyDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Identifier("CanRead"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            LiteralExpression(
                                SyntaxKind.FalseLiteralExpression)))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.ObjectKeyword)),
                        Identifier("ReadJson"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("reader"))
                                    .WithType(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonReader"))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("objectType"))
                                    .WithType(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("Type"))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("existingValue"))
                                    .WithType(
                                        PredefinedType(
                                            Token(SyntaxKind.ObjectKeyword))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("serializer"))
                                    .WithType(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonSerializer")))})))
                    .WithBody(
                        Block(
                            SingletonList<StatementSyntax>(
                                ThrowStatement(
                                    ObjectCreationExpression(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("NotSupportedException")))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SingletonSeparatedList<ArgumentSyntax>(
                                                Argument(
                                                    LiteralExpression(
                                                        SyntaxKind.StringLiteralExpression,
                                                        Literal("Unnecessary because CanRead is false. The type will skip the converter.")))))))))),
                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Identifier("CanConvert"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList<ParameterSyntax>(
                                Parameter(
                                    Identifier("objectType"))
                                .WithType(
                                    QualifiedName(
                                        IdentifierName("System"),
                                        IdentifierName("Type"))))))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            LiteralExpression(
                                SyntaxKind.TrueLiteralExpression)))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken))}));


            if (self.TypeArguments.Length == 0)
            {
                yield break;
            }

            var tupleElements = self.TypeArguments
                .Aggregate(new List<SyntaxNodeOrToken>(self.TypeArguments.Length * 2), (a, current) =>
                {
                    if (a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                    }
                    a.Add(TupleElement(
                        QualifiedName(
                            IdentifierName("System"),
                            IdentifierName("Type"))));
                    return a;
                });

            var cacheKeys = self.TypeArguments
                .Select((_, i) => i)
                .Aggregate(new List<SyntaxNodeOrToken>(self.TypeArguments.Length * 2), (a, i) =>
                {
                    if (a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                    }
                    a.Add(Argument(
                        ElementAccessExpression(
                            IdentifierName("arguments"))
                        .WithArgumentList(
                            BracketedArgumentList(
                                SingletonSeparatedList<ArgumentSyntax>(
                                    Argument(
                                        LiteralExpression(
                                            SyntaxKind.NumericLiteralExpression,
                                            Literal(i)))))))
                    );
                    return a;
                });

            var jsonConverterTypeArguments = self.TypeArguments.Length == 1
                ? (IEnumerable <SyntaxNodeOrToken>)new SyntaxNodeOrToken[] { Argument(IdentifierName("tuple")) }
                : self.TypeArguments
                .Select((_, i) => i + 1)
                .Aggregate(new List<SyntaxNodeOrToken>(self.TypeArguments.Length * 2), (a, i) =>
                {
                    if (a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                    }
                    a.Add(
                        Argument(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                IdentifierName("tuple"),
                                IdentifierName($"Item{i}")))
                    );
                    return a;
                });

            yield return ClassDeclaration($"{typeName}JsonConverter")
        .WithModifiers(
            TokenList(
                Token(SyntaxKind.InternalKeyword)))
        .WithBaseList(
            BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        QualifiedName(
                            QualifiedName(
                                IdentifierName("Newtonsoft"),
                                IdentifierName("Json")),
                            IdentifierName("JsonConverter"))))))
        .WithMembers(
            List<MemberDeclarationSyntax>(
                new MemberDeclarationSyntax[]{
                    FieldDeclaration(
                        VariableDeclaration(
                            QualifiedName(
                                QualifiedName(
                                    QualifiedName(
                                        IdentifierName("System"),
                                        IdentifierName("Collections")),
                                    IdentifierName("Concurrent")),
                                GenericName(
                                    Identifier("ConcurrentDictionary"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(
                                        SeparatedList<TypeSyntax>(
                                            new []{
                                                self.TypeArguments.Length == 1
                                                    ? (SyntaxNodeOrToken)QualifiedName(
                                                        IdentifierName("System"),
                                                        IdentifierName("Type"))
                                                    : TupleType(
                                                        SeparatedList<TupleElementSyntax>(tupleElements)),
                                                Token(SyntaxKind.CommaToken),
                                                QualifiedName(
                                                    QualifiedName(
                                                        IdentifierName("Newtonsoft"),
                                                        IdentifierName("Json")),
                                                    IdentifierName("JsonConverter"))
                                            })))))
                        .WithVariables(
                            SingletonSeparatedList<VariableDeclaratorSyntax>(
                                VariableDeclarator(
                                    Identifier("_cache"))
                                .WithInitializer(
                                    EqualsValueClause(
                                        ObjectCreationExpression(
                                            QualifiedName(
                                                QualifiedName(
                                                    QualifiedName(
                                                        IdentifierName("System"),
                                                        IdentifierName("Collections")),
                                                    IdentifierName("Concurrent")),
                                                GenericName(
                                                    Identifier("ConcurrentDictionary"))
                                                .WithTypeArgumentList(
                                                    TypeArgumentList(
                                                        SeparatedList<TypeSyntax>(
                                                            new SyntaxNodeOrToken[]{
                                                                self.TypeArguments.Length == 1
                                                                    ? (SyntaxNodeOrToken)QualifiedName(
                                                                        IdentifierName("System"),
                                                                        IdentifierName("Type"))
                                                                    : TupleType(
                                                                        SeparatedList<TupleElementSyntax>(tupleElements)),
                                                                Token(SyntaxKind.CommaToken),
                                                                QualifiedName(
                                                                    QualifiedName(
                                                                        IdentifierName("Newtonsoft"),
                                                                        IdentifierName("Json")),
                                                                    IdentifierName("JsonConverter"))
                                                            })))))
                                        .WithArgumentList(
                                            ArgumentList()))))))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PrivateKeyword),
                                Token(SyntaxKind.ReadOnlyKeyword)})),

                    PropertyDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Identifier("CanWrite"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            LiteralExpression(
                                SyntaxKind.FalseLiteralExpression)))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken)),

                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.VoidKeyword)),
                        Identifier("WriteJson"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("writer"))
                                    .WithType(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonWriter"))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("value"))
                                    .WithType(
                                        PredefinedType(
                                            Token(SyntaxKind.ObjectKeyword))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("serializer"))
                                    .WithType(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonSerializer")))})))
                    .WithBody(
                        Block(
                            SingletonList<StatementSyntax>(
                                ThrowStatement(
                                    ObjectCreationExpression(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("NotSupportedException")))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SingletonSeparatedList<ArgumentSyntax>(
                                                Argument(
                                                    LiteralExpression(
                                                        SyntaxKind.StringLiteralExpression,
                                                        Literal("Unnecessary because CanWrite is false. The type will skip the converter.")))))))))),

                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.ObjectKeyword)),
                        Identifier("ReadJson"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SeparatedList<ParameterSyntax>(
                                new SyntaxNodeOrToken[]{
                                    Parameter(
                                        Identifier("reader"))
                                    .WithType(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonReader"))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("objectType"))
                                    .WithType(
                                        QualifiedName(
                                            IdentifierName("System"),
                                            IdentifierName("Type"))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("existingValue"))
                                    .WithType(
                                        PredefinedType(
                                            Token(SyntaxKind.ObjectKeyword))),
                                    Token(SyntaxKind.CommaToken),
                                    Parameter(
                                        Identifier("serializer"))
                                    .WithType(
                                        QualifiedName(
                                            QualifiedName(
                                                IdentifierName("Newtonsoft"),
                                                IdentifierName("Json")),
                                            IdentifierName("JsonSerializer")))})))
                    .WithBody(
                        Block(
                            SingletonList<StatementSyntax>(
                                ReturnStatement(
                                    InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            InvocationExpression(
                                                IdentifierName("ResolveJsonConverter"))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SingletonSeparatedList<ArgumentSyntax>(
                                                        Argument(
                                                            IdentifierName("objectType"))))),
                                            IdentifierName("ReadJson")))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SeparatedList<ArgumentSyntax>(
                                                new SyntaxNodeOrToken[]{
                                                    Argument(
                                                        IdentifierName("reader")),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        IdentifierName("objectType")),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        IdentifierName("existingValue")),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        IdentifierName("serializer"))}))))))),

                    MethodDeclaration(
                        QualifiedName(
                            QualifiedName(
                                IdentifierName("Newtonsoft"),
                                IdentifierName("Json")),
                            IdentifierName("JsonConverter")),
                        Identifier("ResolveJsonConverter"))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PrivateKeyword)))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList<ParameterSyntax>(
                                Parameter(
                                    Identifier("objectType"))
                                .WithType(
                                    QualifiedName(
                                        IdentifierName("System"),
                                        IdentifierName("Type"))))))
                    .WithBody(
                        Block(
                            LocalDeclarationStatement(
                                VariableDeclaration(
                                    IdentifierName("var"))
                                .WithVariables(
                                    SingletonSeparatedList<VariableDeclaratorSyntax>(
                                        VariableDeclarator(
                                            Identifier("arguments"))
                                        .WithInitializer(
                                            EqualsValueClause(
                                                MemberAccessExpression(
                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                    InvocationExpression(
                                                        MemberAccessExpression(
                                                            SyntaxKind.SimpleMemberAccessExpression,
                                                            IdentifierName("objectType"),
                                                            IdentifierName("GetTypeInfo"))),
                                                    IdentifierName("GenericTypeArguments"))))))),
                            ReturnStatement(
                                InvocationExpression(
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName("_cache"),
                                        IdentifierName("GetOrAdd")))
                                .WithArgumentList(
                                    ArgumentList(
                                        SeparatedList<ArgumentSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                Argument(
                                                    TupleExpression(
                                                        SeparatedList<ArgumentSyntax>(cacheKeys))),
                                                Token(SyntaxKind.CommaToken),
                                                Argument(
                                                    SimpleLambdaExpression(
                                                        Parameter(
                                                            Identifier("tuple")),
                                                        CastExpression(
                                                            QualifiedName(
                                                                QualifiedName(
                                                                    IdentifierName("Newtonsoft"),
                                                                    IdentifierName("Json")),
                                                                IdentifierName("JsonConverter")),
                                                            InvocationExpression(
                                                                MemberAccessExpression(
                                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                                    QualifiedName(
                                                                        IdentifierName("System"),
                                                                        IdentifierName("Activator")),
                                                                    IdentifierName("CreateInstance")))
                                                            .WithArgumentList(
                                                                ArgumentList(
                                                                    SingletonSeparatedList<ArgumentSyntax>(
                                                                        Argument(
                                                                            InvocationExpression(
                                                                                MemberAccessExpression(
                                                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                                                    TypeOfExpression(
                                                                                        TypeIdentifier($"{typeName}JsonConverter", true)),
                                                                                    IdentifierName("MakeGenericType")))
                                                                            .WithArgumentList(
                                                                                ArgumentList(
                                                                                    SeparatedList<ArgumentSyntax>(jsonConverterTypeArguments))))))))))
                                            })))))),

                    MethodDeclaration(
                        PredefinedType(
                            Token(SyntaxKind.BoolKeyword)),
                        Identifier("CanConvert"))
                    .WithModifiers(
                        TokenList(
                            new []{
                                Token(SyntaxKind.PublicKeyword),
                                Token(SyntaxKind.OverrideKeyword)}))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList<ParameterSyntax>(
                                Parameter(
                                    Identifier("objectType"))
                                .WithType(
                                    QualifiedName(
                                        IdentifierName("System"),
                                        IdentifierName("Type"))))))
                    .WithExpressionBody(
                        ArrowExpressionClause(
                            BinaryExpression(
                                SyntaxKind.LogicalAndExpression,
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("objectType"),
                                    IdentifierName("IsGenericType")),
                                BinaryExpression(
                                    SyntaxKind.EqualsExpression,
                                    InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("objectType"),
                                            IdentifierName("GetGenericTypeDefinition"))),
                                    TypeOfExpression(
                                        TypeIdentifier(typeName, true))))))
                    .WithSemicolonToken(
                        Token(SyntaxKind.SemicolonToken))
                }));

        }


    }
}
