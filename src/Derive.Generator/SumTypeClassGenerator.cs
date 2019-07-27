using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Linq;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Derive.Generator
{
    public static class SumTypeClassGenerator
    {
        public static SyntaxList<MemberDeclarationSyntax> CreateSyntax(ClassDeclarationSyntax original, string discriminantName, bool generateJsonConverter)
        {
            var typeName = original.Identifier.ValueText;
            var cases = original.Members
                .OfType<ClassDeclarationSyntax>()
                .Select(c => c.Identifier.ValueText)
                .ToList();

            string Camel(string name) => $"@{name[0].ToString().ToLower()}{name.Substring(1)}";

            IEnumerable<MemberDeclarationSyntax> Members()
            {
                yield return ConstructorDeclaration(
                            Identifier(typeName))
                        .WithModifiers(
                            TokenList(
                                Token(SyntaxKind.PrivateKeyword)))
                        .WithBody(Block());

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
                                                IdentifierName("T")}))))));
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
                        IdentifierName("T"),
                        Identifier("Match"))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithTypeParameterList(
                        TypeParameterList(
                            SingletonSeparatedList<TypeParameterSyntax>(
                                TypeParameter(
                                    Identifier("T")))))
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

                foreach (var caseName in cases)
                {
                    var caseClassAttributes = new List<AttributeSyntax>();
                    if (generateJsonConverter)
                    {
                        caseClassAttributes.Add(
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
                                                QualifiedName(
                                                    IdentifierName(typeName),
                                                    IdentifierName("DefaultConverter")))))))
                        );
                    }

                    yield return ClassDeclaration(caseName)
                        .WithAttributeLists(
                            List<AttributeListSyntax>(
                                SingletonList(
                                    AttributeList(
                                        SeparatedList(caseClassAttributes)))))
                        .WithModifiers(
                            TokenList(
                                new[]{
                                    Token(SyntaxKind.PublicKeyword),
                                    Token(SyntaxKind.SealedKeyword),
                                    Token(SyntaxKind.PartialKeyword)}))
                        .WithBaseList(
                            BaseList(
                                SingletonSeparatedList<BaseTypeSyntax>(
                                    SimpleBaseType(
                                        IdentifierName(typeName)))))
                        .WithMembers(
                            SingletonList<MemberDeclarationSyntax>(
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
                                    Token(SyntaxKind.SemicolonToken))));
                }

                if (generateJsonConverter)
                {
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
                                            IdentifierName("Discriminant"),
                                            IdentifierName(caseName)),
                                        Token(SyntaxKind.CommaToken),
                                        TypeOfExpression(
                                            IdentifierName(caseName))}))
                        );
                        return a;
                    });

                    yield return ClassDeclaration("JsonConverter")
                        .WithModifiers(
                            TokenList(
                                Token(SyntaxKind.PublicKeyword)))
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
                                                                IdentifierName("Discriminant"),
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
                                                                                IdentifierName("Discriminant"),
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
                            IdentifierName("Type")),
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
                                                                IdentifierName(typeName),
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
                                                                                    IdentifierName(typeName),
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
                                                                    IdentifierName(typeName)))})))))))))),
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
                                                        IdentifierName("Discriminant"))))))
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
                                                        IdentifierName(typeName)),
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
        ClassDeclaration("DefaultConverter")
        .WithModifiers(
            TokenList(
                Token(SyntaxKind.PublicKeyword)))
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
                                        IdentifierName("Type")),
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
                }
            }

            var attributes = new List<AttributeSyntax>();
            if (generateJsonConverter)
            {
                attributes.Add(
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
                                        QualifiedName(
                                            IdentifierName(typeName),
                                            IdentifierName("JsonConverter")))))))
                );
            }

            return SingletonList<MemberDeclarationSyntax>(ClassDeclaration(typeName)
                .WithAttributeLists(
                    List<AttributeListSyntax>(
                        SingletonList(
                            AttributeList(
                                SeparatedList(attributes)))))
                .WithModifiers(
                    TokenList(
                        new[]{
                            Token(SyntaxKind.AbstractKeyword),
                            Token(SyntaxKind.PartialKeyword)}))
                .WithMembers(
                    List<MemberDeclarationSyntax>(Members())));
        }


    }
}
