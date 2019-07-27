using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Linq;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Derive.Generator
{
    public static class SumTypeStructGenerator
    {
        public static SyntaxList<MemberDeclarationSyntax> CreateSyntax(StructDeclarationSyntax original, string discriminantName)
        {
            var typeName = original.Identifier.ValueText;
            var cases = original.Members
                .OfType<TypeDeclarationSyntax>()
                .Select(c => c.Identifier.ValueText)
                .ToList();

            string Camel(string name) => $"@{name[0].ToString().ToLower()}{name.Substring(1)}";

            IEnumerable<MemberDeclarationSyntax> Members()
            {
                yield return PropertyDeclaration(
                        IdentifierName("Discriminant"),
                        Identifier(discriminantName))
                    .WithModifiers(
                        TokenList(
                            new[]{
                                Token(SyntaxKind.PublicKeyword)}))
                    .WithAccessorList(
                        AccessorList(
                            SingletonList<AccessorDeclarationSyntax>(
                                AccessorDeclaration(
                                    SyntaxKind.GetAccessorDeclaration)
                                .WithSemicolonToken(
                                    Token(SyntaxKind.SemicolonToken)))));

                yield return FieldDeclaration(
                    VariableDeclaration(
                        IdentifierName("Union"))
                    .WithVariables(
                        SingletonSeparatedList<VariableDeclaratorSyntax>(
                            VariableDeclarator(
                                Identifier("union")))))
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.PrivateKeyword)));

                foreach (var caseName in cases)
                {
                    var camel = Camel(caseName);
                    yield return ConstructorDeclaration(
                        Identifier(typeName))
                    .WithModifiers(
                        TokenList(
                            Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList<ParameterSyntax>(
                                Parameter(
                                    Identifier(camel))
                                .WithType(
                                    IdentifierName(caseName)))))
                    .WithBody(
                        Block(
                            ExpressionStatement(
                                AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    IdentifierName("union"),
                                    ObjectCreationExpression(
                                        IdentifierName("Union"))
                                    .WithInitializer(
                                        InitializerExpression(
                                            SyntaxKind.ObjectInitializerExpression,
                                            SingletonSeparatedList<ExpressionSyntax>(
                                                AssignmentExpression(
                                                    SyntaxKind.SimpleAssignmentExpression,
                                                    IdentifierName($"Item{caseName}"),
                                                    IdentifierName(camel))))))),
                            ExpressionStatement(
                                AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    IdentifierName(discriminantName),
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName("Discriminant"),
                                        IdentifierName(caseName))))));
                }

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
                            GenericName(Identifier($"Func{memberName}"))
                            .WithTypeArgumentList(
                                TypeArgumentList(
                                    SeparatedList<TypeSyntax>(
                                        new SyntaxNodeOrToken[]{
                                            IdentifierName("T")})))));
                    return a;
                });

                var matchCases = cases.Aggregate(new List<SwitchSectionSyntax>(), (a, memberName) =>
                {
                    a.Add(
                        SwitchSection()
                            .WithLabels(
                                SingletonList<SwitchLabelSyntax>(
                                    CaseSwitchLabel(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("Discriminant"),
                                            IdentifierName(memberName)))))
                            .WithStatements(
                                SingletonList<StatementSyntax>(
                                    ReturnStatement(
                                        InvocationExpression(
                                            IdentifierName(Camel(memberName)))
                                        .WithArgumentList(
                                            ArgumentList(
                                                SingletonSeparatedList<ArgumentSyntax>(
                                                    Argument(
                                                        MemberAccessExpression(
                                                            SyntaxKind.SimpleMemberAccessExpression,
                                                            IdentifierName("union"),
                                                            IdentifierName($"Item{memberName}")))))))))
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
                                                        IdentifierName(discriminantName)),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        InterpolatedStringExpression(
                                                            Token(SyntaxKind.InterpolatedVerbatimStringStartToken))
                                                        .WithContents(
                                                            List<InterpolatedStringContentSyntax>(
                                                                new InterpolatedStringContentSyntax[]{
                                                                    InterpolatedStringText()
                                                                    .WithTextToken(
                                                                        Token(
                                                                            TriviaList(),
                                                                            SyntaxKind.InterpolatedStringTextToken,
                                                                            "Uninitialized ",
                                                                            "Uninitialized ",
                                                                            TriviaList())),
                                                                    Interpolation(
                                                                        InvocationExpression(
                                                                            IdentifierName("GetType")))})))
                                                })))))));

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
                                    IdentifierName(discriminantName))
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
                            IdentifierName($"Action{memberName}")));
                    return a;
                });

                var switchCases = cases.Aggregate(new List<SwitchSectionSyntax>(), (a, memberName) =>
                {
                    a.Add(
                        SwitchSection()
                            .WithLabels(
                                SingletonList<SwitchLabelSyntax>(
                                    CaseSwitchLabel(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("Discriminant"),
                                            IdentifierName(memberName)))))
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
                                                            MemberAccessExpression(
                                                                SyntaxKind.SimpleMemberAccessExpression,
                                                                IdentifierName("union"),
                                                                IdentifierName($"Item{memberName}"))))))),
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
                                                        IdentifierName(discriminantName)),
                                                    Token(SyntaxKind.CommaToken),
                                                    Argument(
                                                        InterpolatedStringExpression(
                                                            Token(SyntaxKind.InterpolatedVerbatimStringStartToken))
                                                        .WithContents(
                                                            List<InterpolatedStringContentSyntax>(
                                                                new InterpolatedStringContentSyntax[]{
                                                                    InterpolatedStringText()
                                                                    .WithTextToken(
                                                                        Token(
                                                                            TriviaList(),
                                                                            SyntaxKind.InterpolatedStringTextToken,
                                                                            "Uninitialized ",
                                                                            "Uninitialized ",
                                                                            TriviaList())),
                                                                    Interpolation(
                                                                        InvocationExpression(
                                                                            IdentifierName("GetType")))})))
                                                })))))));

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
                                    IdentifierName(discriminantName))
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

                var unionFields = cases.Aggregate(new List<FieldDeclarationSyntax>(), (a, caseName) =>
                {
                    a.Add(
                        FieldDeclaration(
                                VariableDeclaration(
                                    IdentifierName(caseName))
                                .WithVariables(
                                    SingletonSeparatedList<VariableDeclaratorSyntax>(
                                        VariableDeclarator(
                                            Identifier($"Item{caseName}")))))
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
                                                        IdentifierName("InteropServices")),
                                                    IdentifierName("FieldOffset")))
                                            .WithArgumentList(
                                                AttributeArgumentList(
                                                    SingletonSeparatedList<AttributeArgumentSyntax>(
                                                        AttributeArgument(
                                                            LiteralExpression(
                                                                SyntaxKind.NumericLiteralExpression,
                                                                Literal(0))))))))))
                            .WithModifiers(
                                TokenList(
                                    Token(SyntaxKind.PublicKeyword)))
                    );
                    return a;
                });

                yield return StructDeclaration("Union")
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
                                                IdentifierName("InteropServices")),
                                            IdentifierName("StructLayout")))
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
                                                                IdentifierName("InteropServices")),
                                                            IdentifierName("LayoutKind")),
                                                        IdentifierName("Explicit"))))))))))
                    .WithMembers(
                        List<MemberDeclarationSyntax>(unionFields));

                foreach (var caseName in cases)
                {
                    var camel = Camel(caseName);
                    yield return ConversionOperatorDeclaration(
                        Token(SyntaxKind.ImplicitKeyword),
                        IdentifierName(typeName))
                        .WithModifiers(
                            TokenList(
                                new[]{
                                    Token(SyntaxKind.PublicKeyword),
                                    Token(SyntaxKind.StaticKeyword)}))
                        .WithParameterList(
                            ParameterList(
                                SingletonSeparatedList<ParameterSyntax>(
                                    Parameter(
                                        Identifier("value"))
                                    .WithType(
                                        IdentifierName(caseName)))))
                        .WithExpressionBody(
                            ArrowExpressionClause(
                                ObjectCreationExpression(
                                    IdentifierName(typeName))
                                .WithArgumentList(
                                    ArgumentList(
                                        SingletonSeparatedList<ArgumentSyntax>(
                                        Argument(
                                            IdentifierName("value"))
                                        .WithNameColon(
                                            NameColon(
                                                IdentifierName(camel))))))))
                        .WithSemicolonToken(
                            Token(SyntaxKind.SemicolonToken));

                    yield return MethodDeclaration(
                            IdentifierName(typeName),
                            Identifier($"Create{caseName}"))
                        .WithModifiers(
                            TokenList(
                                new[]{
                                    Token(SyntaxKind.PublicKeyword),
                                    Token(SyntaxKind.StaticKeyword)}))
                        .WithParameterList(
                            ParameterList(
                                SingletonSeparatedList<ParameterSyntax>(
                                    Parameter(
                                        Identifier("value"))
                                    .WithType(
                                        IdentifierName(caseName)))))
                        .WithExpressionBody(
                            ArrowExpressionClause(
                                ObjectCreationExpression(
                                    IdentifierName(typeName))
                                .WithArgumentList(
                                    ArgumentList(
                                        SingletonSeparatedList<ArgumentSyntax>(
                                            Argument(
                                                IdentifierName("value"))
                                            .WithNameColon(
                                                NameColon(
                                                    IdentifierName(camel))))))))
                        .WithSemicolonToken(
                            Token(SyntaxKind.SemicolonToken));

                    yield return DelegateDeclaration(
                            PredefinedType(
                                Token(SyntaxKind.VoidKeyword)),
                            Identifier($"Action{caseName}"))
                        .WithModifiers(
                            TokenList(
                                Token(SyntaxKind.PublicKeyword)))
                        .WithParameterList(
                            ParameterList(
                                SingletonSeparatedList<ParameterSyntax>(
                                    Parameter(
                                        Identifier(camel))
                                    .WithModifiers(
                                        TokenList(
                                            Token(SyntaxKind.InKeyword)))
                                    .WithType(
                                        IdentifierName(caseName)))));

                    yield return DelegateDeclaration(
                            IdentifierName("T"),
                            Identifier($"Func{caseName}"))
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
                                SingletonSeparatedList<ParameterSyntax>(
                                    Parameter(
                                        Identifier(camel))
                                    .WithModifiers(
                                        TokenList(
                                            Token(SyntaxKind.InKeyword)))
                                    .WithType(
                                        IdentifierName(caseName)))));
                }
            }

            return SingletonList<MemberDeclarationSyntax>(StructDeclaration(typeName)
                .WithModifiers(
                    TokenList(
                        new[]{
                            Token(SyntaxKind.PartialKeyword)}))
                .WithMembers(
                    List<MemberDeclarationSyntax>(Members())));
        }


    }
}