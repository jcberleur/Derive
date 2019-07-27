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
        public static SyntaxList<MemberDeclarationSyntax> CreateSyntax(ClassDeclarationSyntax original, string discriminantName)
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
                    if(a.Count != 0)
                    {
                        a.Add(Token(SyntaxKind.CommaToken));
                    }
                    a.Add(EnumMemberDeclaration(Identifier(memberName)));
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
                    yield return ClassDeclaration(caseName)
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
            }

            return SingletonList<MemberDeclarationSyntax>(ClassDeclaration(typeName)
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
