package tpagu.compiler.parser

import tpagu.compiler.lexer.Token
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.CompilerError
import tpagu.compiler.lexer.TokenInfo

def statement: ParseRule[AstExt] =
  functionDefinition <|> blockStatement <|> declaration

def declarator: ParseRule[Declarator] =
  directDeclarator.map(decl => Right(Declarator(Nil, decl)))

def typeSpecifier: ParseRule[DeclarationSpecifier] =
  new ParseRule[DeclarationSpecifier] {
    def parse(lexer: Lexer): Either[CompilerError, Out[DeclarationSpecifier]] =
      lexer.nextToken() match {
        case Left(err) => Left(err)
        case Right(TokenInfo(Token.TypeName(t), span), lexer2) =>
          Right(DeclarationSpecifier.TSpecifier(t), lexer2)
        case Right(TokenInfo(tok, span), _) =>
          Left(
            CompilerError(
              s"Expected type specifier, instead got: ${tok.toString}",
              span
            )
          )
      }
  }.named("type specifier")

def declarationSpecifier: ParseRule[DeclarationSpecifier] = {
  typeSpecifier
}

def initDeclarator: ParseRule[(Declarator, Option[AstExt])] = {
  val withInit = for {
    decl <- declarator
    _ <- Just(Token.Assign)
    init <- expression
  } yield Right((decl, Some(init): Option[AstExt]))

  val withoutInit = declarator.map(decl => Right((decl, None: Option[AstExt])))

  (withInit <|> withoutInit).named("init declarator")
}

def declaration: ParseRule[AstExt] =
  (for {
    specs <- declarationSpecifier.many
    decl <- listOf(initDeclarator)
    _ <- Just(Token.Semicolon)
  } yield Right(AstExtKind.DeclarationList(specs, decl))).withSpan
    .named("declaration")

def parameterDeclaration: ParseRule[Declaration] =
  (for {
    specs <- declarationSpecifier.many
    decl <- declarator
  } yield Right(Declaration(specs, decl))).named("parameter declaration")

def extractName(nameOpt: Option[AstExt]): Option[String] = nameOpt.map {
  case AstExt(AstExtKind.Identifier(x), _) => x
  case _                                   => ???
}

def functionDeclarator: ParseRule[DirectDeclarator] =
  (for {
    // case AstExt(AstExtKind.Identifier(name),_) <- identifier.maybe
    name <- identifier.maybe
    _ <- Just(Token.OpenParen)
    params <- listOf(parameterDeclaration)
    _ <- Just(Token.CloseParen)
  } yield Right(DirectDeclarator.Function(extractName(name), params)))
    .named("function declarator")

def directDeclarator: ParseRule[DirectDeclarator] =
  val paranthesizedDeclaration = for {
    _ <- Just(Token.OpenParen)
    d <- declarator
    _ <- Just(Token.CloseParen)
  } yield Right(DirectDeclarator.InnerDeclarator(d))

  val variableDeclarator = for {
    case name <- identifier.maybe
  } yield Right(DirectDeclarator.Variable(extractName(name)))

  (functionDeclarator <|> variableDeclarator <|> paranthesizedDeclaration)
    .named("direct declarator")

def blockStatement: ParseRule[AstExt] = (for {
  _ <- Just(Token.OpenBrace)
  statements <- statement.many
  _ <- Just(Token.CloseBrace)
} yield Right(AstExtKind.Block(statements))).withSpan

def functionDefinition: ParseRule[AstExt] =
  (for {
    declSpecs <- declarationSpecifier.many
    decl <- functionDeclarator
    body <- blockStatement
  } yield Right(AstExtKind.FunctionDefinition(Declaration(declSpecs, Declarator(Nil, decl)), body)))
    .withSpan
    .named("function definition")
