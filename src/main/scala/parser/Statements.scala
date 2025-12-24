package tpagu.compiler.parser

import tpagu.compiler.lexer.Token
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.CompilerError
import tpagu.compiler.lexer.TokenInfo

def expressionStatement: ParseRule[AstExt] = (for {
  expr <- expression
  _ <- Just(Token.Semicolon)
} yield Right(AstExtKind.ExprStatement(expr))).withSpan

def emptyStatement: ParseRule[AstExt] = (for {
  _ <- Just(Token.Semicolon)
} yield Right(AstExtKind.Nothing)).withSpan

def statement: ParseRule[AstExt] =
  emptyStatement <|> functionDefinition <|> blockStatement <|> declaration <|> expressionStatement <|> returnStatement

def onePointer: ParseRule[Pointer] =
  (for {
    _ <- Just(
      Token.Times
    )
    quals <- typeQualifier.many
  } yield Right(Pointer(quals)))

def declarator: ParseRule[Declarator] =
  (for {
    ptr <- onePointer.many
    decl <- directDeclarator
  } yield Right(Declarator(ptr, decl)))

def typeSpecifier: ParseRule[DeclarationSpecifier] =
  (OneOf(List(Token.Int, Token.Long)).map(tok =>
      tok.token match {
        case Token.Int => Right(IntSpec())
        case Token.Long => Right(LongSpec())
        case _ => throw RuntimeException("This branch shouldnt be reachable in type specifier. Check the oneOf and the branches you handle match.")
      }
  )).named("type specifier")

def typeQualifier: ParseRule[TypeQualifier] =
  (for {
    _ <- Just(Token.Const)
  } yield Right(TypeQualifier.Const()))

def declarationSpecifier: ParseRule[DeclarationSpecifier] = {
  // this map upcast is needed since typeQualifier returns TypeSpecifier but for the Or parser combinator these must be the exact same type
  (typeSpecifier <|> typeQualifier.map(a => Right(a: DeclarationSpecifier)))
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
    specs <- declarationSpecifier.repeat(
      1 until Int.MaxValue
    ) // at least one type specifier is needed for a declaration.
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
  val emptyParamList = for {
    name <- identifier.maybe
    _ <- Just(Token.OpenParen)
    _ <- Just(Token.CloseParen)
  } yield Right(DirectDeclarator.Function(extractName(name), Nil))

  val nonEmptyParamList = for {
    // case AstExt(AstExtKind.Identifier(name),_) <- identifier.maybe
    name <- identifier.maybe
    _ <- Just(Token.OpenParen)
    params <- listOf(parameterDeclaration)
    _ <- Just(Token.CloseParen)
  } yield Right(DirectDeclarator.Function(extractName(name), params))

  (emptyParamList <|> nonEmptyParamList)
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
    declSpecs <- declarationSpecifier.repeat(1 until Int.MaxValue)
    decl <- functionDeclarator
    body <- blockStatement
  } yield Right(
    AstExtKind
      .FunctionDefinition(Declaration(declSpecs, Declarator(Nil, decl)), body)
  )).withSpan
    .named("function definition")

def translationUnit: ParseRule[AstExt] =
  statement.many
    .map(stmts => Right(AstExtKind.TranslationUnit(stmts)))
    .withSpan
    .named("translation unit") <* Just(Token.EOF)

def returnStatement: ParseRule[AstExt] = (for {
  _ <- Just(Token.Return)
  e <- expression
  _ <- Just(Token.Semicolon)
} yield Right(AstExtKind.Return(e))).withSpan
