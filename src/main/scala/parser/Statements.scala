package tpagu.compiler.parser

import tpagu.compiler.lexer.Token
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.CompilerError
import tpagu.compiler.lexer.TokenInfo
import tpagu.compiler.typeChecker.getNameOfDeclarator
import tpagu.compiler.Spanned

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
    suffixes <- declaratorSuffix.many
  } yield Right(Declarator(ptr, decl, suffixes)))

// needed because Just() won't work with class tokens (not singletons)
def typedefName: ParseRule[TypeSpecifier.TypedefName] = {
  new ParseRule[TypeSpecifier.TypedefName] {
    def parse(
        lexer: Lexer
    ): Either[CompilerError, (TypeSpecifier.TypedefName, Lexer)] = {
      val res = lexer.nextToken()
      res match {
        case Left(err) => Left(err)
        // TODO: Handle string and char literals, including escaping, etc.
        case Right(TokenInfo(Token.TypedefName(name), span), lexer2) =>
          Right(TypeSpecifier.TypedefName(name), lexer2)
        case Right(TokenInfo(_, span), _) =>
          Left(parserError("Could not parse typedef name.", span))
      }
    }
  }.named("typedefName")
}

def typeSpecifier: ParseRule[DeclarationSpecifier] =
  (OneOf(List(Token.Int, Token.Long))
    .map(tok =>
      tok.token match {
        case Token.Int  => Right(TypeSpecifier.Int)
        case Token.Long => Right(TypeSpecifier.Long)
        case _ =>
          throw RuntimeException(
            "This branch shouldnt be reachable in type specifier. Check the oneOf and the branches you handle match."
          )
      }
      )) <|>
    typedefName.map(x => Right(x:DeclarationSpecifier))
    .named("type specifier")

def typeQualifier: ParseRule[TypeQualifier] =
  (for {
    _ <- Just(Token.Const)
  } yield Right(TypeQualifier.Const))

def storageClassSpecifier: ParseRule[DeclarationSpecifier] =
  (
    for {
      _ <- Just(Token.Typedef)
    } yield Right(StorageClassSpecifier.Typedef)
  )

def declarationSpecifier: ParseRule[DeclarationSpecifier] = {
  // this map upcast is needed since typeQualifier returns TypeSpecifier but for the Or parser combinator these must be the exact same type
  (typeSpecifier <|> typeQualifier.map(a => Right(a: DeclarationSpecifier)) <|> storageClassSpecifier)
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
    .withUpdatedLexer((output, lexer) => {
      output.node match {
        case AstExtKind.DeclarationList(specs, decl) => {
          /** typedef handling within the parser:
           *  This is where we give feedback to the parser so that it knows to now treat any 
           *  example of these names as a Typename and then pass the token back to the parser
           *  as TypedefNames.
           */
          if (specs.contains(StorageClassSpecifier.Typedef)) {
            val names = decl.map(d => getNameOfDeclarator(d._1)).filter(_.isDefined).map(_.get)
            lexer.withTypeNames(lexer.typeNames ++ names)
          } else {
            lexer
          }
        }
        case _ => throw RuntimeException("Unreachable code, declaration rule should always yield a DeclarationList.")
      }
    })
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

def parameterSuffix:ParseRule[DeclaratorSuffix] = {
  val emptyParamList = for {
    _ <- Just(Token.OpenParen)
    _ <- Just(Token.CloseParen)
  } yield Right(DeclaratorSuffix.Params(Nil))

  val nonEmptyParamList = for {
    _ <- Just(Token.OpenParen)
    params <- listOf(parameterDeclaration)
    _ <- Just(Token.CloseParen)
  } yield Right(DeclaratorSuffix.Params(params))

  (emptyParamList <|> nonEmptyParamList)
}

def declaratorSuffix:ParseRule[DeclaratorSuffix] = 
  (parameterSuffix)

def directDeclarator: ParseRule[DirectDeclarator] =
  val paranthesizedDeclaration = for {
    _ <- Just(Token.OpenParen)
    d <- declarator
    _ <- Just(Token.CloseParen)
  } yield Right(DirectDeclarator.InnerDeclarator(d))

  val variableDeclarator = for {
    case name <- identifier.maybe
  } yield Right(DirectDeclarator.Variable(extractName(name)))

  //TODO: Is it really correct to let anonymous declarators
  //like this? this variableDeclarator can always eat up nothing
  //so this can be sketchy...
  (paranthesizedDeclaration <|> variableDeclarator )


def blockStatement: ParseRule[AstExt] = (for {
  lexer <- GetLexer()
  _ <- Just(Token.OpenBrace)
  statements <- statement.many
  _ <- Just(Token.CloseBrace)
  _ <- SetLexer(lexer)
} yield Right(AstExtKind.Block(statements))).withSpan

def functionDefinition: ParseRule[AstExt] =
  (for {
    declSpecs <- declarationSpecifier.repeat(1 until Int.MaxValue)
    decl <- declarator
    body <- blockStatement
  } yield Right(
    AstExtKind
      .FunctionDefinition(Declaration(declSpecs,decl), body)
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
