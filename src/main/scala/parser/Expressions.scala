package tpagu.compiler.parser
import tpagu.compiler.lexer.{Lexer, Token, TokenInfo}
import tpagu.compiler.{CompilerError, Spanned}
import tpagu.compiler.Span

def parserError(msg:String, span: Span):CompilerError = CompilerError(msg, span)

def expression: ParseRule[AstExt] =
  primaryExpression

def literal: ParseRule[AstExt] = {
  new ParseRule[AstExt] {
    def parse(
        lexer: Lexer
    ): Either[CompilerError, (Spanned[AstExtKind], Lexer)] = {
      val res = lexer.nextToken()
      res match {
        case Left(err) => Left(err)
        // TODO: Handle string and char literals, including escaping, etc.
        case Right(TokenInfo(Token.Number(num), span), lexer2) => Right(Spanned(AstExtKind.IntLiteral(num.toInt), span), lexer2)
        case Right(TokenInfo(_, span), _) => Left(parserError("Could not parse literal.", span))
      }
    }
  }
}

def identifier: ParseRule[AstExt] = {
  new ParseRule[AstExt] {
    def parse(
        lexer: Lexer
    ): Either[CompilerError, (Spanned[AstExtKind], Lexer)] = {
      val res = lexer.nextToken()
      res match {
        case Left(err) => Left(err)
        // TODO: Handle string and char literals, including escaping, etc.
        case Right(TokenInfo(Token.Identifier(name), span), lexer2) => Right(Spanned(AstExtKind.Identifier(name), span), lexer2)
        case Right(TokenInfo(_, span), _) => Left(parserError("Could not parse identifier.", span))
      }
    }
  }
}

// atom -> ( expr ) | literal | identifier
def atom: ParseRule[AstExt] = {
  val exprParser = (for {
    _ <- Just(Token.OpenParen)
    e <- expression
    _ <- Just(Token.CloseParen)
  } yield Right(e))
  exprParser <|> literal <|> identifier
}


def postfixOperation: ParseRule[PostfixOp] =
  //TODO add lexer support for ++ and -- and []
  def arg = for {
    _ <- Just(Token.Comma)
    expr <- primaryExpression
  } yield Right(expr)

  for {
    _ <- Just(Token.OpenParen)
    firstArg <- primaryExpression
    args <- arg.many
  } yield {
    Right(PostfixOp.FunctionCall(firstArg :: args))
  }
   
// postfixExpression -> atom {postfixOperation}*
def postfixExpression: ParseRule[AstExt] = 
  for {
    expr <- atom
    postfixOperations <- postfixOperation.many
  } yield postfixOperations.foldLeft[Either[CompilerError, AstExt]](Right(expr)) {
    case (Right(acc), op) => Right(Spanned(AstExtKind.PostfixOperation(op, acc), Span.merge(acc.span, expr.span)))
    case (Left(err), _) => Left(err)
  }

// unaryExpression -> [+-] unaryExpression | postfixExpression
def unaryExpression: ParseRule[AstExt] = {
  val withPrefix = (for {
    op <- OneOf(List(Token.Plus, Token.Minus))
    a <- unaryExpression
  } yield op.token match
    case Token.Plus  => Right(AstExtKind.PrefixOperation(PrefixOp.UnaryPlus, a))
    case Token.Minus => Right(AstExtKind.PrefixOperation(PrefixOp.Negation, a))
    case _           => Left(CompilerError("Invalid unary operation", op.span))
  ).withSpan
  withPrefix <|> postfixExpression
}

// primaryExpression -> unaryExpression {[*/] unaryExpression}*
def secondaryExpression: ParseRule[AstExt] = {
  for {
    left <- unaryExpression
    rest <- (OneOf(List(Token.Times, Token.Div)) <*> unaryExpression).many
  } yield rest.foldLeft[Either[CompilerError, AstExt]](Right(left)) {
    case (acc, (opToken, right)) =>
      acc match
        case Left(_) => acc
        case Right(acc) =>
          opToken.token match {
            case Token.Times =>
              Right(
                Spanned(
                  AstExtKind.Binary(BinaryOp.Mult, acc, right),
                  Span.merge(acc.span, right.span)
                )
              )
            case Token.Div =>
              Right(
                Spanned(
                  AstExtKind.Binary(BinaryOp.Div, acc, right),
                  Span.merge(acc.span, right.span)
                )
              )
            case _ =>
              Left(CompilerError("Invalid binary operation", opToken.span))
          }
  }
}

// primaryExpression -> secondaryExpression {[+-] secondaryExpression}*
def primaryExpression: ParseRule[AstExt] = {
  for {
    left <- secondaryExpression
    rest <- (OneOf(List(Token.Plus, Token.Minus)) <*> secondaryExpression).many
  } yield rest.foldLeft[Either[CompilerError, AstExt]](Right(left)) {
    case (acc, (opToken, right)) =>
      acc match
        case Left(_) => acc
        case Right(acc) =>
          opToken.token match {
            case Token.Plus =>
              Right(
                Spanned(
                  AstExtKind.Binary(BinaryOp.Add, acc, right),
                  Span.merge(acc.span, right.span)
                )
              )
            case Token.Minus =>
              Right(
                Spanned(
                  AstExtKind.Binary(BinaryOp.Sub, acc, right),
                  Span.merge(acc.span, right.span)
                )
              )
            case _ =>
              Left(CompilerError("Invalid binary operation", opToken.span))
          }
  }
}
