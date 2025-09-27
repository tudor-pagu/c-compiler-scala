package tpagu.compiler.parser
import tpagu.compiler.lexer.{Lexer, Token, TokenInfo}
import tpagu.compiler.{CompilerError, Spanned}
import tpagu.compiler.Span

def primaryExpression: ParseRule[AstExt] = ???

def expression: ParseRule[AstExt] =
  primaryExpression


def unaryExpression: ParseRule[AstExt] = ???
def secondaryExpression: ParseRule[AstExt] = ???
def primaryExpressionMonadic: ParseRule[AstExt] = {
  val x = for {
    left <- secondaryExpression
    rest <- (OneOf(List(Token.Plus, Token.Minus)) <*> secondaryExpression).many
  } yield rest.foldLeft[Either[CompilerError, AstExt]](Right(left)) {
    case (acc, (opToken, right)) => acc match
    case Left(_) => acc
    case Right(acc) => opToken.token match {
      case Token.Plus => Right(Spanned(AstExtKind.Binary(BinaryOp.Add, acc, right), Span.merge(acc.span, right.span)))
      case Token.Minus => Right(Spanned(AstExtKind.Binary(BinaryOp.Sub, acc, right), Span.merge(acc.span, right.span)))
      case _ => Left(CompilerError("Invalid binary operation", opToken.span))
    }

  }
  throw NotImplementedError("")
}
