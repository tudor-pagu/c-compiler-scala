package tpagu.compiler.parser
import tpagu.compiler.lexer.{Lexer, Token, TokenInfo}
import tpagu.compiler.CompilerError

type Node = Out[AstNodeExt]
def primaryExpression: ParseRule[AstNodeExt] = ???

def expression: ParseRule[AstNodeExt] =
  primaryExpression


def unaryExpression: ParseRule[AstNodeExt] = ???
def secondaryExpression: ParseRule[AstNodeExt] = ???

def primaryExpressionMonadic: ParseRule[AstNodeExt] = {
  def buildBinary(left: AstNodeExt, ops: List[(TokenInfo, AstNodeExt)]): Either[CompilerError, AstNodeExt] = {
    ops.foldLeft[Either[CompilerError, AstNodeExt]](Right(left)) { case (acc, (opToken, right)) =>
      acc match {
        case Left(_) => acc
        case Right(acc) =>       opToken.token match {
        case Token.Plus => Right(AstNodeExt.Binary(BinaryOp.Add, acc, right))
        case Token.Minus => Right(AstNodeExt.Binary(BinaryOp.Sub, acc, right))
        case _ => Left(CompilerError("Invalid operator", opToken.span))
      }

      }

    }
  }
  
  for {
    left <- secondaryExpression
    rest <- (OneOf(List(Token.Plus, Token.Minus)) <*> secondaryExpression).many
  } yield buildBinary(left, rest)
}
