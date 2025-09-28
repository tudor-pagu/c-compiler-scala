import tpagu.compiler.parser.AstExt
import tpagu.compiler.parser.TypeExt
import tpagu.compiler.parser.TypeExtKind
import tpagu.compiler.parser.AstExtKind
import tpagu.compiler.CompilerError
import tpagu.compiler.parser.BinaryOp

type TypeEnvironment = Map[String, Type]

def createError (message: String, span: AstExt): CompilerError =
  CompilerError(message, span.span)
def lookupType(identifier: AstExt, env: TypeEnvironment): Type =
  identifier.node match {
    case AstExtKind.Identifier(name) => env.get(name) match {
    case Some(t) => t
    case None    => throw CompilerError(s"Undefined identifier: $name", identifier.span)
  }

    case _ => throw new RuntimeException("lookupType called on non-identifier")
  }

object TypeCheck {
  def typeOf(e: AstExt, nv: TypeEnvironment): Type = e.node match {
    case AstExtKind.IntLiteral(_) => NumT(8)
    case AstExtKind.Binary(op, l, r) => (op, typeOf(l, nv), typeOf(r, nv)) match {
      case (BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mult | BinaryOp.Div, l@NumT(lsize, lsigned), r@NumT(rsize, rsigned)) => {
        //TODO: This works now when the only type is int, but will not be correct once we have more types.
        l
      }
      case _ => throw CompilerError(s"Invalid binary operation: ${op.toString} between ${typeOf(l, nv).toString} and ${typeOf(r, nv).toString}", e.span)
    }
    case AstExtKind.Identifier(name) => lookupType(e, nv)

  }

}
