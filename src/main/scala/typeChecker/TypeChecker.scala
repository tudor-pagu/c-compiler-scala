package tpagu.compiler.typeChecker

import tpagu.compiler.parser.AstExt
import tpagu.compiler.parser.AstExtKind
import tpagu.compiler.CompilerError
import tpagu.compiler.parser.BinaryOp
import tpagu.compiler.parser.PrefixOp
import tpagu.compiler.Span
import tpagu.compiler.parser.PostfixOp
import tpagu.compiler.parser.DeclarationSpecifier
import tpagu.compiler.parser.Declarator

type TypeEnvironment = Map[String, Type]

def createError(message: String, span: Span): CompilerError =
  CompilerError(message, span)
def lookupType(identifier: AstExt, env: TypeEnvironment): Type =
  identifier.node match {
    case AstExtKind.Identifier(name) =>
      env.get(name) match {
        case Some(t) => t
        case None =>
          throw CompilerError(s"Undefined identifier: $name", identifier.span)
      }

    case _ => throw new RuntimeException("lookupType called on non-identifier")
  }

object TypeCheck {
  def typeOf(e: AstExt, nv: TypeEnvironment): Type = e.node match {
    case AstExtKind.IntLiteral(_) => NumT(8)
    case AstExtKind.Binary(op, l, r) =>
      (op, typeOf(l, nv), typeOf(r, nv)) match {
        case (
              BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mult | BinaryOp.Div,
              l @ NumT(lsize, lsigned),
              r @ NumT(rsize, rsigned)
            ) => {
          // TODO: This works now when the only type is int, but will not be correct once we have more types.
          l
        }
        case _ =>
          throw createError(
            s"Invalid binary operation: ${op.toString} between ${typeOf(l, nv).toString} and ${typeOf(r, nv).toString}",
            e.span
          )
      }
    case AstExtKind.PrefixOperation(op, e) =>
      (op, typeOf(e, nv)) match {
        case (PrefixOp.Negation, t @ NumT(size, signed)) =>
          Type.numericalPromotion(t)
        case (PrefixOp.UnaryPlus, t @ NumT(size, signed)) =>
          Type.numericalPromotion(t)
        case (_, t) =>
          throw createError(s"Invalid unary opration on ${t.toString}", e.span)
      }
    case AstExtKind.PostfixOperation(op, e) =>
      (op, typeOf(e, nv)) match {
        case (PostfixOp.FunctionCall(args), t @ FunT(returnType, params)) => {
          if (args.size != params.size) {
            throw createError(
              s"Tried to call a function with {args.size} arguments, when the function has ${params.size} parameters",
              e.span
            )
          }
          val argTypes = args.map(typeOf(_, nv))
          if (argTypes != params) {
            throw createError(
              s"Mismatch between types expected by function and arguments.",
              e.span
            )
          }
          returnType
        }
        case _ => throw createError(s"Tried to call non-function type.", e.span)
      }
    case AstExtKind.Identifier(name) => lookupType(e, nv)
    case AstExtKind.DeclarationList(declSpecifiers, initDeclaratorList)  => {
      for ( (declarator, init) <- initDeclaratorList) {
        val declaredType = getTypeOfDeclaration(declSpecifiers, declarator)
      }
      NoneT()
    }
    case AstExtKind.FunctionDefinition(_, _) => NoneT()
    case AstExtKind.Block(_) => NoneT()


  }

}


def getTypeOfDeclaration(declSpecifiers: List[DeclarationSpecifier], declarator: Declarator): Type = {
  
  ???
}
