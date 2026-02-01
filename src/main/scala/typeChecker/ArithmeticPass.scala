package tpagu.compiler.typeChecker

import tpagu.compiler.parser.AstExt
import tpagu.compiler.AstID
import tpagu.compiler.parser.AstExtKind
import tpagu.compiler.typeChecker.NumT
import tpagu.compiler.parser.BinaryOp
import tpagu.compiler.parser.PrefixOp
import tpagu.compiler.typeChecker.PtrT
import tpagu.compiler.parser.PostfixOp
import tpagu.compiler.typeChecker.FunT
import tpagu.compiler.CompilerError
import tpagu.compiler.Span

def takeMaxWidth(l: NumT, r: NumT): NumT = {
  if (l.width > r.width) {
    l
  } else {
    r
  }
}

def checkFunctionCall(
    e: AstExt,
    typeMap: TypeMap,
    f: FunT,
    args: List[AstExt]
): Type = {
  if (args.size != f.paramTypes.size) {
    throw createError(
      s"Tried to call a function with ${args.size} arguments, when the function takes ${f.paramTypes.size} parameters.",
      e.span
    )
  }
  val argTypes = args.map(arg => typeMap(arg))
  if (argTypes != f.paramTypes) {
    throw createError(
      s"Mismatch between types expected by function and arguments.",
      e.span
    )
  }
  f.returnType
}

def binaryOpConversion(l: Type, r: Type, span: Span): Type = {
  (l, r) match {
    case (
          lt @ NumT(widthLeft, signedLeft, qualsLeft),
          rt @ NumT(widthRight, signedRight, qualsRight)
        ) => {
      if (widthLeft == widthRight) {
        NumT(widthLeft, signedLeft && signedRight)
      } else {
        takeMaxWidth(lt, rt)
      }
    }
    case _ => {
      throw createError(
        s"Could not convert between types: ${l} and ${r} for binary operation.",
        span
      )
    }
  }
}

/*
 * I expand dereference and reference operations as part of "arithmetic" which
 * is maybe a weird naming
 */
class ArithmeticPass extends PropagatingTypeChecker[Unit] {
  override protected def initializeContext: Unit = ()

  override protected def updateTypeMap(
      context: Unit,
      typeMap: TypeMap,
      node: AstExt
  ): TypeMap = {
    node.node match {
      case AstExtKind.Binary(op, l, r) => {
        op match {
          case (BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mult | BinaryOp.Div) => {
            val leftT = typeMap(l)
            val rightT = typeMap(r)
            val t = (if (leftT != rightT) {
                       // we try to convert
                       binaryOpConversion(leftT, rightT, node.span)
                     } else {
                       leftT
                     })

            typeMap + (node -> t)
          }
        }
      }
      case AstExtKind.PrefixOperation(op, e) => {
        op match {
          case PrefixOp.AddressOf => {
            val t = typeMap(e)
            if (isLvalue(e, typeMap)) {
              typeMap + (node -> PtrT(t, t.qualifiers))
            } else {
              throw createError(
                s"Tried to get the address of rvalue.",
                e.span
              )
            }
          }
          case PrefixOp.Dereference => {
            val t = typeMap(e)
            t match {
              case PtrT(inner, qualifiers) => typeMap + (node -> inner)
              case _ =>
                throw createError(
                  s"Tried to dereference non pointer type: ${t.prettyName()}",
                  node.span
                )
            }
          }
          case PrefixOp.Negation | PrefixOp.UnaryPlus => {
            val t = typeMap(e)
            if (!t.isInstanceOf[NumT]) {
              throw createError(
                "Tried to apply unary arithmetic operation to non number type.",
                node.span
              )
            }

            typeMap + (node -> t)
          }
        }
      }
      case AstExtKind.PostfixOperation(op: PostfixOp.FunctionCall, e) => {
        val calleeT = typeMap(e)
        calleeT match {
          case t @ FunT(_, _) => {
            typeMap + (node -> checkFunctionCall(e, typeMap, t, op.args))
          }
          case t @ PtrT(innerT: FunT, _) => {
            typeMap + (node -> checkFunctionCall(e, typeMap, innerT, op.args))
          }
          case _ => {
            throw createError(
              "Tried to call callee that was not a function or function pointer.",
              e.span
            )
          }
        }
      }

      case _ => typeMap
    }
  }

}
