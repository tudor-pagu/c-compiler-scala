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

class CallPass extends PropagatingTypeChecker[Unit] {
  override protected def initializeContext: Unit = ()

  override protected def updateContext(
      context: Unit,
      typeMap: Map[AstID, Type],
      node: AstExt
  ): Unit = context

  def checkFunctionCall(
      e: AstExt,
      typeMap:Map[AstID, Type],
      f: FunT,
      args: List[AstExt]
  ): Type = {
    if (args.size != f.paramTypes.size) {
      throw createError(
        s"Tried to call a function with ${args.size} arguments, when the function takes ${f.paramTypes.size} parameters.",
        e.span
      )
    }
    val argTypes = args.map(arg => typeMap(arg.id))
    if (argTypes != f.paramTypes) {
      throw createError(
        s"Mismatch between types expected by function and arguments.",
        e.span
      )
    }
    f.returnType
  }

  override protected def updateTypeMap(
      context: Unit,
      typeMap: Map[AstID, Type],
      node: AstExt
  ): Map[Int, Type] = {
    node.node match {
      case AstExtKind.PostfixOperation(op: PostfixOp.FunctionCall, e) => {
        val calleeT = typeMap(e.id)
        calleeT match {
          case t @ FunT(_, _) => {
            typeMap + (e.id -> checkFunctionCall(e, typeMap,t, op.args))
          }
          case t @ PtrT(innerT:FunT, _) => {
            typeMap + (e.id -> checkFunctionCall(e, typeMap,innerT, op.args))
          }
          case _ => {
            throw createError("Tried to call callee that was not a function or function pointer.", e.span)
          }
        }
      }
      case _ => typeMap
    }
  }

}
