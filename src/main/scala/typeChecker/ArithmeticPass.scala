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

class ArithmeticPass extends PropagatingTypeChecker[Unit] {
  override protected def initializeContext: Unit = ()

  override protected def updateTypeMap(context: Unit, typeMap: Map[AstID, Type], node: AstExt): Map[Int, Type] = {
    node.node match {
      case AstExtKind.Binary(op, l, r) => {
        op match {
          case (BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mult | BinaryOp.Div) => {
            // TODO: This works now when the only type is int, but will not be correct once we have more types.
            typeMap + (node.id -> typeMap(l.id))
          }
        }
      }
      case _ => typeMap
    }
  }

}


