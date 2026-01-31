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

class LiteralPass extends PropagatingTypeChecker[Unit] {
  def lookupType(identifier: AstExt, env: TypeEnvironment): Type =
    identifier.node match {
      case AstExtKind.Identifier(name) =>
        env.get(name) match {
          case Some(t) => t
          case None =>
            throw CompilerError(s"Undefined identifier: $name", identifier.span)
        }

      case _ =>
        throw new RuntimeException("lookupType called on non-identifier")
    }

  override protected def initializeContext: Unit = ()

  override protected def updateContext(context: Unit, typeMap: Map[AstID, Type], node: AstExt): Unit = context

  override protected def updateTypeMap(context: Unit, typeMap: Map[AstID, Type], node: AstExt): Map[Int, Type] = {
    node.node match {
      case AstExtKind.IntLiteral(_) => {
        typeMap + (node.id -> NumT(4))
      }
      case _ => typeMap
    }
  }

}

