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
import tpagu.compiler.parser.DeclarationSpecifier
import tpagu.compiler.parser.Declarator

class IntegerPromotionPass extends PropagatingTypeChecker[Boolean] {
  override protected def initializeContext: Boolean =
    false

  override protected def updateContextForChildren(
      context: Boolean,
      typeMap: TypeMap,
      node: AstExt
  ): Boolean = {

    node.node match {
      case AstExtKind.Binary(op, l, r) => true
      case AstExtKind.PrefixOperation(op, e) => {
        op match {
          case PrefixOp.UnaryPlus => true
          case PrefixOp.Negation  => true
          case _                  => context
        }
      }
      case _ => context
    }
  }

  override protected def updateTypeMap(
      context: Boolean,
      typeMap: TypeMap,
      node: AstExt
  ): TypeMap = {
    if (!context) {
      return typeMap
    }

    val t = typeMap.get(node)
    if (t.isEmpty) {
      return typeMap
    }

    t.get match {
      case NumT(width, signed, qualifiers) => {
        if (width < 4) {
          typeMap + (node -> NumT(4))
        } else {
          typeMap
        }

      }
      case _ => {
        typeMap
      }
    }
  }
}
