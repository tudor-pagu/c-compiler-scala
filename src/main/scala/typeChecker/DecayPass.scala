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

case class DecayContext(decayFunctions:Boolean, decayArrays:Boolean)

class DecayPass extends PropagatingTypeChecker[DecayContext] {
  override protected def initializeContext: DecayContext =
    DecayContext(false, false)

  override protected def updateContextForChildren(
      context: DecayContext,
      typeMap: TypeMap,
      node: AstExt
  ): DecayContext = {

    node.node match {
      case AstExtKind.Binary(op, l, r) => DecayContext(true, true)
      case AstExtKind.PostfixOperation(op, e) => DecayContext(true, true)
      case AstExtKind.PrefixOperation(op, e) => {
        op match {
          case PrefixOp.UnaryPlus => DecayContext(true, true)
          case PrefixOp.Negation  => DecayContext(true, true)
          case PrefixOp.AddressOf => DecayContext(context.decayFunctions, false)
          case _                  => context
        }
      }
      case AstExtKind.DeclarationList(_,_) => DecayContext(true, true)
      case AstExtKind.Assignment(_,_) => DecayContext(true, true)
      case AstExtKind.Return(_) => DecayContext(true, true)

      case _ => context
    }
  }

  override protected def updateTypeMap(
      context: DecayContext,
      typeMap: TypeMap,
      node: AstExt
  ): TypeMap = {
    if (!context.decayFunctions && !context.decayArrays) {
      return typeMap
    }

    val t = typeMap.get(node)
    if (t.isEmpty) {
      return typeMap
    }

    t.get match {
      case fun @ FunT(_,_) if context.decayFunctions => {
        (typeMap + (node -> PtrT(fun))).markUnmodifiable(node)
      }
      case array @ ArrayT(_,innerT) if context.decayArrays => {
        (typeMap + (node -> PtrT(innerT))).markUnmodifiable(node)
      }
      case _ => {
        typeMap
      }
    }
  }
}

