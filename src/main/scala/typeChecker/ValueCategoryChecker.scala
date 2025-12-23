package tpagu.compiler.typeChecker

import java.util.IdentityHashMap
import tpagu.compiler.parser.AstExt
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.parser.AstExtKind.Identifier
import tpagu.compiler.typeChecker.FunT
import tpagu.compiler.parser.PrefixOp
import tpagu.compiler.parser.AstExtKind.PrefixOperation

// Checks that no rules regarding lvalue/rvalue
// are violated.
object ValueCategoryChecker {
  def isLvalue(e: AstExt, typeMap: IdentityHashMap[AstExt, Type]): Boolean =
    e.node match {
      case Identifier(name) => {
        typeMap.get(e) match {
          case FunT(_, _) => false
          case _          => true
        }
      }
      case PrefixOperation(PrefixOp.Dereference, e) => {
        true
      }
      case _                                 => false
      // TODO add more cases, for now we just support identifiers.
    }

}
