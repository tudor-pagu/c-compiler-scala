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

object CombinedTypeCheck {
  def check(e: AstExt): TypeMap = {
    val typeMap = TypeMap()

    val passes = List(
      IdLookupPass(),
      LiteralPass(),

      DecayPass(),
      IntegerPromotionPass(),

      // time to combine everything
      ArithmeticPass(),
      AssignmentPass()
    )

    passes.foldLeft(typeMap)((acc, pass) => pass.check(e, acc))
  }

}
