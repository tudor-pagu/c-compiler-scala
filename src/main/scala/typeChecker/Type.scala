// This represents the type we use in our core typed AST for lowering into IR, etc.
import tpagu.compiler.parser.TypeExt
// It's not user facing.

// A type may be created from a user provided type, so we may want to keep a reference to it for diagnostics.
sealed trait Type(val typeExt: Option[TypeExt] = None) {

}

case class NumT(size: Int, signed: Boolean = true) extends Type
case class FunT(returnType: Type, paramTypes: List[Type]) extends Type

object Type {
  val IntSize = 4
  def numericalPromotion(t: NumT): Type = 
    if t.size < IntSize then NumT(IntSize, t.signed) else t
}

