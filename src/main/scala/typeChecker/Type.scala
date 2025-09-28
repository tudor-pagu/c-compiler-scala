package tpagu.compiler.typeChecker
// This represents the type we use in our core typed AST for lowering into IR, etc.
// It's not user facing.

// A type may be created from a user provided type, so we may want to keep a reference to it for diagnostics.
sealed trait Type {

}

case class NumT(size: Int, signed: Boolean = true) extends Type
case class FunT(returnType: Type, paramTypes: List[Type]) extends Type
case class NoneT() extends Type // Just used for statements, things which shouldn't have types

object Type {
  val IntSize = 4
  def numericalPromotion(t: NumT): Type = 
    if t.size < IntSize then NumT(IntSize, t.signed) else t
}

