package tpagu.compiler.typeChecker

// This represents the type we use in our core typed AST for lowering into IR, etc.
// It's not user facing.

// A type may be created from a user provided type, so we may want to keep a reference to it for diagnostics.
sealed trait Type {
  def size(): Long // all types need to know their size
  def alignment(): Long // all types need to know their alignment requirement
}

case class TypeQualifiers(
  isConst: Boolean = false,
  isVolatile: Boolean = false,
  isRestrict: Boolean = false
)

case class NumT(width: Long, signed: Boolean = true, qualifiers: TypeQualifiers = TypeQualifiers()) extends Type {
  override def size(): Long = width
  override def alignment(): Long = width
  override def toString(): String = s"NumT($width,$signed)"
}

case class PtrT(innerT: Type, qualifiers: TypeQualifiers = TypeQualifiers()) extends Type {
  override def size(): Long = 8 // just hard coding 8 bytes as the size
  override def alignment(): Long = 8
}

case class FunT(returnType: Type, paramTypes: List[Type]) extends Type {
  override def size(): Long = {
    throw RuntimeException("Tried to take size of a function type. This should not be allowed by the type checker.")
  }
  override def alignment(): Long = {
    throw RuntimeException("Tried to take alignment of a function type. This should not be allowed by the type checker.")
  }
}
case class NoneT() extends Type { // Just used for statements, things which shouldn't have types\
  override def size(): Long = {
    throw RuntimeException("Tried to take size of a NoneT type. This should not be allowed by the type checker.")
  }
  override def alignment(): Long = {
    throw RuntimeException("Tried to take alignment of a NoneT type. This should not be allowed by the type checker.")
  }
}

object Type {
  val ptrSize = 8
  val defaultIntSize: Long = 4
  def numericalPromotion(t: NumT): Type = 
    if t.width < defaultIntSize then NumT(defaultIntSize , t.signed) else t
}

