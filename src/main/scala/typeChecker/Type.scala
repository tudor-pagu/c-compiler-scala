package tpagu.compiler.typeChecker

// This represents the type we use in our core typed AST for lowering into IR, etc.
// It's not user facing.

// A type may be created from a user provided type, so we may want to keep a reference to it for diagnostics.
sealed trait Type {
  def size(): Int // all types need to know their size
  def alignment(): Long // all types need to know their alignment requirement
  def qualifiers: TypeQualifiers

  private var _frontendName: Option[String] = None

  def frontendName: Option[String] = _frontendName
  def withFrontendName(name: String): Type = {
    _frontendName = Some(name)
    this
  }
  def withFrontendName(name: Option[String]): Type = {
    _frontendName = name
    this
  }

  def prettyName(): String = {
    frontendName match {
      case Some(name) => name
      case None       => toString
    }
  }
}

case class TypeQualifiers(
    isConst: Boolean = false,
    isVolatile: Boolean = false,
    isRestrict: Boolean = false
)

case class NumT(
    width: Int,
    signed: Boolean = true,
    qualifiers: TypeQualifiers = TypeQualifiers()
) extends Type {
  override def size(): Int = width
  override def alignment(): Long = width
}

case class PtrT(innerT: Type, qualifiers: TypeQualifiers = TypeQualifiers())
    extends Type {
  override def size(): Int = 8 // just hard coding 8 bytes as the size
  override def alignment(): Long = 8
}

case class FunT(returnType: Type, paramTypes: List[Type]) extends Type {
  override def size(): Int = {
    throw RuntimeException(
      "Tried to take size of a function type. This should not be allowed by the type checker."
    )
  }
  override def alignment(): Long = {
    throw RuntimeException(
      "Tried to take alignment of a function type. This should not be allowed by the type checker."
    )
  }
  override def qualifiers: TypeQualifiers = TypeQualifiers(isConst = true)
}
case class NoneT() extends Type { // Just used for statements, things which shouldn't have types\
  override def size(): Int = {
    throw RuntimeException(
      "Tried to take size of a NoneT type. This should not be allowed by the type checker."
    )
  }
  override def alignment(): Long = {
    throw RuntimeException(
      "Tried to take alignment of a NoneT type. This should not be allowed by the type checker."
    )
  }
  override def qualifiers: TypeQualifiers = {
    throw RuntimeException(
      "Tried to take qualifiers of NoneT type. This should not be allowed by the type checker."
    )
  }
}

object Type {
  val ptrSize = 8
  val defaultIntSize: Int = 4
  def numericalPromotion(t: NumT): Type =
    if t.width < defaultIntSize then NumT(defaultIntSize, t.signed) else t

  def dropQualifiers(t: Type): Type = {
    t match {
      case NumT(w, s, _) => NumT(w, s).withFrontendName(t._frontendName)
      case PtrT(i, _)    => PtrT(i).withFrontendName(t._frontendName)
      case _             => t
    }
  }

  /**
   * Return true if left has more or equal qualifiers than right
   */
  def qualifierSuperset(left: TypeQualifiers, right: TypeQualifiers): Boolean = {
    return (left.isConst || (!right.isConst)) && (left.isVolatile || (!right.isVolatile)) && (left.isRestrict || (!right.isRestrict))
  }

  def pointsToConst(t:Type):Boolean = {
    t match {
      case PtrT(inner, quals) => {
        inner.qualifiers.isConst
      }
      case _ => {
        false
      }
    }
  }

  /*
   * Returns true iff left and right are the same type,
   * and left has strictly more qualifiers than right
   */
  def qualifierCompatibleTypes(left: Type, right: Type): Boolean = {
    (left, right) match {
      case (NumT(_,_,lQuals), NumT(_,_,rQuals)) => {
        return dropQualifiers(left) == dropQualifiers(right) && qualifierSuperset(lQuals, rQuals)
      }
      case (PtrT(innerL, qualsL), PtrT(innerR, qualsR)) => {
        return qualifierCompatibleTypes(innerL, innerR) && qualifierSuperset(qualsL, qualsR) && (!pointsToConst(innerL) || pointsToConst(left))
      }
      case _ => false
    }
    return dropQualifiers(left) == dropQualifiers(right) && qualifierSuperset(left.qualifiers, right.qualifiers)
  }
}
