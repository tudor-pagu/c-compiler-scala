package tpagu.compiler.typeChecker

def areQualifiersImplicitlyConvertable(
    from: TypeQualifiers,
    to: TypeQualifiers
): Either[String, Unit] = {
  if (from.isConst && !to.isConst) {
    return Left("drops const qualifier.");
  }

  if (from.isVolatile && !to.isVolatile) {
    return Left("drops volatile qualifier.");
  }

  if (from.isRestrict && !to.isRestrict) {
    return Left("drops restrict qualifier.");
  }

  return Right(());
}

def isQualifierConvertible(from: Type, to: Type): Either[String, Unit] = {
  val x = areQualifiersImplicitlyConvertable(from.qualifiers, to.qualifiers)
  if (x.isLeft) {
    return x
  }

  val fromWithoutQuals = Type.dropQualifiers(from)
  val toWithoutQuals = Type.dropQualifiers(to)

  (fromWithoutQuals, toWithoutQuals) match {
    case (PtrT(innerFrom, _), PtrT(innerTo, _)) =>
      isQualifierConvertible(innerFrom, innerTo)
    case _ => {
      if (fromWithoutQuals != toWithoutQuals) {
        Left(
          s"type ${fromWithoutQuals} and type ${toWithoutQuals} are not the same."
        )
      } else {
        Right(())
      }

    }
  }
}

def isAssignmentSafe(left: Type, right: Type): Either[String, Unit] = {
  (Type.dropQualifiers(left), Type.dropQualifiers(right)) match {
    case (NumT(wFrom, sFrom, qualsFrom), NumT(wTo, sTo, qualsTo)) => {
      // all NumT are implicitly convertible between each other.
      // i.e. long and int
      Right(())
    }
    case (PtrT(innerLeft, qualsFrom), PtrT(innerRight, qualsTo)) => {
      return isQualifierConvertible(innerRight, innerLeft)
    }
    case _ => Left(s"Could not implicitly convert from ${right} to ${left}")
  }
}
