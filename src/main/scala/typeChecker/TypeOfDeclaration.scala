package tpagu.compiler.typeChecker

import tpagu.compiler.parser.AstExt
import tpagu.compiler.parser.AstExtKind
import tpagu.compiler.CompilerError
import tpagu.compiler.parser.BinaryOp
import tpagu.compiler.parser.PrefixOp
import tpagu.compiler.Span
import tpagu.compiler.parser.PostfixOp
import tpagu.compiler.parser.DeclarationSpecifier
import tpagu.compiler.parser.Declarator
import tpagu.compiler.parser.DirectDeclarator
import tpagu.compiler.parser.Declaration
import java.util.IdentityHashMap
import tpagu.compiler.parser.TypeQualifier
import tpagu.compiler.parser.TypeSpecifier
import tpagu.compiler.parser.DeclaratorSuffix
import tpagu.compiler.parser.Pointer
import tpagu.compiler.createError

def getBaseType(declSpecifiers: List[DeclarationSpecifier])(implicit
    span: Span,
    typeEnvironment: TypeEnvironment
): Type = {
  val typeSpecifiers = declSpecifiers
    .filter {
      case ts: TypeSpecifier => true
      case _                 => false
    }
  if (typeSpecifiers.isEmpty) {
    throw createError("No valid type specifier found in declaration.", span);
  }

  val typedefNames = typeSpecifiers
    .filter(x =>
      x match {
        case TypeSpecifier.TypedefName(name) => true
        case _                               => false
      }
    )
    .map(x =>
      x match
        case TypeSpecifier.TypedefName(name) => name
    )

  val numTypedefNames = typedefNames.length

  if (numTypedefNames > 0) {
    if (typeSpecifiers.length > 1) {
      throw createError(
        "Cannot have any other type specifiers besides a typedef name.",
        span
      )
    }

    if (numTypedefNames > 1) {
      throw createError(
        "Cannot have more than one typedef name in a declaration",
        span
      );
    }

    val typeOfTypedef = typeEnvironment.get(typedefNames.head)
    typeOfTypedef match {
      case Some(t) => {
        return t
      }
      case _ =>
        throw RuntimeException(
          "Undefined typedef in type checker even though that makes no sense; how did the lexer give us a typename?"
        )
    }
  }

  // handling of integer types
  val numInt = typeSpecifiers.count(_ == TypeSpecifier.Int)
  val numLong = typeSpecifiers.count(_ == TypeSpecifier.Long)

  if (numInt == 0 && numLong == 0) {
    throw createError(
      "No type specifier indicating an actual type was found.",
      span
    );
  }
  val numUnsigned = typeSpecifiers.count(_ == TypeSpecifier.Unsigned)
  val numSigned = typeSpecifiers.count(_ == TypeSpecifier.Signed)

  if (numUnsigned >= 1 && numSigned >= 1) {
    throw createError("Cannot combine signed specifier with unsigned.", span)
  }

  if (numSigned > 1) {
    throw createError("Duplicate signed specifier.", span)
  }

  if (numUnsigned > 1) {
    throw createError("Duplicate unsigned specifier.", span)
  }

  val signed: Boolean = (numUnsigned == 0)

  val isConst = declSpecifiers.count(_ == TypeQualifier.Const) > 0
  val isVolatile = declSpecifiers.count(_ == TypeQualifier.Volatile) > 0
  // TODO: Add restrict
  val typeQualifiers = TypeQualifiers(isConst, isVolatile)

  if (numInt > 0 || numLong > 0) {
    // it must be a numeric type
    if (numInt > 1) {
      throw createError("Cannot specify int more than once.", span)
    }
    if (numLong > 2) {
      throw createError("Cannot specify long more than twice.", span)
    }

    if (numLong == 0) {
      return NumT(4, signed, typeQualifiers).withFrontendName("int")
    }

    if (numLong <= 1) {
      return NumT(4, signed, typeQualifiers).withFrontendName("long")
    }

    if (numLong == 2) {
      return NumT(8, signed, typeQualifiers).withFrontendName("long long")
    }
  }

  throw RuntimeException(
    "Reached end of get getBaseType, which shouldn't happen (we should throw an error early). There must be a bug here."
  );
}


def getTypeOfDeclaration(
    declaration: Declaration
)(implicit span: Span, typeEnvironment: TypeEnvironment): Type =
  getTypeOfDeclaration(
    declaration.declarationSpecifiers,
    declaration.declarator
  )

def combineQualifiers(quals: List[TypeQualifier]): TypeQualifiers = {
  TypeQualifiers(
    isConst = quals.contains(TypeQualifier.Const)
    // TODO: Add the rest of the qualifiers
  )
}

def applySuffixesToBaseType(
    baseType: Type,
    suffixes: List[DeclaratorSuffix]
)(implicit span: Span, typeEnvironment: TypeEnvironment): Type = {

  if (suffixes.isEmpty) {
    return baseType
  }

  if (suffixes.length > 1) {
    throw NotImplementedError(
      s"More than one consecutive suffix is only fine for arrays, but those are not implemented yet."
    )
  }

  suffixes.head match {
    case params @ DeclaratorSuffix.Params(_) => {
      return FunT(
        baseType,
        getParameterTypes(params)
      )
    }
    case DeclaratorSuffix.Array(length) => {
      throw NotImplementedError("Arrays not yet implemented.")
    }
  }
}

/** The important thing to keep in mind when reading this code is this:
  *
  * In C, the order in which you parse a declarator is this:
  *
  * 1) First you need to handle any forced priorities imposed by paranthesis
  * around the declarator. This is what makes a statement like `int (*p)(int)`
  * possible. We start with (*p), making the base type int*
  *
  * 2) Then you need to handle any suffixes. These are either arrays or
  * functions.
  *
  * A function suffix (like `(int)` in the example above) makes the type go from
  * `int*` to `int (*)(int)`, which is a ptr to function returning int. Similar
  * logic exists for arrays.
  *
  * 3) Now you need to handle any pointers in the top level declarator. These
  * are things like: int *x; These just add one or more levels of indirection.
  * For example in the expression int* (*p)(int) the steps would be:
  *
  * (1) int* => (2) int(*)(int) => (3) int* (*) (int)
  *
  * And we get the final interpretation of a pointer to a function taking an int
  * and returning a pointer.
  */

def applyPointersToType(baseType: Type, pointers: List[Pointer]): Type = {
  if (pointers.isEmpty) {
    return baseType
  }

  val quals = pointers.last.qualifiers
  PtrT(
    applyPointersToType(baseType, pointers.init),
    combineQualifiers(quals)
  )
}

//TODO: Clean this up a bit to remove duplication
def applyInnerDeclarator(
    baseType: Type,
    directDeclarator: DirectDeclarator
)(implicit span: Span, typeEnvironment: TypeEnvironment): Type = {
  directDeclarator match {
    case DirectDeclarator.InnerDeclarator(decl) => {
      val typeWithPtrs = applyPointersToType(baseType, decl.pointers)
      val typeWithSuffix = applySuffixesToBaseType(typeWithPtrs, decl.suffixes)
      val typeWithInnerDeclarator =
        applyInnerDeclarator(typeWithSuffix, decl.direct)
      typeWithInnerDeclarator
    }
    case DirectDeclarator.Variable(name) => {
      baseType
    }
  }
}

def getTypeOfDeclaration(
    declSpecifiers: List[DeclarationSpecifier],
    declarator: Declarator
)(implicit span: Span, typeEnvironment: TypeEnvironment): Type = {

  val baseType = getBaseType(declSpecifiers)

  val typeWithPtrs = applyPointersToType(baseType, declarator.pointers)
  val typeWithSuffix =
    applySuffixesToBaseType(typeWithPtrs, declarator.suffixes)
  val typeWithInnerDeclarator =
    applyInnerDeclarator(typeWithSuffix, declarator.direct)

  typeWithInnerDeclarator
}

def getNameOfDeclarator(declarator: Declarator): Option[String] =
  declarator.direct match {
    case DirectDeclarator.Variable(name)        => name
    case DirectDeclarator.InnerDeclarator(decl) => getNameOfDeclarator(decl)
  }

def getParameterTypes(
    rawParams: DeclaratorSuffix.Params
)(implicit span: Span, typeEnvironment: TypeEnvironment): List[Type] = {
  rawParams.params.map(decl => getTypeOfDeclaration(decl))
}

def getParameterMappings(
    decl: Declarator
)(implicit span: Span, typeEnvironment: TypeEnvironment): Map[String, Type] = {
  decl.suffixes match {
    case List(DeclaratorSuffix.Params(params)) => {
      val x = params
        .filter(decl => decl.declarator.direct.getName().isDefined)
        .map(decl =>
          (
            decl.declarator.direct
              .getName()
              .getOrElse(throw new RuntimeException("impossible")),
            getTypeOfDeclaration(decl)
          )
        )
      x.toMap
    }
    case _ =>
      throw new RuntimeException(
        "unreachable code - called getParameterMappings with non function declaration"
      )
  }
}

