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
import tpagu.compiler.parser.DeclarationSpecifier.TSpecifier
import java.util.IdentityHashMap

type TypeEnvironment = Map[String, Type]

def createError(message: String, span: Span): CompilerError =
  CompilerError(message, span)

def lookupType(identifier: AstExt, env: TypeEnvironment): Type =
  identifier.node match {
    case AstExtKind.Identifier(name) =>
      env.get(name) match {
        case Some(t) => t
        case None =>
          throw CompilerError(s"Undefined identifier: $name", identifier.span)
      }

    case _ => throw new RuntimeException("lookupType called on non-identifier")
  }


object TypeCheck {
  def typeOf(e: AstExt, nv: TypeEnvironment): (Type, TypeEnvironment) = TypeCheck().typeOf(e, nv)
}

class TypeCheck {
  // typeMap gives hints to the desugarer when creating the typed, core AST.
  var typeMap : IdentityHashMap[AstExt, Type] = IdentityHashMap()

  def typeOf(e: AstExt, nv: TypeEnvironment): (Type, TypeEnvironment) =
    implicit val span: Span = e.span
    val result = e.node match {
      case AstExtKind.IntLiteral(_) => (NumT(4), nv)
      case AstExtKind.Binary(op, l, r) =>
        (op, typeOf(l, nv)._1, typeOf(r, nv)._1) match {
          case (
                BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mult | BinaryOp.Div,
                l @ NumT(lsize, lsigned),
                r @ NumT(rsize, rsigned)
              ) => {
            // TODO: This works now when the only type is int, but will not be correct once we have more types.
            (l, nv)
          }
          case _ =>
            throw createError(
              s"Invalid binary operation: ${op.toString} between ${typeOf(l, nv)._1.toString} and ${typeOf(r, nv)._1.toString}",
              e.span
            )
        }
      case AstExtKind.PrefixOperation(op, e) =>
        (
          (op, typeOf(e, nv)._1) match {
            case (PrefixOp.Negation, t @ NumT(size, signed)) =>
              Type.numericalPromotion(t)
            case (PrefixOp.UnaryPlus, t @ NumT(size, signed)) =>
              Type.numericalPromotion(t)
            case (_, t) =>
              throw createError(
                s"Invalid unary opration on ${t.toString}",
                e.span
              )
          },
          nv
        )
      case AstExtKind.PostfixOperation(op, e) =>
        (
          (op, typeOf(e, nv)._1) match {
            case (
                  PostfixOp.FunctionCall(args),
                  t @ FunT(returnType, params)
                ) => {
              if (args.size != params.size) {
                throw createError(
                  s"Tried to call a function with ${args.size} arguments, when the function takes ${params.size} parameters.",
                  e.span
                )
              }
              val argTypes = args.map(typeOf(_, nv)._1)
              if (argTypes != params) {
                throw createError(
                  s"Mismatch between types expected by function and arguments.",
                  e.span
                )
              }
              returnType
            }
            case (op, t) =>
              throw createError(s"Tried to call non-function type: ${t.toString}", e.span)
          },
          nv
        )
      case AstExtKind.Identifier(name) => (lookupType(e, nv), nv)
      case AstExtKind.DeclarationList(declSpecifiers, initDeclaratorList) => {
        var newNv = nv;
        for ((declarator, init) <- initDeclaratorList) {
          val declaredType = getTypeOfDeclaration(declSpecifiers, declarator)
          init.map(initValue => {
            if (typeOf(initValue, nv)._1 != declaredType) {
              throw createError("Type of initializer did not match declared type.", e.span)
            }
          })

          val name = getNameOfDeclarator(declarator).getOrElse(
            throw createError(
              "Abstract declaration not allowed here. You must give this declaration a name.",
              e.span
            )
          )
          newNv = nv + (name -> declaredType)
        }
        (NoneT(), newNv)
      }
      case AstExtKind.FunctionDefinition(declaration, body) => {
        val declarationType = getTypeOfDeclaration(declaration)
        val name = getNameOfDeclarator(declaration.declarator).getOrElse(
          throw createError(
            "Abstract declaration not allowed here. You must give this declaration a name.",
            e.span
          )
        )
        declarationType match {
          case func@FunT(_, _) => {}
          case t =>
            throw createError(
              s"Expected function type in function declaration, but got ${t}. This is probably a bug in the parser.",
              e.span
            )
        }
        val newNv = nv + (name -> declarationType)
        val nvForBody = newNv ++ getParameterMappings(declaration)
        val bodyT = typeOf(body, nvForBody)._1

        typeMap.put(e, declarationType)
        (NoneT(), newNv)
      }
      case AstExtKind.Block(statements) => {
        val check = statements.foldLeft[TypeEnvironment](nv) {
          (accNv, statement) =>
            typeOf(statement, accNv)._2
        }
        (
          NoneT(),
          nv
        )
      }
      case AstExtKind.TranslationUnit(statements) => {
        val check = statements.foldLeft[TypeEnvironment](nv) {
          (accNv, statement) =>
            typeOf(statement, accNv)._2
        }
        (NoneT(), nv)
      }
      case AstExtKind.ExprStatement(e: AstExt) => {
        // compute the type of the expresssion just to check it.
        val typeOfE = typeOf(e, nv)._1
        (NoneT(), nv)
      }
      case AstExtKind.Nothing => (NoneT(), nv)
      case AstExtKind.Return(e) => {
        val typeOfE = typeOf(e, nv)._1
        (NoneT(), nv)
      }
    }
    if (!typeMap.containsKey(e)) {
      // for some nodes we want to give special hints to the desugarer, so we override this behavior...
      // for example, for function definitions, the type of the node is NoneT(), but we make it be the type of the function anyway, just so that
      // the desugerar can create the AstC properly.
      typeMap.put(e, result._1)
    }
    result

}


def getBaseType(declSpecifiers: List[DeclarationSpecifier])(implicit span: Span): Type =
  val l = declSpecifiers.filter { 
    case TSpecifier(t) => true
    case _ => false
  }.map {
    case TSpecifier(t) => t
    case _ => throw RuntimeException("Unreachable code!")
  }
  l.headOption.getOrElse({println("tpagu bad"); throw createError("No valid type specifier found in declaration.",span);})

def getTypeOfDeclaration(declaration: Declaration)(implicit span: Span): Type = getTypeOfDeclaration(
  declaration.declarationSpecifiers,
  declaration.declarator
)

def getTypeOfDeclaration(
    declSpecifiers: List[DeclarationSpecifier],
    declarator: Declarator
)(implicit span: Span): Type = {
  val baseType = getBaseType(declSpecifiers)
  declarator.direct match {
    case DirectDeclarator.Variable(_) => baseType
    case DirectDeclarator.Function(_, params) =>
      FunT(baseType, params.map(getTypeOfDeclaration(_)))
    case DirectDeclarator.InnerDeclarator(decl) =>
      getTypeOfDeclaration(declSpecifiers, decl)
  }
}

def getNameOfDeclarator(declarator: Declarator): Option[String] =
  declarator.direct match {
    case DirectDeclarator.Variable(name)        => name
    case DirectDeclarator.Function(name, _)     => name
    case DirectDeclarator.InnerDeclarator(decl) => getNameOfDeclarator(decl)
  }

def getParameterMappings(f: Declaration)(implicit span: Span): Map[String, Type] = f.declarator.direct match {
  case DirectDeclarator.Function(name, params) => {
    val x = params
      .filter(decl => decl.declarator.direct.getName().isDefined)
      .map(decl => (decl.declarator.direct.getName()
      .getOrElse(throw new RuntimeException("impossible")), getTypeOfDeclaration(decl)))
    x.toMap
  }
  case _ => throw new RuntimeException("unreachable code - called getParameterMappings with non function declaration")
}

