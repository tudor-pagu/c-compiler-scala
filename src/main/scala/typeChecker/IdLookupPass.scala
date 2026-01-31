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

class IdLookupPass extends PropagatingTypeChecker[TypeEnvironment] {
  def lookupType(identifier: AstExt, env: TypeEnvironment): Type =
    identifier.node match {
      case AstExtKind.Identifier(name) =>
        env.get(name) match {
          case Some(t) => t
          case None =>
            throw CompilerError(s"Undefined identifier: $name", identifier.span)
        }

      case _ =>
        throw new RuntimeException("lookupType called on non-identifier")
    }

  override protected def initializeContext: TypeEnvironment =
    Map[String, Type]()

  override protected def updateContextForChildren(
      context: TypeEnvironment,
      typeMap: Map[AstID, Type],
      node: AstExt
  ): TypeEnvironment = {

    node.node match {
      case AstExtKind.FunctionDefinition(declaration, body) => {
        implicit val typeEnvironment = context
        implicit val span = node.span

        val declarationType = getTypeOfDeclaration(declaration)

        val name = getNameOfDeclarator(declaration.declarator).getOrElse(
          throw createError(
            "Abstract declaration not allowed here. You must give this declaration a name.",
            node.span
          )
        )
        declarationType match {
          case func @ FunT(_, _) => {}
          case t =>
            throw createError(
              s"Expected function type in function declaration, but got ${t}. This is probably a bug in the parser.",
              node.span
            )
        }

        val newNv = context + (name -> declarationType)
        val nvForBody = newNv ++ getParameterMappings(declaration.declarator)
        nvForBody
      }
      case _ => context
    }
  }

  override protected def updateContext(
      context: TypeEnvironment,
      typeMap: Map[AstID, Type],
      node: AstExt
  ): TypeEnvironment = {
    node.node match {
      case AstExtKind.FunctionDefinition(declaration, body) => {
        implicit val typeEnvironment = context
        implicit val span = node.span

        val declarationType = getTypeOfDeclaration(declaration)

        val name = getNameOfDeclarator(declaration.declarator).getOrElse(
          throw createError(
            "Abstract declaration not allowed here. You must give this declaration a name.",
            node.span
          )
        )

        val newNv = context + (name -> declarationType)
        newNv
      }

      case AstExtKind.DeclarationList(declSpecifiers, initDeclaratorList) => {
        var newNv = context;
        for ((declarator, init) <- initDeclaratorList) {

          // TODO: Get rid of this being needed
          implicit val typeEnvironment = newNv
          implicit val span = node.span

          val declaredType = getTypeOfDeclaration(declSpecifiers, declarator)

          val name = getNameOfDeclarator(declarator).getOrElse(
            throw createError(
              "Abstract declaration not allowed here. You must give this declaration a name.",
              node.span
            )
          )
          newNv = newNv + (name -> declaredType)
        }

        newNv
      }

      case _ => context
    }
  }

  override protected def updateTypeMap(
      context: TypeEnvironment,
      typeMap: Map[AstID, Type],
      node: AstExt
  ): Map[Int, Type] = {
    node.node match {
      case AstExtKind.Identifier(name) => {
        typeMap + (node.id -> lookupType(node, context))
      }
      case _ => typeMap
    }
  }

}
