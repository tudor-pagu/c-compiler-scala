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

abstract class PropagatingTypeChecker[Context] extends TypeCheckerPass {
  // get new context
  protected def initializeContext: Context
  protected def updateContext(
      context: Context,
      typeMap: TypeMap,
      node: AstExt
  ): Context = context

  protected def updateContextForChildren(
      context: Context,
      typeMap: TypeMap,
      node: AstExt
  ): Context = context

  protected def updateTypeMap(
      context: Context,
      typeMap: TypeMap,
      node: AstExt
  ): TypeMap = typeMap

  private def processNode(
      context: Context,
      typeMap: TypeMap,
      node: AstExt
  ): (Context, TypeMap) = {
    val newContext = updateContext(context, typeMap, node)
    val newTypeMap = updateTypeMap(context, typeMap, node)
    (newContext, newTypeMap)
  }

  override def check(e: AstExt, typeMap: TypeMap): TypeMap = {
    val ctx = initializeContext
    checkRec(ctx, typeMap, e)._2
  }

  // check recursively
  private def checkRec(
      context: Context,
      typeMap: TypeMap,
      e: AstExt
  ): (Context, TypeMap) = {

    val childrenCtx = updateContextForChildren(context, typeMap, e)

    e.node match {
      // Leaf nodes - no children
      case AstExtKind.IntLiteral(_) | AstExtKind.StringLiteral(_) |
          AstExtKind.CharLiteral(_) | AstExtKind.Identifier(_) |
          AstExtKind.Nothing => {
        processNode(context, typeMap, e)
      }

      // Binary - two children
      case AstExtKind.Binary(_, l, r) => {
        val (ctx1, tm1) = checkRec(childrenCtx, typeMap, l)
        val (ctx2, tm2) = checkRec(ctx1, tm1, r)
        processNode(context, tm2, e)
      }

      // Assignment - two children
      case AstExtKind.Assignment(left, right) => {
        val (ctx1, tm1) = checkRec(childrenCtx, typeMap, left)
        val (ctx2, tm2) = checkRec(ctx1, tm1, right)
        processNode(context, tm2, e)
      }

      // Prefix - one child
      case AstExtKind.PrefixOperation(_, inner) => {
        val (ctx1, tm1) = checkRec(childrenCtx, typeMap, inner)
        processNode(context, tm1, e)
      }

      // Postfix - one child
      case AstExtKind.PostfixOperation(op, inner) => {
        val (ctx1, tm1) = op match {
          case PostfixOp.FunctionCall(args) => {
            args.foldLeft((childrenCtx, typeMap))( (acc, argNode) => {
              checkRec(acc._1, acc._2, argNode)
            })
          }
          // case _ => {
          //   val (ctx1, tm1) = checkRec(childrenCtx, typeMap, inner)
          //   processNode(context, tm1, e)
          // }
        }
        val (_, tm2) = checkRec(ctx1, tm1, inner)
        processNode(context, tm2, e)
      }

      // ExprStatement - one child
      case AstExtKind.ExprStatement(inner) => {
        val (ctx1, tm1) = checkRec(childrenCtx, typeMap, inner)
        processNode(context, tm1, e)
      }

      // Return - one child
      case AstExtKind.Return(inner) => {
        val (ctx1, tm1) = checkRec(childrenCtx, typeMap, inner)
        processNode(context, tm1, e)
      }

      // FunctionDefinition - one child (body)
      case AstExtKind.FunctionDefinition(_, body) => {
        val (ctx1, tm1) = checkRec(childrenCtx, typeMap, body)
        processNode(context, tm1, e)
      }

      // Block - list of children
      case AstExtKind.Block(statements) => {
        val (ctx1, tm1) = statements.foldLeft((childrenCtx, typeMap)) {
          case ((ctx, tm), stmt) =>
            checkRec(ctx, tm, stmt)
        }
        processNode(context, tm1, e)
      }

      // TranslationUnit - list of children
      case AstExtKind.TranslationUnit(statements) => {
        val (ctx1, tm1) = statements.foldLeft((childrenCtx, typeMap)) {
          case ((ctx, tm), stmt) =>
            checkRec(ctx, tm, stmt)
        }
        processNode(context, tm1, e)
      }

      // DeclarationList - has optional AstExt in initDeclaratorList
      case AstExtKind.DeclarationList(_, initDeclaratorList) => {
        val (ctx1, tm1) = initDeclaratorList.foldLeft((childrenCtx, typeMap)) {
          case ((ctx, tm), (_, optInit)) =>
            optInit match {
              case Some(init) => checkRec(ctx, tm, init)
              case None       => (ctx, tm)
            }
        }
        processNode(context, tm1, e)
      }
    }
  }

}
