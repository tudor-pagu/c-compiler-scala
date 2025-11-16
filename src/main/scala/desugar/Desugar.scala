package tpagu.compiler.desugar

import java.util.IdentityHashMap
import tpagu.compiler.parser.AstExt
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.parser.AstExtKind
import java.util.function.BinaryOperator
import tpagu.compiler.parser.BinaryOp
import tpagu.compiler.parser.PrefixOp
import tpagu.compiler.parser.PostfixOp
import tpagu.compiler.typeChecker.getTypeOfDeclaration
import tpagu.compiler.Span
import tpagu.compiler.parser.AstExtKind.ExprStatement

object Desugar {
  // the typeMap is produced by the TypeChecker as it type checks a program.
  // It will cache the type of every AstExt it encounters so that we can use it in the
  // desugarer
  def desugar(e: AstExt)(implicit typeMap: IdentityHashMap[AstExt, Type]): AstC = {
    e.node match {
      case AstExtKind.IntLiteral(num) => IntLiteral(num, typeMap.get(e))
      case AstExtKind.Binary(op, l, r) =>
        op match {
          case BinaryOp.Add => Add(desugar(l), desugar(r), typeMap.get(e))
          case BinaryOp.Sub => Add(desugar(l), Neg(desugar(r), typeMap.get(r)), typeMap.get(e))
          // TODO add more
          case BinaryOp.Mult => Mult(desugar(l), desugar(r), typeMap.get(e))
        }
      case AstExtKind.PrefixOperation(op, inner) => 
        op match {
          case PrefixOp.Negation => Neg(desugar(inner), typeMap.get(e))
          case PrefixOp.UnaryPlus => Cast(desugar(inner), typeMap.get(e))
        }
      case AstExtKind.PostfixOperation(op, inner) => op match {
        case PostfixOp.FunctionCall(args) => FunctionCall(desugar(inner), args.map(arg=>desugar(arg)), typeMap.get(e))
      }
      case AstExtKind.Identifier(name) => Identifier(name, typeMap.get(e))
      case AstExtKind.DeclarationList(declSpecifiers, initDeclaratorList) => Seq(initDeclaratorList.map(initDecl => {
          assert(initDecl._1.direct.getName().isDefined)

          implicit val span: Span = e.span // at this point, we should *never* get a compiler error when deducing this type, since it got typechecked.
          val declaredType = getTypeOfDeclaration(declSpecifiers, initDecl._1)
          val init = initDecl._2.map(i => Desugar.desugar(i))
          val varDef = VarDefinition(initDecl._1.direct.getName().get, declaredType)
          if (init.isDefined) then {
            Seq(List(varDef, Assignment(Identifier(initDecl._1.direct.getName().get, declaredType), init.get)))
          } else {
            varDef
          }
        }
      ))
      case AstExtKind.FunctionDefinition(declaration, body) => {
        declaration.declarator.direct match {
          case tpagu.compiler.parser.DirectDeclarator.Function(name, params) => {
            FunctionDefinition(name.get, params.map(param => param.declarator.direct.getName().get), desugar(body), typeMap.get(e))
          }
          case _ => throw RuntimeException("Didnt get function definition in desugarer when it was expected.")
        }
      }

      case AstExtKind.Block(statements) => Block(statements.map(desugar(_)))
      case AstExtKind.TranslationUnit(statements) => TranslationUnit(statements.map(desugar(_)))
      case AstExtKind.ExprStatement(e) => desugar(e)
      case AstExtKind.Nothing => throw RuntimeException("tried to desugar Nothing, this should get filtered out ahead of time.")
      case AstExtKind.Return(e) => Return(desugar(e))
      case AstExtKind.Assignment(left, right) => Assignment(desugar(left), desugar(right))
    }
  }

}
