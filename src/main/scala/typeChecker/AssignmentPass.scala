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

def isLvalue(e: AstExt, typeMap: TypeMap): Boolean =
  e.node match {
    case AstExtKind.Identifier(name) => {
      typeMap(e) match {
        case FunT(_, _) => false
        case _          => true
      }
    }
    case AstExtKind.PrefixOperation(PrefixOp.Dereference, e) => {
      true
    }
    case _ => false
    // TODO add more cases, for now we just support identifiers.
  }

class AssignmentPass extends PropagatingTypeChecker[Unit] {
  override protected def initializeContext: Unit = ()

  override protected def updateTypeMap(
      context: Unit,
      typeMap: TypeMap,
      node: AstExt
  ): TypeMap = {
    node.node match {
      case AstExtKind.Assignment(left, right) => {
        val leftT = typeMap(left)
        val rightT = typeMap(right)

        if !isLvalue(left, typeMap) then {
          throw createError(s"Trying to assign to non-lvalue.", left.span)
        }

        if (leftT != rightT) {
          throw createError(
            s"Can't assign type ${rightT.prettyName()} to type ${leftT.prettyName()}",
            node.span
          )
        }
        typeMap + (node -> rightT)
      }
      case AstExtKind.DeclarationList(declSpecifiers, initDeclaratorList) => {
        val declaredTypes = typeMap.getTypesOfDeclaration(node)
        assert(declaredTypes.size == initDeclaratorList.size)

        initDeclaratorList.zip(declaredTypes).foreach( ( declPair, declaredType) => {
          declPair._2 match {
            case Some(initValue) => {
              val initType = typeMap(initValue)
              if (declaredType != initType) {
                throw createError(s"Tried to initialize `${declPair._1.getName().get}` which is declared to be of type ${declaredType} with initializer of type ${initType}", node.span)
              }
            }
            case None => {}
          }
        })

        typeMap
      }
      case _ => typeMap
    }
  }

}
