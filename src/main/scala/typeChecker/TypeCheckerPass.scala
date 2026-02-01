package tpagu.compiler.typeChecker

import tpagu.compiler.parser.AstExt
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.typeChecker.TypeEnvironment
import tpagu.compiler.AstID
import tpagu.compiler.parser.AstExtKind
import tpagu.compiler.typeChecker.NumT
import tpagu.compiler.parser.BinaryOp
import tpagu.compiler.typeChecker.createError
import tpagu.compiler.parser.PrefixOp
import tpagu.compiler.typeChecker.PtrT
import tpagu.compiler.parser.PostfixOp
import tpagu.compiler.typeChecker.FunT
import tpagu.compiler.CompilerError
import tpagu.compiler.typeChecker.getTypeOfDeclaration
import tpagu.compiler.typeChecker.getNameOfDeclarator
import tpagu.compiler.Span
import tpagu.compiler.typeChecker.getParameterMappings
import tpagu.compiler.typeChecker.NoneT

trait TypeCheckerPass {
  /*
   * typeMap is the map from the previous pass.
   */
  def check(
      e: AstExt,
      typeMap: TypeMap
  ): TypeMap;
}

