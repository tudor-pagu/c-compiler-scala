package tpagu.compiler.desugar
import tpagu.compiler.typeChecker.Type

sealed trait AstC {}

case class IntLiteral(value: Int, t: Type) extends AstC
case class Add(l:IntLiteral, r: IntLiteral, t: Type) extends AstC
case class Mult(l:IntLiteral, r: IntLiteral, t: Type) extends AstC
case class Neg(e: IntLiteral, t: Type) extends AstC
case class FunctionCall(callee: AstC, args: List[AstC], t: Type) extends AstC
case class Identifier(name: String, t: Type) extends AstC

case class Declaration(name: String,t: Type, init:Option[AstC]) extends AstC
case class FunctionDefinition(name: String, paramNames:List[String], t: Type) extends AstC // t tells us the types of the params, while params tells us the names
case class Block(statements: List[AstC]) extends AstC
case class TranslationUnit(statements: List[AstC]) extends AstC
case class Return(e: AstC)
