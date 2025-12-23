package tpagu.compiler.desugar
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.typeChecker.NoneT

sealed trait AstC {
  def t:Type
}

// An expression can also double as a statement in the core AST.
// So if we encounter an expression at the first level inside of a block,
// we assume it was actually an expression statement. e.g ( `f(2,3);` or `2 + 3;` )
// Otherwise it would have been caught in the parser or type checker.
case class IntLiteral(value: Int, t: Type) extends AstC
case class Add(l:AstC , r:AstC , t: Type) extends AstC
case class Mult(l:AstC, r:AstC, t: Type) extends AstC
case class Neg(e:AstC, t: Type) extends AstC
case class Dereference(e:AstC, t:Type) extends AstC
case class AddressOf(e:AstC, t:Type) extends AstC
case class FunctionCall(callee: AstC, args: List[AstC], t: Type) extends AstC
case class Identifier(name: String, t: Type) extends AstC

case class Assignment(leftSide:AstC, rightSide: AstC) extends AstC {
  def t:Type = rightSide.t
}

// We are defining a variable (so we must allocate space for it right here, etc.) This can't be an extern statement.
case class VarDefinition(name: String,ofType: Type) extends AstC {
  def t:Type = NoneT()
}

case class FunctionDefinition(name: String, paramNames:List[String], body:AstC, ofType: Type) extends AstC { // t tells us the types of the params, while params tells us the names
  def t:Type = NoneT()
}

case class Block(statements: List[AstC]) extends AstC {
  def t:Type = NoneT()
}

case class TranslationUnit(statements: List[AstC]) extends AstC {
  def t:Type = NoneT()
}
case class Return(e: AstC) extends AstC {
  def t:Type = NoneT()
}
case class Cast(e: AstC, t: Type) extends AstC
// first execute this, then execute that. Both statements are executed
// in the same block/scope. Seq does *not* introduce a new scope, it's not {}
case class Seq(statements: List[AstC]) extends AstC {
  def t:Type = NoneT()
}
