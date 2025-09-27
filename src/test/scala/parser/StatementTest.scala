package tpagu.compiler.parser
import tpagu.compiler.lexer.*
import tpagu.compiler.*
import munit.FunSuite

class StatementTest extends FunSuite {
  
  // Helper methods
  def parseStatement(input: String): String = {
    val lexer = new Lexer(new File("test.txt", input))
    declaration.parse(lexer) match {
      case Left(err) => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast.toString()
    }
  }
  
  
  def expectParseError(input: String, parser: ParseRule[AstExt]): CompilerError = {
    val lexer = new Lexer(new File("test.txt", input))
    declaration.parse(lexer) match {
      case Left(err) => err
      case Right(_) => fail(s"Expected parse error for input: $input")
    }
  }
  
  
  test("parse simple integer literal") {
    assertEquals(parseStatement("int a = 2;"), "Declaration([Var(a) = Int(2)])")
  }
  
  test("parse simple integer literal") {
    assertEquals(parseStatement("int a = 2, b = 3;"), "Declaration([Var(a) = Int(2), Var(b) = Int(3)])")
  }

  test("function declaration1") {
    assertEquals(parseStatement("int a(int b, int c);"), "Declaration([Func(a, [Type(Int,List()) Some(Var(b)), Type(Int,List()) Some(Var(c))])])")
  }

  test("function declaration1") {
    assertEquals(parseStatement("int a(int, int);"), "Declaration([Func(a, [Type(Int,List()) None, Type(Int,List()) None])])")
  }
}

