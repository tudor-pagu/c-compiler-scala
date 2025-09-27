package tpagu.compiler.parser
import tpagu.compiler.lexer.*
import tpagu.compiler.*
import munit.FunSuite

class StatementTest extends FunSuite {

  // Helper methods
  def parseStatement(input: String): String = {
    val lexer = new Lexer(new File("test.txt", input))
    statement.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast.toString()
    }
  }

  def expectParseError(
      input: String,
      parser: ParseRule[AstExt]
  ): CompilerError = {
    val lexer = new Lexer(new File("test.txt", input))
    statement.parse(lexer) match {
      case Left(err) => err
      case Right(_)  => fail(s"Expected parse error for input: $input")
    }
  }

  test("parse simple integer literal") {
    assertEquals(
      parseStatement("int a = 2;"),
      "Declaration([Var(Some(a)) = Int(2)])"
    )
  }

  test("parse simple integer literal") {
    assertEquals(
      parseStatement("int a = 2, b = 3;"),
      "Declaration([Var(Some(a)) = Int(2), Var(Some(b)) = Int(3)])"
    )
  }

  test("function declaration1") {
    assertEquals(
      parseStatement("int a(int b, int c);"),
      "Declaration([Func(Some(a), [Type(Int,List()) Var(Some(b)), Type(Int,List()) Var(Some(c))])])"
    )
  }

  test("function declaration2") {
    assertEquals(
      parseStatement("int a(int, int);"),
      "Declaration([Func(Some(a), [Type(Int,List()) Var(None), Type(Int,List()) Var(None)])])"
    )
  }

  test("function declaration3") {
    assertEquals(
      parseStatement("int a(int(int, int c), int);"),
      "Declaration([Func(Some(a), [Type(Int,List()) Func(None, [Type(Int,List()) Var(None), Type(Int,List()) Var(Some(c))]), Type(Int,List()) Var(None)])])"
    )
  }

  test("abstract function declaration") {
    assertEquals(
      parseStatement(
        "int(int, int);"
      ), // should be valid syntactically but not semantically
      "Declaration([Func(None, [Type(Int,List()) Var(None), Type(Int,List()) Var(None)])])"
    )
  }

  test("block test 1") {
    assertEquals(
      parseStatement(
        "{int a; int b;}"
      ),
      "Block({ Declaration([Var(Some(a))]); Declaration([Var(Some(b))]) })"
    )
  }
  test("block test 2") {
    assertEquals(
      parseStatement(
        "{}"
      ),
      "Block({  })"
    )
  }
  
  test("block test 3") {
    assertEquals(
      parseStatement(
        "{int a;}"
      ),
      "Block({ Declaration([Var(Some(a))]) })"
    )
  }

  test("function definition test") {
    assertEquals(
      parseStatement(
        "int a(int b) { int c; }"
      ),
      "FuncDef(Type(Int,List()) Func(Some(a), [Type(Int,List()) Var(Some(b))]), Block({ Declaration([Var(Some(c))]) }))"
    )
  }

  test("function definition test") {
    assertEquals(
      parseStatement(
        "int a(int b) { int c; }"
      ),
      "FuncDef(Type(Int,List()) Func(Some(a), [Type(Int,List()) Var(Some(b))]), Block({ Declaration([Var(Some(c))]) }))"
    )
  }

}
