package tpagu.compiler.parser
import tpagu.compiler.lexer.*
import tpagu.compiler.*
import munit.FunSuite

class StatementTest extends FunSuite {

  // Helper methods
  def parseStatement(input: String): String = {
    val lexer = new Lexer(new File("test.txt", input))
    statement.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse statement: $err")
      case Right((ast, _)) => ast.toString()
    }
  }

  def parseTranslationUnit(input: String): String = {
    val lexer = new Lexer(new File("test.txt", input))
    translationUnit.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse translation unit: $err")
      case Right((ast, _)) => ast.toString()
    }
  }

  def testParseRule[A](input: String, rule:ParseRule[A]) = {
    val lexer = new Lexer(new File("test.txt", input))
    rule.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse translation unit: $err")
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
      "Declaration([Func(Some(a), [NumT(4,true) Var(Some(b)), NumT(4,true) Var(Some(c))])])"
    )
  }

  test("function declaration2") {
    assertEquals(
      parseStatement("int a(int, int);"),
      "Declaration([Func(Some(a), [NumT(4,true) Var(None), NumT(4,true) Var(None)])])"
    )
  }

  test("function declaration3") {
    assertEquals(
      parseStatement("int a(int(int, int c), int);"),
      "Declaration([Func(Some(a), [NumT(4,true) Func(None, [NumT(4,true) Var(None), NumT(4,true) Var(Some(c))]), NumT(4,true) Var(None)])])"
    )
  }

  test("abstract function declaration") {
    assertEquals(
      parseStatement(
        "int(int, int);"
      ), // should be valid syntactically but not semantically
      "Declaration([Func(None, [NumT(4,true) Var(None), NumT(4,true) Var(None)])])"
    )
  }

  test("function definition test") {
    assertEquals(
      parseStatement(
        "int a(int b) { int c; }"
      ),
      "FuncDef(NumT(4,true) Func(Some(a), [NumT(4,true) Var(Some(b))]), Block({ Declaration([Var(Some(c))]) }))"
    )
  }

  test("translation unit test 1") {
    assertEquals(
      parseTranslationUnit(
        """
        int f(int a, int b) {
            a + b;
        }
        int main() {
          int a = f(2, 3);
          a + 5;
        }
        """
      ),
      """TU({ FuncDef(NumT(4,true) Func(Some(f), [NumT(4,true) Var(Some(a)), NumT(4,true) Var(Some(b))]), Block({ ExprStmt(Add(Id(a), Id(b))) })); FuncDef(NumT(4,true) Func(Some(main), []), Block({ Declaration([Var(Some(a)) = Id(f){Call([Int(2), Int(3)])}]); ExprStmt(Add(Id(a), Int(5))) })) })"""
    )
  }

  test("translation unit test 2") {
    assertEquals(
      parseTranslationUnit(
        """
        int f(int a, int b) {
            return a + b;
        }
        int main() {
          int a = f(2, 3);
          int c = a + 5;
          return 0;
        }
        """
      ),
      """TU({ FuncDef(NumT(4,true) Func(Some(f), [NumT(4,true) Var(Some(a)), NumT(4,true) Var(Some(b))]), Block({ Return(Add(Id(a), Id(b))) })); FuncDef(NumT(4,true) Func(Some(main), []), Block({ Declaration([Var(Some(a)) = Id(f){Call([Int(2), Int(3)])}]); Declaration([Var(Some(c)) = Add(Id(a), Int(5))]); Return(Int(0)) })) })"""
    )
  }

  test("pointer test") {
    assertEquals(
      testParseRule("""
        *a
        """, declarator), "*Var(Some(a))"
      )
  }

  test("pointer test 2") {
    assertEquals(
      testParseRule("""
        **a
        """, declarator), "**Var(Some(a))"
      )
  }

}
