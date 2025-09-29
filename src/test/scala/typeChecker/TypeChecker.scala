import munit.FunSuite
import tpagu.compiler.typeChecker.TypeCheck.typeOf
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.File
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.CompilerError
import tpagu.compiler.parser.expression
import tpagu.compiler.typeChecker.TypeEnvironment

class TypeCheckerTest extends FunSuite {

  // Helper methods
  def parseExpression(input: String): (Type, TypeEnvironment) = {
    val lexer = new Lexer(new File("test.txt", input))
    val ast = expression.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast
    }
    val empty: Map[String, Type] = Map()
    val t = typeOf(ast, empty)
    t
  }

  def expectTypeError(input: String, expectedContains: String) = {
    val lexer = new Lexer(new File("test.txt", input))
    try {
      val ast = translationUnit.parse(lexer) match {
        case Left(err)       => fail(s"Could not parse expression: $err")
        case Right((ast, _)) => ast
      }
      val empty: Map[String, Type] = Map()
      val t = typeOf(ast, empty)

      fail("Did not fail the type checker.")
    } catch {
      case c @ CompilerError(msg, span, _) => {
        assert(
          msg.contains(expectedContains),
          s"""
        |Expected error message to contain:
        |  '$expectedContains'
        |But got:
        |  '$msg'
        |
        |While checking code:
        |$input
        with span: ${span.start} -> ${span.end}
        """.stripMargin
        )
      }
      case x => throw x
    }
  }

  def expectTypePass(input: String) = {
    val lexer = new Lexer(new File("test.txt", input))
    val ast = translationUnit.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast
    }
    val empty: Map[String, Type] = Map()
    val t = typeOf(ast, empty)
  }

  def testResultingType(input: String, expected: String) =
    assertEquals(parseExpression(input)._1.toString(), expected)

  test("simple type checking") {
    testResultingType("2 + 2", "NumT(4,true)")
  }

  test("simple type checking") {
    testResultingType("2 - 2", "NumT(4,true)")
  }

  test("failure 1") {
    expectTypeError(
      """
      int main() {
        f(2,3,5);
      }
      """,
      "Undefined identifier: f"
    )
  }

  test("function introduces parameters into type environment") {
    expectTypePass("""
      int f(int a, int b) {
        return a + b;
      }
      """)
  }

  test("argument correct pass 1") {
    expectTypePass("""
      int f(int a, int b) {
        return a + b;
      }
      int main() {
        f(2, 3);
      }
      """)
  }

  test("argument mismatch failure 1") {
    expectTypeError(
      """
      int f(int a, int b) {
        return a + b;
      }
      int main() {
        f(2, 3, 4);
      }
      """,
      "Tried to call a function with 3 arguments, when the function takes 2 parameters"
    )
  }

  test("argument mismatch failure 2") {
    expectTypeError(
      """
      int f(int a, int b) {
        return a + b;
      }
      int main() {
        f();
      }
      """,
      "Tried to call a function with 0 arguments, when the function takes 2 parameters"
    )
  }

  test("argument mismatch failure 3") {
    expectTypeError(
      """
      int f() {
        return 5;
      }
      int main() {
        f(2);
      }
      """,
      "Tried to call a function with 1 arguments, when the function takes 0 parameters"
    )
  }

  test("argument mismatch failure 4") {
    expectTypeError(
      """
      int f() {
        return 5;
      }
      int main() {
        int x = f(2);
      }
      """,
      "Tried to call a function with 1 arguments, when the function takes 0 parameters"
    )
  }

  test("declared type mismatch failure") {
    expectTypeError(
      """
      int f() {
        return 5;
      }
      int main() {
        int x(int, int) = f(2);
      }
      """,
      "Tried to call a function with 1 arguments, when the function takes 0 parameters"
    )
  }
  
  test("correct function call 1") {
    expectTypePass(
      """
      int f() {
        return 5;
      }
      int main() {
        f();
      }
      """
    )
  }

}
