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
        if (!expectedContains.isBlank()) {
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

  test("Complex call") {
    expectTypePass("""
      int f() {
        return 5;
      }
      int c(int a, int f) {
        return a + f + 2;
      }
      int main() {
        int y = c(f(), c(2, 3));
      }
      """)
  }

  test("Complex call fail") {
    expectTypeError(
      """
      int f() {
        return 5;
      }
      int c(int a, int f) {
        return a + f + 2;
      }
      int main() {
        int y = c(f(2), c(2, 3));
      }
      """,
      ""
    )
  }

  test("Complex call 2") {
    expectTypePass("""
      int f() {
        return f() + 5;
      }
      int c(int a, int f) {
        return a + f + 2;
      }
      int main() {
        int y = c(f(), c(c(4,5), 3));
        int x = c(y, y);
      }
      """)
  }

  test("Complex call failure 2") {
    expectTypeError(
      """
      int f() {
        return f() + 5;
      }
      int c(int a, int f) {
        return a + f + 2;
      }
      int main() {
        int y = c(f(), c(c(4,5), 3));
        int x = c(c, y);
      }
      """,
      ""
    )
  }

  test("Complex call failure 3") {
    expectTypeError(
      """
      int f(int a) {
        return a + 2;
      }
      int g(int a) {
        return a;
      }

      int main() {
        int x = g(f);
      }
      """,
      ""
    )
  }

  test("Inner declarations shadow outer ones.") {
    expectTypePass("""
      int f(int a) {
        return a + 2;
      }
      int g(int a) {
        return a;
      }

      int main() {
        int f = 5;
        int x = g(f);
      }
      """)
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

  test("expect type checker fail assignment") {
    expectTypeError(
      """ 
      int f() {
      }
      int main() {
        int x;
        x = f;
      }
      """,
      ""
    )

    expectTypeError(
      """ 
      int f() {
      }
      int main() {
        2 = 3;
      }
      """,
      ""
    )

    expectTypeError(
      """ 
      int f() {
      }
      int main() {
        int x;
        2 = x;
      }
      """,
      ""
    )

    expectTypeError(
      """ 
      int f() {
      }
      int main() {
        int x = 2;
        int y;
        y = x = f;
      }
      """,
      ""
    )

    expectTypeError(
      """ 
      int f() {
      }
      int main() {
        int x = 2;
        int y;
        x = y = f;
      }
      """,
      ""
    )

    expectTypeError(
      """ 
      int f() {
      }
      int main() {
        int x = 2;
        int y;
        f = y = x;
      }
      """,
      ""
    )
  }

  test("expect type checker pass assignment") {
    expectTypePass(""" 
      int main() {
        int x = 1;
        x = 2;
      }
      """)

    expectTypePass(""" 
      int main() {
        int x = 1;
        int y = 2;
        y = x;
      }
      """)

    expectTypePass(""" 
      int main() {
        int x = 1;
        int y = 2;
        y = x = y;
      }
      """)

    expectTypePass(""" 
      int main() {
        int x = 1;
        int y = 2;
        x = x;
      }
      """)

    expectTypePass(""" 
      int main() {
        int x = 1;
        int y,z;
        y = z = x;
      }
      """)
  }

  test("pointer type tests") {
    expectTypePass(
      """
      int main() {
        int *x;
        int *y;
        x = y;
      }
      """
    )
    expectTypeError(
      """
      int main() {
        int *x;
        int y;
        x = y;
      }
      """,""
    )

  }

  test("const assignment") {
    expectTypePass(
      """
      int main() {
        const int x = 2;
        return 0;
      }
      """
      )
  }

}
