import munit.FunSuite
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.File
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.CompilerError
import tpagu.compiler.parser.expression
import tpagu.compiler.typeChecker.TypeEnvironment
import tpagu.compiler.typeChecker.CombinedTypeCheck

class TypeCheckerTest extends FunSuite {

  // Helper methods
  def parseExpression(input: String): Type = {
    val lexer = new Lexer(new File("test.txt", input))
    val ast = expression.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast
    }
    val empty: Map[String, Type] = Map()
    val map = CombinedTypeCheck.check(ast)
    val t = map(ast)
    t
  }

  def expectParseError(input: String) = {
    val lexer = new Lexer(new File("test.txt", input))
    val ast = translationUnit.parse(lexer) match {
      case Left(err)       => {}
      case Right((ast, _)) => fail("Did not fail parser")
    }
  }

  def expectTypeError(input: String, expectedContains: String) = {
    val lexer = new Lexer(new File("test.txt", input))
    try {
      val ast = translationUnit.parse(lexer) match {
        case Left(err)       => fail(s"Could not parse expression: $err")
        case Right((ast, _)) => ast
      }
      val empty: Map[String, Type] = Map()
      val t = CombinedTypeCheck.check(ast)

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
    val t = CombinedTypeCheck.check(ast)
  }

  def testResultingType(input: String, expected: String) =
    assertEquals(parseExpression(input).toString(), expected)

  test("simple type checking 1") {
    testResultingType("2", "NumT(4,true,TypeQualifiers(false,false,false))")
  }

  test("simple type checking 2") {
    testResultingType("2 + 2", "NumT(4,true,TypeQualifiers(false,false,false))")
  }

  test("simple type checking 3") {
    testResultingType("2 - 2", "NumT(4,true,TypeQualifiers(false,false,false))")
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
      """,
      ""
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

  test("type conversion tests") {
    expectTypePass(
      """
      int main() {
        const int x = 2;
        int y = x;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int x = 2;
        const int y = x;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int x = 2;
        const int const y = x;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int x = 2;
        const int const y = x + 1;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int const x = 2;
        const int const y = x + 1;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int const x = 2;
        int y = x + 1;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int *x;
        int const * y = x;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int const *x;
        int const * y = x;
      }
      """
    )
    expectTypeError(
      """
      int main() {
        int const *x;
        int * y = x;
      }
      """,
      ""
    )
    expectTypeError(
      """
      int main() {
        int * const *x;
        int * * y = x;
      }
      """,
      ""
    )
    expectTypePass(
      """
      int main() {
        int * * const x;
        int * * y = x;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int * const x;
        int * y = x;
      }
      """
    )
    expectTypeError(
      """
      int main() {
        int * * x;
        int const * * y = x;
      }
      """,
      ""
    )

    expectTypeError(
      """
      int main() {
        int * * **x;
        int const * * * * y = x;
      }
      """,
      ""
    )

    expectTypeError(
      """
      int main() {
        int * * **x;
        int const * const * * * y = x;
      }
      """,
      ""
    )

    expectTypeError(
      """
      int main() {
        int * * **x;
        int const * const * * const * y = x;
      }
      """,
      ""
    )

    expectTypePass(
      """
      int main() {
        int * * **x;
        int const * const * const * const * y = x;
      }
      """
    )

    expectTypeError(
      """
      int main() {
        int const * * x;
        int * * y = x;
      }
      """,
      ""
    )
    expectTypeError(
      """
      int main() {
        int const * * * x;
        int * * * y = x;
      }
      """,
      ""
    )

    expectTypeError(
      """
      int main() {
        int ** x;
        int * * * y = x;
      }
      """,
      ""
    )

    expectTypePass(
      """
      int main() {
        int * * x;
        int const * const * y = x;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int * const p1;
        int * p2 = p1;
      }
      """
    )
    expectTypePass(
      """
      int main() {
        int * const p1;
        int * p2 = p1;
      }
      """
    )

  }

  test("usual arithmetic conversion 1") {
    expectTypePass("""
        int main() {
          long long x = 1;
          int y = 2;
          long long z = x + y;
          return 0;
        }
      """)
  }

  test("long type tests") {
    expectTypePass("""
      int main() {
        long x;
      }
      """)

    expectTypePass("""
      int main() {
        long x = 2;
      }
      """)

    expectTypePass("""
      int main() {
        long long x = 2;
      }
      """)

    expectTypeError(
      """
      int main() {
        long long x = 2;
        long long* p1 = &x;
        int* p2 = p1;
      }
      """,
      ""
    )
  }

  test("mixed pointer multiple inits") {
    expectTypePass(
      """
      int main() {
        int x = 2, *p = &x;
        return *p;
      }
      """
    )

    expectTypeError(
      """
      int main() {
        int x = 2, *p;
        p = 3;
      }
      """,
      ""
    )
  }

  test("typedef tests") {
    expectTypePass(
      """
      int main() {
        typedef int foo_t;
        foo_t x = 2;
        return x + 2;
      }
      """
    )

    expectTypePass(
      """
      int main() {
        typedef int foo_t;
        int y;
        foo_t x = 2;
        return x + 2;
      }
      """
    )

    expectTypePass(
      """
      int main() {
        typedef int foo_t;
        int y;
        {
          foo_t x = 2;
          return x + 2;
        }
      }
      """
    )

    expectParseError(
      """
      int main() {
        {
          typedef int foo_t;
        }
        foo_t a;
      }
      """
    )

    expectParseError(
      """
      int foo() {
        typedef int foo_t;
      }
      int main() {
        foo_t a;
      }
      """
    )
  }
  test("nested typedefs") {
    expectTypePass(
      """
      int main() {
        typedef int int_sub;
        typedef int_sub int_sub_2;
        int_sub_2 y = 2;
        return y + 1;
      }
      """
    )
  }

  test("more complex typedefs") {
    expectTypeError(
      """
      int main() {
        typedef int int_sub;
        long int_sub x;
      }
      """,
      ""
    )

    expectTypePass(
      """
      int main() {
        long long typedef ll;
        ll x;
      }
      """
    )
  }

  test("bad function pointers") {
    expectTypeError(
      """
      int foo(int x, int y) {
          return x + y;
      }
      int main() {
        int (*p)(long long, int) = foo;
        return p(2,3);
      }
      """,
      ""
    )

  }

  test("function to ptr decay 1") {
    expectTypePass(
      """
      int foo(int x, int y) {
        return x + y;
      }
      int main() {
        int (*p)(int ,int) = foo;
      }
      """
    )

    expectTypePass(
      """
      int foo(int x, int y) {
        return x + y;
      }
      int main() {
        int (*p)(int ,int);
        p = foo;
      }
      """
    )
  }

  test("function pointer decay - basic") {
    expectTypePass("""
    int f() { return 1; }
    int main() { int (*p)() = f; }
  """)
  }

  test("function pointer decay - pointer to function pointer") {
    expectTypePass("""
    int f() { return 1; }
    int main() { int (**pp)() = &f; }
  """)
  }

  test("function pointer decay - wrong return type") {
    expectTypeError(
      """
    int f() { return 1; }
    int main() { long long (*p)() = f; }
  """,
      ""
    )
  }

  test("function pointer decay - wrong param count") {
    expectTypeError(
      """
    int f(int x) { return x; }
    int main() { int (*p)() = f; }
  """,
      ""
    )
  }

  test("function pointer decay - wrong param type") {
    expectTypeError(
      """
    int f(int x) { return x; }
    int main() { int (*p)(long long) = f; }
  """,
      ""
    )
  }

  test("function pointer decay - function returning pointer") {
    expectTypePass("""
    int* f() { return 0; }
    int main() { int* (*p)() = f; }
  """)
  }

  test("function pointer decay - function returning pointer and typedef") {
    expectTypePass("""
    int* f() { return 0; }
    int main() { 
      typedef int int_type;
    int_type* (*p)() = f; 
    }
  """)
  }

  test("function pointer decay - function returning function pointer") {
    expectTypePass("""
    int g() { return 1; }
    int (*f())() { return g; }
    int main() { int (*(*p)())() = f; }
  """)
  }

  test("function pointer decay - const mismatch on return") {
    expectTypeError(
      """
    int* f() { return 0; }
    int main() { const int* (*p)() = f; }
  """,
      ""
    )
  }

  test("array definition") {
    expectTypePass("""
    int main() { 
      int a[100];
    }
  """)
  }

  test("array of pointers definition") {
    expectTypePass("""
    int main() { 
      int* a[100];
    }
  """)
  }

  test("pointer to array definition") {
    expectTypePass("""
    int main() { 
      int (*a)[100];
    }
  """)
  }

  test("array decay") {
    expectTypePass("""
    int main() { 
      int a[100];
      int *b = a;
    }
  """)
  }

  test("array doesn't decay to ptr to array, but to ptr to element type") {
    expectTypeError("""
      int main() {
        int a[100];
        int (*a_ptr)[100];
        a_ptr = a;
      }
      """, "")
  }

  test("array doesn't decay when you take reference of it") {
    expectTypePass("""
      int main() {
        int a[100];
        int (*a_ptr)[100];
        a_ptr = &a;
      }
      """)
  }


}
