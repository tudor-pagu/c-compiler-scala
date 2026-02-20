package tpagu.compiler.ir

import tpagu.compiler.ir.IRGenerator
import munit.FunSuite
import tpagu.compiler.File
import tpagu.compiler.desugar.Desugar
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.typeChecker.{Type}

import scala.util.matching.Regex
import tpagu.compiler.typeChecker.CombinedTypeCheck
import tpagu.compiler.typeChecker.TypeMap

class IrInterpreterTest extends FunSuite {
  def irInterpHelper(input: String, expectedExitCode: Long): Unit = {
    val lexer = new Lexer(new File("test.txt", input))
    val ast = translationUnit.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast
    }
    implicit val typeMap: TypeMap = CombinedTypeCheck.check(ast)

    val coreAst = Desugar.desugar(ast)
    val program = IRGenerator.pub_lower(coreAst)
    val interpreter = IrInterpreter()
    val interpResult = interpreter.interp(program)
    assertEquals(interpResult, expectedExitCode, s"Generated ir: ${program.mkString("\n")}")
  }

  test("memory test 1") {
    var mem = Memory.zeroed(256)
    mem.storeAt(0, 10, 8)
    assertEquals(mem.loadAt(0, 8), 10L)
  }

  test("memory test 2") {
    var mem = Memory.zeroed(256)
    mem.storeAt(0, 127, 1)
    assertEquals(mem.loadAt(0, 8), 127L)
  }

  test("memory test 3") {
    var mem = Memory.zeroed(256)
    mem.storeAt(0, -128, 1)
    // we expect this to write
    // | 1000 000 | 0x00 | 0x00
    // to memory
    assertEquals(mem.loadAt(0, 1), -128L)
    assertEquals(mem.loadAt(0, 2), 128L)
    assertEquals(mem.loadAt(0, 4), 128L)
    assertEquals(mem.loadAt(0, 8), 128L)
  }

  test("memory test 4") {
    var mem = Memory.zeroed(256)
    mem.storeAt(0, -1, 1)
    // we expect this to write
    // | 0xFF | 0x00 | 0x00
    // to memory
    assertEquals(mem.loadAt(0, 1), -1L)
    assertEquals(mem.loadAt(0, 2), 255L)
    assertEquals(mem.loadAt(0, 4), 255L)
    assertEquals(mem.loadAt(0, 8), 255L)
  }

  test("memory test 5") {
    var mem = Memory.zeroed(256)
    mem.storeAt(0, -1, 2)
    assertEquals(mem.loadAt(0, 1), -1L)
  }

  test("test 1") {
    irInterpHelper(
      """
      int foo(int x) {
        return x + 1;
      }
      int main() {
        return 1;
      }
      """,
      1
    )
  }

  test("test 2") {
    irInterpHelper(
      """
      int foo(int x) {
        return x + 1;
      }
      int main() {
        return 2;
      }
      """,
      2
    )
  }

  test("test 3") {
    irInterpHelper(
      """
      int foo(int x) {
        return x + 1;
      }
      int main() {
        return foo(1);
      }
      """,
      2
    )
  }

  test("test 4") {
    irInterpHelper(
      """
      int foo(int x) {
        return x + 1;
      }
      int main() {
        int x = 5;
        return foo(x);
      }
      """,
      6
    )

    irInterpHelper(
      """
      int foo(int x, int y) {
        return x + 1;
      }
      int main() {
        int x = 5;
        return foo(x, x);
      }
      """,
      6
    )

    irInterpHelper(
      """
      int sum(int x, int y) {
        return x + y;
      }
      int main() {
        int x = 5;
        int y = x * 2;
        return sum(x, y);
      }
      """,
      15
    )
  }

  test("test assignment") {
    irInterpHelper(
      """ 
      int main() {
        int x = 2;
        x = 3;
        return x;
      }
      """,
      3
    )

    irInterpHelper(
      """ 
      int main() {
        int x = 2;
        x = 3;
        x = 4;
        return x;
      }
      """,
      4
    )

    irInterpHelper(
      """ 
      int main() {
        int x = 2;
        int y = 1;
        x = 3;
        y = 4;
        return x;
      }
      """,
      3
    )

    irInterpHelper(
      """ 
      int sum(int x, int y) {
        return x + y;
      }
      int main() {
        int x = 2;
        int y = 1;
        x = 3;
        y = 4;
        return sum(x,y);
      }
      """,
      7
    )

    irInterpHelper(
      """ 
      int sum(int x, int y) {
        return x + y;
      }
      int main() {
        int x = 2;
        int y = 1;
        x = y;
        return x;
      }
      """,
      1
    )

    irInterpHelper(
      """ 
      int sum(int x, int y) {
        return x + y;
      }
      int main() {
        int x = 2;
        int y = 1;
        x = y;
        return y;
      }
      """,
      1
    )

    irInterpHelper(
      """ 
      int sum(int x, int y) {
        return x + y;
      }
      int main() {
        int x = 2;
        int y = 1,z = 5;
        x = y = z;
        return sum(x,z);
      }
      """,
      10
    )

    irInterpHelper(
      """ 
      int sum(int x, int y) {
        return x + y;
      }
      int main() {
        int x = 2;
        int y = 1,z = 5;
        x = y = z;
        z = 1;
        return sum(x,z);
      }
      """,
      6
    )

    irInterpHelper(
      """
      int f(int x) {
        x = 1;
        return 0;
      }
      int main() {
        int x = 3;
        f(x);
        return x;
      }
      """,
      3
    )
  }

  test("test pointers") {
    irInterpHelper(
      """
      int main() {
        int *p;
        int a = 2;
        p = &a;
        return *p;
      }
      """,
      2
    )

    irInterpHelper(
      """
      int main() {
        int a = 3;
        int *p = &a;
        a = 4;
        return *p;
      }
      """,
      4
    )

    irInterpHelper(
      """
      int main() {
        int a = 2;
        int *p = &a;
        int *p2 = &a;
        a = 3;
        int c = *p + *p2;
        return c;
      }
      """,
      6
    )
  }

  test("test double pointers") {
    irInterpHelper(
      """
      int main() {
        int **p2;
        int *p;
        int a, c;

        p = &a;
        p2 = &p;
        int c = **p2;
        a = 42;
        return a;
      }
      """,
      42
    )
  }

  test("test pointer deref assignment") {
    irInterpHelper(
      """
      int main() {
        int a; 
        int *p = &a;
        *p = 42;
        return a;
      }
      """,
      42
    )
  }
  test("empty main returns 0") {
    irInterpHelper(
      """
      int main() {
      }
      """,
      0
    )
  }

  test("inner scope test") {
    irInterpHelper(
      """
      int main() {
        int a = 42;
        {
          int a = 3;
        }
        return a;
      }
      """,
      42
    )

    irInterpHelper(
      """
      int main() {
        int a = 42;
        {
          int b = 3;
        }
        return a;
      }
      """,
      42
    )

    irInterpHelper(
      """
      int main() {
        int a = 42;
        {
          a = 3;
        }
        return a;
      }
      """,
      3
    )

    irInterpHelper(
      """
      int main() {
        int a = 42;
        int b;
        {
          b = a;
          int a = 45;
        }
        return b;
      }
      """,
      42
    )
    irInterpHelper(
      """
      int main() {
        int a = 42;
        int b;
        {
          b = a;
          int a = 45;
          b = a;
        }
        return b;
      }
      """,
      45
    )

    irInterpHelper(
      """
      int main() {
        int a = 42;
        int b;
        {
          int a = 45;
          b = a;
        }
        b = a;
        return b;
      }
      """,
      42
    )
  }

  test("numeric limits test") {
    irInterpHelper(
      """
    int main() {
      int x;
      x = 2147483647;
      x = x + 1;
      return x;
    }
    """,
      -2147483648
    )
    irInterpHelper(
      """
    int main() {
      long long x;
      x = 2147483647;
      x = x + 1;
      return x;
    }
    """,
      2147483648L
    )
  }

  test("interpreter typedef tests") {
    irInterpHelper(
      """
      int main() {
        typedef long long ll;
        ll x = 2147483647;
        x = x + 1;
        return x;
      }
      """,
      2147483648L
    )
  }

  test("function pointer with any args") {
    irInterpHelper(
      """
      int foo() {
        return 42;
      }
      int main() {
        int (*p)() = foo;
        return p();

      }
      """,
      42
    )
  }

  test("function pointer with typedef") {
    irInterpHelper(
      """
      typedef long long ll;
      int foo(ll x, int y) {
          return x + y;
      }
      int main() {
          int (*p)(ll, int) = foo;
          ll x = 2;
          return p(x,3);
      }
      """,
      5
    )
  }
  test("function pointer with args") {
    irInterpHelper(
      """
      int foo(int x, int y) {
        return x + y;
      }
      int main() {
        int (*p)(int, int) = foo;
        return p(2,3);
      }
      """,
      5
    )

  }
  test("function pointer - reassignment") {
    irInterpHelper(
      """
    int f() { return 1; }
    int g() { return 2; }
    int main() {
      int (*p)() = f;
      p = g;
      return p();
    }
    """,
      2
    )
  }

  test("function pointer - passed as argument") {
    irInterpHelper(
      """
    int apply(int (*f)(int), int x) {
      return f(x);
    }
    int inc(int x) { return x + 1; }
    int main() {
      return apply(inc, 5);
    }
    """,
      6
    )
  }

  test("function pointer - returned from function") {
    irInterpHelper(
      """
    int add(int x, long long y) { return x + y; }
    int (*getfn())(int, long long) { return add; }
    int main() {
      int (*p)(int,long long ) = getfn();
      long long x = 4;
      int result = p(3, x);
      result = (result + 1) * 2;
      return result;
    }
    """,
      16
    )
  }

  test("function pointer - double pointer") {
    irInterpHelper(
      """
    int f() { return 42; }
    int main() {
      int (*p)() = f;
      int (**pp)() = &p;
      return (*pp)();
    }
    """,
      42
    )
  }

  test("function pointer - call through dereference") {
    irInterpHelper(
      """
    int f() { return 10; }
    int main() {
      int (*p)() = f;
      return (*p)();
    }
    """,
      10
    )
  }

  test("function pointer - nested calls") {
    irInterpHelper(
      """
    int f(int x) { return x * 2; }
    int g(int x) { return x + 1; }
    int main() {
      int (*pf)(int) = f;
      int (*pg)(int) = g;
      return pf(pg(3));
    }
    """,
      8
    )
  }

  test("immediate call") {
    irInterpHelper(
      """
    int add(int x, int y) { return x + y; }
    int (*getfn())(int, int) { return add; }
    int main() {
      return getfn()(2,3);
    }""",
      5
    )
  }

  test("basic array test") {
    irInterpHelper(
      """
      int main() {
        int a[10];
        *a = 10;
        return *a;
      }
      """,
      10
    )

  }

  test("interp dereferncing a reference of an lvalue") {
    irInterpHelper(
      """
      int main() {
        int a = 4;
        return *(&a);
      }
      """,
      4
    )
  }

  test("accessing other elements in array") {
    irInterpHelper("""
      int main() {
        int a[10];
        *a = 1;
        *(a + 1) = 2;
        return *a + *(a+1);
      }
      """,3)
  }
}

// TODO: implement this
//
// test("void argument list") {
//   irInterpHelper(
//     """
//     int foo(void) {
//       return 42;
//     }
//     int main() {
//       return foo();
//     }
//     """, 42
//   )
// }
// TODO: void
// test("function pointer test") {
//   irInterpHelper(
//     """
//     int foo() {
//       return 42;
//     }
//     int main() {
//       int (*p)(void) = foo;
//       return p();
//
//     }
//     """, 42)
// }
