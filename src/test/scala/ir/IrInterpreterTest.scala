package tpagu.compiler.ir

import tpagu.compiler.ir.IRGenerator
import munit.FunSuite
import tpagu.compiler.File
import tpagu.compiler.desugar.Desugar
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.typeChecker.{Type, TypeCheck}

import scala.util.matching.Regex

class IrInterpreterTest extends FunSuite {
  def irInterpHelper(input: String, expectedExitCode: Long): Unit = {
    val lexer = new Lexer(new File("test.txt", input))
    val ast = translationUnit.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast
    }
    val typeChecker = TypeCheck()
    val empty: Map[String, Type] = Map()
    typeChecker.typeOf(ast, empty)
    implicit val typeMap = typeChecker.typeMap
    implicit val declarationTypeMap = typeChecker.declarationTypeMap
    val coreAst = Desugar.desugar(ast)
    val program = IRGenerator.pub_lower(coreAst)
    val interpreter = IrInterpreter()
    val interpResult = interpreter.interp(program)
    assertEquals(interpResult, expectedExitCode)
  }

  test("memory test 1") {
    var mem = Memory.zeroed(256)
    mem = mem.storeAt(0, 10, 8)
    assertEquals(mem.loadAt(0, 8), 10L)
  }

  test("memory test 2") {
    var mem = Memory.zeroed(256)
    mem = mem.storeAt(0, 127, 1)
    assertEquals(mem.loadAt(0, 8), 127L)
  }

  test("memory test 3") {
    var mem = Memory.zeroed(256)
    mem = mem.storeAt(0, -128, 1)
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
    mem = mem.storeAt(0, -1, 1)
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
    mem = mem.storeAt(0, -1, 2)
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

}
