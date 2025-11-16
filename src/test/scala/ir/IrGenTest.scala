import tpagu.compiler.ir.IRGenerator
import munit.FunSuite
import tpagu.compiler.File
import tpagu.compiler.desugar.Desugar
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.typeChecker.{Type, TypeCheck}

import scala.util.matching.Regex

private val AnsiRegex: Regex = raw"\u001B\[[0-9;]*m".r

class IrGenTest extends FunSuite {
  private def normalize(s: String): String =
    AnsiRegex
      .replaceAllIn(s, "") // remove ANSI colors
      .replace("\r\n", "\n") // normalize Windows newlines
      .linesIterator
      .map(_.replaceFirst("\\s+$", "")) // trim end-of-line whitespace
      .mkString("\n")
      .trim // trim leading/trailing whitespace

  def assertEqualsNormalized(obtained: String, expected: String)(using
      loc: munit.Location
  ): Unit =
    assertEquals(normalize(obtained), normalize(expected))

  def irGenHelper(input: String, expected: String): Unit = {
    val lexer = new Lexer(new File("test.txt", input))
    val ast = translationUnit.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast
    }
    val typeChecker = TypeCheck()
    val empty: Map[String, Type] = Map()
    typeChecker.typeOf(ast, empty)
    implicit val typeMap = typeChecker.typeMap
    val coreAst = Desugar.desugar(ast)
    val irgen = IRGenerator.newIrGenerator()
    val program = irgen.lower(coreAst)._1
    val programToString = program.mkString("\n")
    assertEqualsNormalized(programToString, expected)
  }

  test("test 1") {
    irGenHelper(
      """
      int main() {
        int a = 2;
      }
      """,
      """
main():
r1 <= alloc(4, 4)
store $2 => [r1]
"""
    )
  }

  test("test 2") {
    irGenHelper(
      """
      int main() {
      }
      """,
      """
main():
"""
    )
  }

  test("test 3") {
    irGenHelper(
      """
      int main() {
        int x = 1;
        int y = x + 1;
        return y;
      }
      """,
      """
main():
r1 <= alloc(4, 4)
store $1 => [r1]
r2 <= alloc(4, 4)
load [r1] => r3
r4 <= r3 + $1
store r4 => [r2]
load [r2] => r5
return r5
"""
    )
  }

  test("test 4 - complex expression") {
    irGenHelper(
      """
      int main() {
        int x = 1;
        int y = 2;
        return y + x + y - 2 + 5;
      }
      """,
      """
main():
r1 <= alloc(4, 4)
store $1 => [r1]
r2 <= alloc(4, 4)
store $2 => [r2]
load [r2] => r3
load [r1] => r4
r5 <= r3 + r4
load [r2] => r6
r7 <= r5 + r6
r8 <= -$2
r9 <= r7 + r8
r10 <= r9 + $5
return r10
"""
    )
  }

  test("test 5 - function expression") {
    irGenHelper(
      """
      int foo(int x, int y) {
        return x + y;
      }
      int main() {
        int x = 2;
        int z = 3;
        int c = foo(x,z);
        return c;
      }
      """,
      """
foo(r1, r2):
r3 <= alloc(4, 4)
store r1 => [r3]
r4 <= alloc(4, 4)
store r2 => [r4]
load [r3] => r5
load [r4] => r6
r7 <= r5 + r6
return r7
main():
r8 <= alloc(4, 4)
store $2 => [r8]
r9 <= alloc(4, 4)
store $3 => [r9]
r10 <= alloc(4, 4)
load [r8] => r11
load [r9] => r12
r13 <= foo(r11, r12)
store r13 => [r10]
load [r10] => r14
return r14 
"""
    )
  }

  test("assignment tests") {
    irGenHelper(
      """
      int main() {
        int x,y = 1;
        x = y;
        return x;
      }
      """,
      """
main():
r1 <= alloc(4, 4)
r2 <= alloc(4, 4)
store $1 => [r2]
load [r2] => r3
store r3 => [r1]
load [r1] => r4
return r4
      """
    )

    irGenHelper(
      """
      int main() {
        int x,y,z = 1;
        x = y = z;
        return y;
      }
      """,
      """
main():
r1 <= alloc(4, 4)
r2 <= alloc(4, 4)
r3 <= alloc(4, 4)
store $1 => [r3]
load [r3] => r4
store r4 => [r2]
load [r2] => r5
store r5 => [r1]
load [r2] => r6
return r6
      """
    )

  }

  test("test foo with same param") {
    irGenHelper(
      """
      int foo(int x, int y) {
        return x + 1;
      }
      int main() {
        int x = 5;
        return foo(x, x);
      }
      """,
      """
foo(r1, r2):
r3 <= alloc(4, 4)
store r1 => [r3]
r4 <= alloc(4, 4)
store r2 => [r4]
load [r3] => r5
r6 <= r5 + $1
return r6
main():
r7 <= alloc(4, 4)
store $5 => [r7]
load [r7] => r8
load [r7] => r9
r10 <= foo(r8, r9)
return r10
"""
    )
  }

}
