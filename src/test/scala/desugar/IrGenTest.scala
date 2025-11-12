import munit.FunSuite
import tpagu.compiler.desugar.IRGenerator
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.File
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.typeChecker.TypeCheck
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.desugar.Desugar
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
r1 <= alloc(4, 4)                                                                                                                                              
r2 <= alloc(4, 4)
load [r1] => r3
load [r2] => r4
r5 <= r3 + r4
return r5
main():
r6 <= alloc(4, 4)
store $2 => [r6]
r7 <= alloc(4, 4)
store $3 => [r7]
r8 <= alloc(4, 4)
load [r6] => r9
load [r7] => r10
r11 <= foo(r9, r10)
store r11 => [r8]
load [r8] => r12
return r12
"""
    )
  }


  // test("test 2") {
  //   irGen
  // }

}
