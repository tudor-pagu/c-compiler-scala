import tpagu.compiler.ir.IRGenerator
import munit.FunSuite
import tpagu.compiler.File
import tpagu.compiler.desugar.Desugar
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.typeChecker.{Type, TypeCheck}
import scala.util.matching.Regex
import tpagu.compiler.GoldCopyFunSuite
private val AnsiRegex: Regex = raw"\u001B\[[0-9;]*m".r
class IrGenTest extends GoldCopyFunSuite {
  private def normalize(s: String): String =
    AnsiRegex
      .replaceAllIn(s, "") // remove ANSI colors
      .replace("\r\n", "\n") // normalize Windows newlines
      .linesIterator
      .map(_.replaceFirst("\\s+$", "")) // trim end-of-line whitespace
      .mkString("\n")
      .trim // trim leading/trailing whitespace
  def irGenHelper(input: String): String = {
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
    val program = IRGenerator.pub_lower(coreAst)
    val programToString = program.mkString("\n")
    programToString
  }
  goldcopyTest("test 1") {
    irGenHelper(
      """
      int main() {
        int a = 2;
      }
      """
    )
  }
  goldcopyTest("test 2") {
    irGenHelper(
      """
      int main() {
      }
      """
    )
  }
  goldcopyTest("test 3") {
    irGenHelper(
      """
      int main() {
        int x = 1;
        int y = x + 1;
        return y;
      }
      """
    )
  }
  goldcopyTest("test 4 - complex expression") {
    irGenHelper(
      """
      int main() {
        int x = 1;
        int y = 2;
        return y + x + y - 2 + 5;
      }
      """
    )
  }
  goldcopyTest("test 5 - function expression") {
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
      """
    )
  }
  goldcopyTest("assignment tests 1") {
    irGenHelper(
      """
      int main() {
        int x,y = 1;
        x = y;
        return x;
      }
      """
    )
  }
  goldcopyTest("assignment tests 2") {
    irGenHelper(
      """
      int main() {
        int x,y,z = 1;
        x = y = z;
        return y;
      }
      """
    )
  }
  goldcopyTest("test foo with same param") {
    irGenHelper(
      """
      int foo(int x, int y) {
        return x + 1;
      }
      int main() {
        int x = 5;
        return foo(x, x);
      }
      """
    )
  }
  goldcopyTest("double pointer program") {
    irGenHelper(
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
      """
    )
  }

}
