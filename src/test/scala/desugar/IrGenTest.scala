import munit.FunSuite
import tpagu.compiler.desugar.IRGenerator
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.File
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.typeChecker.TypeCheck
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.desugar.Desugar

class IrGenTest extends FunSuite {
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
    assertEquals(program.toString(), expected)
  }

  test("test 1") {
    irGenHelper("""
      int main() {
        int a = 2;
      }
      """, 
      """ 
      """)
  }

}
