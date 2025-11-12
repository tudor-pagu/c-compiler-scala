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
  def irInterpHelper(input: String, expectedExitCode: Long, expectedDebugStmts:List[Long] = List()): Unit = {
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
    val interpreter = IrInterpreter()
    val interpResult = interpreter.interp(program)
    assertEquals(interpResult._2, expectedExitCode)
    assertEquals(interpResult._1, expectedDebugStmts)
  }

  test("memory test 1") {
    val mem = Memory(Array.ofDim(100))
    mem.storeAt(0, 10, 8)
    assertEquals(mem.loadAt(0,8), 10L)
  }
  
  test("memory test 2") {
    val mem = Memory(Array.ofDim(100))
    mem.storeAt(0, 127, 1)
    assertEquals(mem.loadAt(0,8), 127L)
  }

  test("memory test 3") {
    val mem = Memory(Array.ofDim(100))
    mem.storeAt(0, -128, 1)
    assertEquals(mem.loadAt(0,8), -128L)
  }

  // test("test 1") {
  //   irInterpHelper(
  //     """
  //     int foo(int x) {
  //       return x + 1;
  //     }
  //     int main() {
  //       return 1;
  //     }
  //     """,1)
  // }
  //
}
