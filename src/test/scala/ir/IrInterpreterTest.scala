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
    val coreAst = Desugar.desugar(ast)
    val irgen = IRGenerator.newIrGenerator()
    val program = irgen.lower(coreAst)._1
    val interpreter = IrInterpreter()
    val interpResult = interpreter.interp(program)
    assertEquals(interpResult, expectedExitCode)
  }

  test("memory test 1") {
    var mem = Memory.zeroed(256)
    mem = mem.storeAt(0, 10, 8)
    assertEquals(mem.loadAt(0,8), 10L)
  }
  
  test("memory test 2") {
    var mem = Memory.zeroed(256)
    mem = mem.storeAt(0, 127, 1)
    assertEquals(mem.loadAt(0,8), 127L)
  }

  test("memory test 3") {
    var mem = Memory.zeroed(256)
    mem = mem.storeAt(0, -128, 1)
    // we expect this to write
    // | 1000 000 | 0x00 | 0x00
    // to memory
    assertEquals(mem.loadAt(0,1), -128L)
    assertEquals(mem.loadAt(0,2), 128L)
    assertEquals(mem.loadAt(0,4), 128L)
    assertEquals(mem.loadAt(0,8), 128L)
  }

  test("memory test 4") {
    var mem = Memory.zeroed(256)
    mem = mem.storeAt(0, -1, 1)
    // we expect this to write
    // | 0xFF | 0x00 | 0x00
    // to memory
    assertEquals(mem.loadAt(0,1), -1L)
    assertEquals(mem.loadAt(0,2), 255L)
    assertEquals(mem.loadAt(0,4), 255L)
    assertEquals(mem.loadAt(0,8), 255L)
  }

  test("memory test 5") {
    var mem = Memory.zeroed(256)
    mem = mem.storeAt(0, -1, 2)
    assertEquals(mem.loadAt(0,1), -1L)
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
