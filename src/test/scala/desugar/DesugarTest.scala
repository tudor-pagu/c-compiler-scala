import munit.FunSuite
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.File
import tpagu.compiler.parser.translationUnit
import tpagu.compiler.typeChecker.TypeCheck
import tpagu.compiler.typeChecker.Type
import tpagu.compiler.desugar.Desugar

class DesugarTest extends FunSuite {
  def desugarHelper(input: String, expected: String): Unit = {
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
    assertEquals(coreAst.toString(), expected)
  }

  test("test 1") {
    desugarHelper("""
      int main() {
        int a = 2;
      }
      """, "TranslationUnit(List(FunctionDefinition(main,List(),Block(List(Seq(List(Seq(List(VarDefinition(a,NumT(4,true)), Assignment(a,IntLiteral(2,NumT(4,true))))))))),NoneT())))")
  }
  test("test subtraction") {
    desugarHelper("""
      int main() {
        int a = 2 - 3;
      }
      """, "TranslationUnit(List(FunctionDefinition(main,List(),Block(List(Seq(List(Seq(List(VarDefinition(a,NumT(4,true)), Assignment(a,Add(IntLiteral(2,NumT(4,true)),Neg(IntLiteral(3,NumT(4,true)),NumT(4,true)),NumT(4,true))))))))),NoneT())))")
  }
  test("test subtraction") {
    desugarHelper("""
      int f(int a, int b) {
        return a + b + 1;
      }
      int main() {
        int a = 2 - 3;
        int c = +3;
        5;
        return f(a, c);
      }
      """, "TranslationUnit(List(FunctionDefinition(f,List(a, b),Block(List(Return(Add(Add(Identifier(a,NumT(4,true)),Identifier(b,NumT(4,true)),NumT(4,true)),IntLiteral(1,NumT(4,true)),NumT(4,true))))),NoneT()), FunctionDefinition(main,List(),Block(List(Seq(List(Seq(List(VarDefinition(a,NumT(4,true)), Assignment(a,Add(IntLiteral(2,NumT(4,true)),Neg(IntLiteral(3,NumT(4,true)),NumT(4,true)),NumT(4,true))))))), Seq(List(Seq(List(VarDefinition(c,NumT(4,true)), Assignment(c,Cast(IntLiteral(3,NumT(4,true)),NumT(4,true))))))), IntLiteral(5,NumT(4,true)), Return(FunctionCall(Identifier(f,FunT(NumT(4,true),List(NumT(4,true), NumT(4,true)))),List(Identifier(a,NumT(4,true)), Identifier(c,NumT(4,true))),NumT(4,true))))),NoneT())))")
  }
}
