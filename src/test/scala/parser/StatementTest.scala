package tpagu.compiler.parser
import tpagu.compiler.lexer.*
import tpagu.compiler.*
import tpagu.compiler.GoldCopyFunSuite

class StatementTest extends GoldCopyFunSuite {

  // Helper methods
  def parseStatement(input: String): String = {
    val lexer = new Lexer(new File("test.txt", input))
    statement.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse statement: $err")
      case Right((ast, _)) => ast.toString()
    }
  }

  def parseTranslationUnit(input: String): String = {
    val lexer = new Lexer(new File("test.txt", input))
    translationUnit.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse translation unit: $err")
      case Right((ast, _)) => ast.toString()
    }
  }

  def testParseRule[A](input: String, rule: ParseRule[A]): String = {
    val lexer = new Lexer(new File("test.txt", input))
    rule.parse(lexer) match {
      case Left(err)       => fail(s"Could not parse translation unit: $err")
      case Right((ast, _)) => ast.toString()
    }
  }

  goldcopyTest("parse simple integer literal") {
    parseStatement("int a = 2;")
  }

  goldcopyTest("parse simple integer literal with multiple declarations") {
    parseStatement("int a = 2, b = 3;")
  }

  goldcopyTest("function declaration1") {
    parseStatement("int a(int b, int c);")
  }

  goldcopyTest("function declaration2") {
    parseStatement("int a(int, int);")
  }

  goldcopyTest("function declaration3") {
    parseStatement("int a(int(int, int c), int);")
  }

  goldcopyTest("abstract function declaration") {
    parseStatement("int(int, int);")
  }

  goldcopyTest("function definition test") {
    parseStatement("int a(int b) { int c; }")
  }

  goldcopyTest("translation unit test 1") {
    parseTranslationUnit(
      """
        int f(int a, int b) {
            a + b;
        }
        int main() {
          int a = f(2, 3);
          a + 5;
        }
        """
    )
  }

  goldcopyTest("translation unit test 2") {
    parseTranslationUnit(
      """
        int f(int a, int b) {
            return a + b;
        }
        int main() {
          int a = f(2, 3);
          int c = a + 5;
          return 0;
        }
        """
    )
  }

  goldcopyTest("pointer test") {
    testParseRule("*a", declarator)
  }

  goldcopyTest("pointer test 2") {
    testParseRule("**a", declarator)
  }

  goldcopyTest("pointer test const") {
    testParseRule("*const a", declarator)
  }

  goldcopyTest("pointer test const const") {
    testParseRule("*const const a", declarator)
  }

  goldcopyTest("pointer test const star const") {
    testParseRule("*const * const a", declarator)
  }

  goldcopyTest("pointer test constconst not split") {
    testParseRule("*constconst a", declarator)
  }

  goldcopyTest("const declarations") {
    testParseRule("const int a = 42;", statement)
  }

  goldcopyTest("function pointer declaration 1") {
    testParseRule("int (*p)(int, int) = foo;", statement)
  }

  goldcopyTest("function pointer declaration 2") {
    testParseRule("int (*p)(void) = foo;", statement)
  }

  goldcopyTest("function pointer declaration 3") {
    testParseRule("int (*p)() = foo;", statement)
  }

  goldcopyTest("function pointer declaration 4") {
    testParseRule("int (*p)(int, long long);", statement)
  }
}
