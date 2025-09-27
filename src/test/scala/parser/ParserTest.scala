package tpagu.compiler.parser
import tpagu.compiler.lexer.*
import tpagu.compiler.*
import munit.FunSuite

class ParserTest extends FunSuite {
  
  // Helper methods
  def parseExpression(input: String): String = {
    val lexer = new Lexer(new File("test.txt", input))
    expression.parse(lexer) match {
      case Left(err) => fail(s"Could not parse expression: $err")
      case Right((ast, _)) => ast.toString()
    }
  }
  
  
  def expectParseError(input: String, parser: ParseRule[AstExt]): CompilerError = {
    val lexer = new Lexer(new File("test.txt", input))
    parser.parse(lexer) match {
      case Left(err) => err
      case Right(_) => fail(s"Expected parse error for input: $input")
    }
  }
  
  
  // Literal parser tests
  test("parse simple integer literal") {
    val result = parseExpression("42")
    assertEquals(result, "Int(42)")
  }
  
  test("parse zero literal") {
    val result = parseExpression("0")
    assertEquals(result.toString, "Int(0)")
  }

  // this fails now. TODO: Support long long literals
  // test("parse large number literal") {
  //   val result = parseExpression("12345678901")
  //   assertEquals(result.toString, "Int(12345678901)")
  // }

  test("literal parser fails on non-number") {
    val error = expectParseError("abc", literal)
    assert(error.message.contains("Could not parse literal"))
  }

  // Identifier parser tests
  test("parse simple identifier") {
    val result = parseExpression("hello")
    assertEquals(result.toString, "Id(hello)")
  }

  test("parse identifier with numbers") {
    val result = parseExpression("var123")
    assertEquals(result.toString, "Id(var123)")
  }

  test("parse camelCase identifier") {
    val result = parseExpression("myVariable")
    assertEquals(result.toString, "Id(myVariable)")
  }

  test("identifier parser fails on number") {
    val error = expectParseError("123", identifier)
    assert(error.message.contains("Could not parse identifier"))
  }

  test("atom parses parenthesized expression") {
    val result = parseExpression("(42)")
    assertEquals(result.toString, "Int(42)")
  }

  test("atom parses complex parenthesized expression") {
    val result = parseExpression("(2 + 3)")
    assertEquals(result.toString, "Add(Int(2), Int(3))")
  }

  test("atom fails on unmatched parenthesis") {
    val error = expectParseError("(42", atom)
    assert(error.message.contains("Expected") && error.message.contains("CloseParen"))
  }

  // Unary expression tests
  test("parse unary positive") {
    val result = parseExpression("+42")
    assertEquals(result.toString, "Plus(Int(42))")
  }

  test("parse unary negative") {
    val result = parseExpression("-123")
    assertEquals(result.toString, "Neg(Int(123))")
  }

  test("parse unary positive zero") {
    val result = parseExpression("+0")
    assertEquals(result.toString, "Plus(Int(0))")
  }

  test("parse unary negative zero") {
    val result = parseExpression("-0")
    assertEquals(result.toString, "Neg(Int(0))")
  }

  test("parse double negative") {
    val result = parseExpression("- -1")
    assertEquals(result.toString, "Neg(Neg(Int(1)))")
  }

  test("parse double positive") {
    val result = parseExpression("+ +1")
    assertEquals(result.toString, "Plus(Plus(Int(1)))")
  }

  test("parse double positive with parenthesis") {
    val result = parseExpression("+(+1)")
    assertEquals(result.toString, "Plus(Plus(Int(1)))")
  }

  test("parse mixed unary operators") {
    val result = parseExpression("+ +-+1")
    assertEquals(result.toString, "Plus(Plus(Neg(Plus(Int(1)))))")
  }

  test("unary expression with identifier") {
    val result = parseExpression("-x")
    assertEquals(result.toString, "Neg(Id(x))")
  }

  test("unary expression with parentheses") {
    val result = parseExpression("-(2 + 3)")
    assertEquals(result.toString, "Neg(Add(Int(2), Int(3)))")
  }

  // Secondary expression tests (multiplication and division)
  test("parse simple multiplication") {
    val result = parseExpression("2 * 3")
    assertEquals(result.toString, "Mult(Int(2), Int(3))")
  }

  test("parse simple division") {
    val result = parseExpression("8 / 2")
    assertEquals(result.toString, "Div(Int(8), Int(2))")
  }

  test("parse multiplication chain") {
    val result = parseExpression("2 * 3 * 4")
    assertEquals(result.toString, "Mult(Mult(Int(2), Int(3)), Int(4))")
  }
  //
  test("parse division chain") {
    val result = parseExpression("24 / 4 / 2")
    assertEquals(result.toString, "Div(Div(Int(24), Int(4)), Int(2))")
  }

  test("parse mixed multiplication and division") {
    val result = parseExpression("12 / 3 * 2")
    assertEquals(result.toString, "Mult(Div(Int(12), Int(3)), Int(2))")
  }

  test("parse multiplication with division") {
    val result = parseExpression("2 * 3 / 4")
    assertEquals(result.toString, "Div(Mult(Int(2), Int(3)), Int(4))")
  }

  test("secondary expression with unary") {
    val result = parseExpression("-2 * -3")
    assertEquals(result.toString, "Mult(Neg(Int(2)), Neg(Int(3)))")
  }

  test("secondary expression single term") {
    val result = parseExpression("42")
    assertEquals(result.toString, "Int(42)")
  }

  // Primary expression tests (addition and subtraction)
  test("parse simple addition") {
    val result = parseExpression("2 + 3")
    assertEquals(result.toString, "Add(Int(2), Int(3))")
  }

  test("parse simple subtraction") {
    val result = parseExpression("5 - 2")
    assertEquals(result.toString, "Sub(Int(5), Int(2))")
  }

  test("parse addition chain") {
    val result = parseExpression("1 + 2 + 3")
    assertEquals(result.toString, "Add(Add(Int(1), Int(2)), Int(3))")
  }

  test("parse subtraction chain") {
    val result = parseExpression("10 - 3 - 2")
    assertEquals(result.toString, "Sub(Sub(Int(10), Int(3)), Int(2))")
  }

  test("parse mixed addition and subtraction") {
    val result = parseExpression("5 + 3 - 1")
    assertEquals(result.toString, "Sub(Add(Int(5), Int(3)), Int(1))")
  }

  test("primary expression with unary") {
    val result = parseExpression("2 + -3")
    assertEquals(result.toString, "Add(Int(2), Neg(Int(3)))")
  }

  test("primary expression with both unary") {
    val result = parseExpression("-2 + -3")
    assertEquals(result.toString, "Add(Neg(Int(2)), Neg(Int(3)))")
  }
  test("division has higher precedence than addition") {
    val result = parseExpression("2 + 8 / 4")
    assertEquals(result.toString, "Add(Int(2), Div(Int(8), Int(4)))")
  }

  test("multiplication has higher precedence than subtraction") {
    val result = parseExpression("10 - 2 * 3")
    assertEquals(result.toString, "Sub(Int(10), Mult(Int(2), Int(3)))")
  }

  test("addition before multiplication") {
    val result = parseExpression("2 * 3 + 4")
    assertEquals(result.toString, "Add(Mult(Int(2), Int(3)), Int(4))")
  }

  test("complex mixed expression") {
    val result = parseExpression("1 + 2 * 3 / 6 + 4")
    assertEquals(result.toString, "Add(Add(Int(1), Div(Mult(Int(2), Int(3)), Int(6))), Int(4))")
  }

  test("multiplication chain with addition") {
    val result = parseExpression("5 + 2 * 3 * 4")
    assertEquals(result.toString, "Add(Int(5), Mult(Mult(Int(2), Int(3)), Int(4)))")
  }
  // Parentheses tests
  test("parentheses override precedence") {
    val result = parseExpression("2 * (3 + 4)")
    assertEquals(result.toString, "Mult(Int(2), Add(Int(3), Int(4)))")
  }

  test("nested parentheses") {
    val result = parseExpression("((2 + 3))")
    assertEquals(result.toString, "Add(Int(2), Int(3))")
  }

  test("complex parentheses expression") {
    val result = parseExpression("(2 + 3) * (4 - 1)")
    assertEquals(result.toString, "Mult(Add(Int(2), Int(3)), Sub(Int(4), Int(1)))")
  }

  test("parentheses with unary") {
    val result = parseExpression("-(2 + 3)")
    assertEquals(result.toString, "Neg(Add(Int(2), Int(3)))")
  }

  test("unmatched opening parenthesis fails") {
    val error = expectParseError("(2 + 3", expression)
    assert(error.message.contains("Expected") && error.message.contains("CloseParen"))
  }
}
