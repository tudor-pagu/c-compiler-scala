class LexerTest extends munit.FunSuite {
  
  // Helper methods
  def tokenize(input: String): Token = {
    val lexer = new Lexer(new File("test.txt", input))
    val (token, _) = lexer.nextToken()
    token
  }
  
  def tokenizeAll(input: String): List[Token] = {
    var lexer = new Lexer(new File("test.txt", input))
    var tokens = List.empty[Token]
    
    var continue = true
    while (continue) {
      lexer.nextToken() match {
        case (token, nextLexer) =>
          tokens = tokens :+ token
          if (token == Token.EOF) continue = false
          else lexer = nextLexer
        case error: CompilerError =>
          fail(s"Unexpected error: $error")
      }
    }
    tokens
  }
  
  // Single token tests
  test("single character tokens") {
    assertEquals(tokenize("("), Token.OpenParen)
    assertEquals(tokenize(")"), Token.CloseParen)
    assertEquals(tokenize(";"), Token.Semicolon)
    assertEquals(tokenize("5"), Token.Number("5"))
    assertEquals(tokenize("a"), Token.Identifier("a"))
  }
  //
  test("multi-character tokens") {
    assertEquals(tokenize("123"), Token.Number("123"))
    assertEquals(tokenize("hello"), Token.Identifier("hello"))
    assertEquals(tokenize("var123"), Token.Identifier("var123"))
    assertEquals(tokenize("987654321"), Token.Number("987654321"))
    assertEquals(tokenize("CamelCaseVar"), Token.Identifier("CamelCaseVar"))
  }
  //
  test("whitespace handling") {
    assertEquals(tokenize("  123"), Token.Number("123"))
    assertEquals(tokenize("\t\n  hello"), Token.Identifier("hello"))
    assertEquals(tokenize(""), Token.EOF)
    assertEquals(tokenize("   \t\n  "), Token.EOF)
  }
  //
  // test("token sequences") {
  //   assertEquals(
  //     tokenizeAll("hello123 ( ) ;"),
  //     List(Token.Identifier("hello123"), Token.OpenParen, Token.CloseParen, Token.Semicolon, Token.EOF)
  //   )
  //
  //   assertEquals(
  //     tokenizeAll("123abc"),
  //     List(Token.Number("123"), Token.Identifier("abc"), Token.EOF)
  //   )
  //
  //   assertEquals(
  //     tokenizeAll("func("),
  //     List(Token.Identifier("func"), Token.OpenParen, Token.EOF)
  //   )
  // }
  //
  // test("complex expression") {
  //   assertEquals(
  //     tokenizeAll("add(123, var456);"),
  //     List(
  //       Token.Identifier("add"),
  //       Token.OpenParen,
  //       Token.Number("123"),
  //       Token.Identifier("var456"),
  //       Token.CloseParen,
  //       Token.Semicolon,
  //       Token.EOF
  //     )
  //   )
  // }
}
