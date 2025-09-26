enum Token:
  case Identifier(name: String)
  case Number(value: String)
  case OpenParen, CloseParen
  case Semicolon
  case EOF

case class Empty()
case class Accept(token: Token)

class Lexer private (input: File, ind: Int) {
  def makeError(message: String): CompilerError =
    CompilerError(message, Span(ind, ind + 1, input))

  def this(input: File) = this(input, 0)
  def nextToken(): (Token, Lexer) | CompilerError = {

    var currentState: Empty | Token = Empty();
    while ind < input.contents.length do
      val result = transition(currentState, input.contents(ind))
      result match
        case Empty() =>
          assert(currentState == Empty())
          currentState = Empty()
        case Accept(token) =>
          return (token, new Lexer(input, ind))
        case token: Token =>
          currentState = token
        case c: CompilerError =>
          return c

    if currentState == Empty() then (Token.EOF, new Lexer(input, ind))
    else transition(currentState, ' ') match
      case Accept(token) => (token, new Lexer(input, ind))
      case c: CompilerError => c
      case _ => makeError("Unexpected end of input.")
  }

  // we always try to eat one more character if we can. Then we return a new token
  // Otherwise either accept if the token state is acceptable, or return a parsing error if its not acceptable.
  // Also immediately return a parser error if we see a character which is not allowed.
  private def transition(
      token: Empty | Token,
      c: Char
  ): Empty | Accept | Token | CompilerError = token match
    case Empty() =>
      c match
        case '('                 => Accept(Token.OpenParen)
        case ')'                 => Accept(Token.CloseParen)
        case ';'                 => Accept(Token.Semicolon)
        case d if d.isDigit      => Token.Number(d.toString)
        case i if i.isLetter     => Token.Identifier(i.toString)
        case _ if c.isWhitespace => Empty()
        case _ => throw new Exception(s"Unexpected character: $c")
    case token @ Token.Number(value) =>
      c match
        case d if d.isDigit => Token.Number(value + d.toString)
        case _              => Accept(token)
    case token @ Token.Identifier(name) =>
      c match
        case i if i.isLetterOrDigit => Token.Identifier(name + i.toString)
        case _                      => Accept(token)
    case _ =>
      makeError(s"Received invalid character $c.")
}
