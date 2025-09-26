sealed trait Token
case class Identifier(name: String) extends Token
case class Number(value: String) extends Token
case class OpenParen() extends Token
case class CloseParen() extends Token
case class Semicolon() extends Token
case class EOF() extends Token

case class Empty()
case class Accept(token: Token)

class Lexer private (input: File, ind: Int) {
  def makeError(message: String): CompilerError =
    CompilerError(message, Span(ind, ind + 1, input))

  def this(input: File) = this(input, 0)
  def nextToken(): (Token, Lexer) | CompilerError = {

    var currentState: Empty | Token = Empty();
    var i = ind
    while i < input.contents.length do
      val result = transition(currentState, input.contents(i))
      result match
        case Empty() =>
          assert(currentState == Empty())
          currentState = Empty()
        case Accept(token) =>
          return (token, new Lexer(input, i))
        case token: Token =>
          currentState = token
        case c: CompilerError =>
          return c

      i += 1

    if currentState == Empty() then (EOF(), new Lexer(input, ind))
    else
      transition(currentState, ' ') match
        case Accept(token)    => (token, new Lexer(input, ind))
        case c: CompilerError => c
        case _                => makeError("Unexpected end of input.")
  }

  // we always try to eat one more character if we can. Then we return a new token
  // Also immediately return a parser error if we see a character which is not allowed.
  // Otherwise either accept if the token state is acceptable, or return a parsing error if its not acceptable.
  private def transition(
      token: Empty | Token,
      c: Char
  ): Empty | Accept | Token | CompilerError = token match
    case Empty() =>
      c match
        case '('                 => OpenParen()
        case ')'                 => CloseParen()
        case ';'                 => Semicolon()
        case d if d.isDigit      => Number(d.toString)
        case i if i.isLetter     => Identifier(i.toString)
        case _ if c.isWhitespace => Empty()
        case _ => throw new Exception(s"Unexpected character: $c")
    case token @ Identifier(name) =>
      c match
        case i if i.isLetterOrDigit => Identifier(name + i.toString)
        case _ if c.isWhitespace    => Accept(token)
    case token @ Number(value) =>
      c match
        case d if d.isDigit      => Number(value + d.toString)
        case _ if c.isWhitespace => Accept(token)
    case token @ OpenParen() => Accept(token)

    // case _ =>
    //   makeError(s"Received invalid character $c.")
}
