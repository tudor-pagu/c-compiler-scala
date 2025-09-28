package tpagu.compiler.lexer
import tpagu.compiler.{CompilerError, Span, File}
import tpagu.compiler.parser.{Declaration, TypeExt, TypeExtKind}

enum Token:
  case Identifier(name: String)
  case Number(value: String)
  case OpenParen, CloseParen
  case OpenBrace, CloseBrace
  case Semicolon, Comma
  case Plus, Minus, Times, Div
  case Assign
  case EOF
  case TypeName(t: TypeExt)
  case Return

case class TokenInfo(token: Token, span: Span)

case class Empty()
case class Accept(token: Token)

val builtinTypeTable: Map[String, TypeExt] = Map(
  "int" -> TypeExt(TypeExtKind.Int, Nil)
)

class Lexer private (
    val input: File,
    val ind: Int,
    typeTable: Map[String, TypeExt] = builtinTypeTable
) {
  def makeError(message: String): CompilerError =
    CompilerError(message, Span(ind, ind + 1, input))

  def makeTokenInfo(token: Token, start: Int, end: Int): TokenInfo =
    TokenInfo(token, Span(start, end, input))

  def this(input: File) = this(input, 0)
  def nextToken(): Either[CompilerError, (TokenInfo, Lexer)] = {

    var currentState: Empty | Token = Empty();
    var i = ind
    while i < input.contents.length do
      val result = transition(currentState, input.contents(i))
      result match
        case Empty() =>
          assert(currentState == Empty())
          currentState = Empty()
        case Accept(token) =>
          return Right((makeTokenInfo(token, ind, i + 1), new Lexer(input, i)))
        case token: Token =>
          currentState = token
        case c: CompilerError =>
          return Left(c)

      i += 1

    if currentState == Empty() then
      Right(makeTokenInfo(Token.EOF, i, i), new Lexer(input, i))
    else
      transition(currentState, ' ') match
        case Accept(token) =>
          Right(makeTokenInfo(token, ind, i), new Lexer(input, i))
        case c: CompilerError => Left(c)
        case _                => Left(makeError("Unexpected end of input."))
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
        case '('                 => Token.OpenParen
        case ')'                 => Token.CloseParen
        case ';'                 => Token.Semicolon
        case ','                 => Token.Comma
        case '+'                 => Token.Plus
        case '-'                 => Token.Minus
        case '*'                 => Token.Times
        case '/'                 => Token.Div
        case '='                 => Token.Assign
        case '{'                 => Token.OpenBrace
        case '}'                 => Token.CloseBrace
        case d if d.isDigit      => Token.Number(d.toString)
        case i if i.isLetter     => Token.Identifier(i.toString)
        case _ if c.isWhitespace => Empty()
        case _ => throw new Exception(s"Unexpected character: $c")
    case token @ Token.Identifier(name) =>
      c match
        case i if i.isLetterOrDigit => Token.Identifier(name + i.toString)
        case _ if name == "return" => Accept(Token.Return)
        case _ if typeTable.contains(name) =>
          Accept(Token.TypeName(typeTable(name)))
        case _ => Accept(token)

    case token @ Token.Number(value) =>
      c match
        case d if d.isDigit => Token.Number(value + d.toString)
        case _              => Accept(token)
    case t @ (Token.OpenParen | Token.CloseParen | Token.Semicolon | Token.EOF |
        Token.Plus | Token.Minus | Token.Times | Token.Div | Token.Comma |
        Token.Assign | Token.OpenBrace | Token.CloseBrace) =>
      Accept(t)
    case t @ (Token.TypeName(_) | Token.Return) =>
      throw new RuntimeException(
        s"Had to transition from state ${t} in lexer, but this state should only appear immediately before accepting, so this should be unreachable."
      )

    // case _ =>
    //   makeError(s"Received invalid character $c.")
}
