type Out[A] = (A, Lexer)

trait ParseRule[A] {
  def parse(lexer: Lexer): Either[CompilerError, Out[A]]

  // Functor
  def map[B](f: A => B): ParseRule[B] =
    Map(this, f)

  def flatMap[B](f: A => ParseRule[B]): ParseRule[B] =
    FlatMap(this, f)
}

case class Map[A, B](parser: ParseRule[A], f: A => B) extends ParseRule[B] {
  def parse(lexer: Lexer): Either[CompilerError, Out[B]] =
    parser.parse(lexer) match
      case Left(err)             => Left(err)
      case Right((a, nextLexer)) => Right((f(a), nextLexer))
}

case class FlatMap[A, B](parser: ParseRule[A], f: A => ParseRule[B])
    extends ParseRule[B] {
  def parse(lexer: Lexer): Either[CompilerError, Out[B]] =
    parser.parse(lexer) match
      case Left(err)             => Left(err)
      case Right((a, nextLexer)) => f(a).parse(nextLexer)
}

// Alternative (try left, then right)
case class Or[A](left: ParseRule[A], right: ParseRule[A]) extends ParseRule[A] {
  def parse(lexer: Lexer): Either[CompilerError, Out[A]] =
    left.parse(lexer) match {
      case success @ Right(_) => success
      case Left(leftError) =>
        right.parse(lexer) match {
          case success @ Right(_) => success
          case Left(rightError)   =>
            // Return error that got furthest
            if (leftError.span.start >= rightError.span.start) Left(leftError)
            else Left(rightError)
        }
    }
}

// Expect specific token
case class Just(expected: Token) extends ParseRule[TokenInfo] {
  def parse(lexer: Lexer): Either[CompilerError, Out[TokenInfo]] =
    lexer.nextToken() match {
      case Left(err) => Left(err)
      case Right((tokenInfo, nextLexer)) =>
        if (tokenInfo.token == expected) 
          Right((tokenInfo, nextLexer))
        else 
          Left(CompilerError(s"Expected $expected, got ${tokenInfo.token}", tokenInfo.span))
    }
}

