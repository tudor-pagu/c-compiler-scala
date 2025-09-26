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
      case Left(err) => Left(err)
      case Right((a, nextLexer)) => f(a).parse(nextLexer)
}
