package tpagu.compiler.parser
import tpagu.compiler.lexer.{Lexer, Token, TokenInfo}
import tpagu.compiler.{CompilerError, Spanned}
import tpagu.compiler.Span

type Out[A] = (A, Lexer)

trait ParseRule[A] {
  def parse(lexer: Lexer): Either[CompilerError, Out[A]]

  // Functor
  def map[B](f: A => Either[CompilerError, B]): ParseRule[B] =
    Map(this, f)

  // Monad
  def flatMap[B](f: A => ParseRule[B]): ParseRule[B] =
    FlatMap(this, f)

  def withFilter(p: A => Boolean): ParseRule[A] =
    return Filter(this, p)

  def <|>(other: ParseRule[A]): ParseRule[A] =
    Or(this, other)

  def or(other: ParseRule[A]): ParseRule[A] =
    Or(this, other)

  def repeat(range: Range): ParseRule[List[A]] =
    Repeat(this, range)

  def many: ParseRule[List[A]] =
    Repeat(this, 0 until Int.MaxValue)

  def maybe: ParseRule[Option[A]] =
    Maybe(this)

  def withUpdatedLexer(f: Out[A] => Lexer): ParseRule[A] =
    UpdatedLexer(this, f)

  // Applicative operators
  def <*>[B](that: ParseRule[B]): ParseRule[(A, B)] =
    for { a <- this; b <- that } yield Right((a, b))

  def *>[B](that: ParseRule[B]): ParseRule[B] =
    for { _ <- this; b <- that } yield Right(b)

  def <*[B](that: ParseRule[B]): ParseRule[A] =
    for { a <- this; _ <- that } yield Right(a)

  var debugName: String = ""
  def named(s:String): ParseRule[A] = {
    debugName = s
    this
  }

  override def toString: String = 
    if debugName != "" then s"ParseRule($debugName)" else super.toString
}

case class UpdatedLexer[A](parser: ParseRule[A], f: Out[A] => Lexer) extends ParseRule[A] {
  def parse(lexer: Lexer): Either[CompilerError, Out[A]] = {
    parser.parse(lexer) match {
      case Left(err) => Left(err)
      case Right((a, nextLexer)) => {
        Right(a, f((a, nextLexer)))
      }
    }
  }
}

case class Map[A, B](parser: ParseRule[A], f: A => Either[CompilerError, B])
    extends ParseRule[B] {
  def parse(lexer: Lexer): Either[CompilerError, Out[B]] =
    parser.parse(lexer) match
      case Left(err) => Left(err)
      case Right((a, nextLexer)) => {
        f(a) match
          case Left(err) => Left(err)
          case Right(v)  => Right((v, nextLexer))
      }
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
          Left(
            CompilerError(
              s"Expected $expected, got ${tokenInfo.token}",
              tokenInfo.span
            )
          )
    }
}

// Expect one of several tokens
case class OneOf(expected: List[Token]) extends ParseRule[TokenInfo] {
  def parse(lexer: Lexer): Either[CompilerError, Out[TokenInfo]] =
    lexer.nextToken() match {
      case Left(err) => Left(err)
      case Right((tokenInfo, nextLexer)) =>
        if (expected.contains(tokenInfo.token))
          Right((tokenInfo, nextLexer))
        else
          Left(
            CompilerError(
              s"Expected one of $expected, got ${tokenInfo.token}",
              tokenInfo.span
            )
          )
    }
}

case class Filter[A](parser: ParseRule[A], p: (A => Boolean)) extends ParseRule[A] {
  def parse(lexer: Lexer): Either[CompilerError, Out[A]] = 
    parser.parse(lexer) match {
      case Right((value, nextLexer)) if p(value) => Right((value, nextLexer))
      case Right((value, _)) => throw RuntimeException(s"Failed to pattern match inside a withFilter used by a for comprehension of a ParseRule. This should never happen. The value you tried to filter: $value") 
      case Left(err) => Left(err)
    }
}

// Repeat parser between min and max times
case class Repeat[A](parser: ParseRule[A], range: Range)
    extends ParseRule[List[A]] {
  def parse(lexer: Lexer): Either[CompilerError, Out[List[A]]] = {
    def loop(
        currentLexer: Lexer,
        acc: List[A],
        count: Int
    ): Either[CompilerError, Out[List[A]]] = {
      val min = range.start
      val max = if (range.isInclusive) range.end else range.end - 1

      if (count >= max) {
        Right((acc.reverse, currentLexer))
      } else {
        parser.parse(currentLexer) match {
          case Right((value, nextLexer)) =>
            loop(nextLexer, value :: acc, count + 1)
          case Left(_) if count >= min =>
            Right((acc.reverse, currentLexer))
          case Left(error) =>
            Left(error)
        }
      }
    }
    loop(lexer, Nil, 0)
  }
}

// Optional parser
case class Maybe[A](parser: ParseRule[A]) extends ParseRule[Option[A]] {
  def parse(lexer: Lexer): Either[CompilerError, Out[Option[A]]] =
    parser.parse(lexer) match {
      case Right((value, nextLexer)) => Right((Some(value), nextLexer))
      case Left(_)                   => Right((None, lexer))
    }
}

extension [A](p: ParseRule[A])
  def withSpan: ParseRule[Spanned[A]] =
    new ParseRule[Spanned[A]] {
      def parse(lexer: Lexer): Either[CompilerError, Out[Spanned[A]]] =
        p.parse(lexer) match
          case Left(err) => Left(err)
          case Right((node, nextLexer)) =>
            val span = Span(lexer.ind, nextLexer.ind, lexer.input)
            Right((Spanned(node, span), nextLexer))
    }

def listOf[A](p: ParseRule[A]): ParseRule[List[A]] =
  for {
    first <- p
    rest <- (Just(Token.Comma) <*> p).many
  } yield Right(first :: rest.map(_._2))
