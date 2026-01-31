package tpagu.compiler

class File(val name: String, val contents: String)

class Span(val start: Int, val end: Int, val file: File)

type AstID = Int
// id is used to add metadata to the tree
case class Spanned[A](node: A, span: Span, id:AstID) {
  override def toString(): String = node.toString
}

object Span {
  val empty = Span(0, 0, File("<unknown>", ""))
  def merge(a: Span, b: Span): Span = {
    if a == empty then b
    else if b == empty then a
    else {
      assert(a.file == b.file)
      Span(Math.min(a.start, b.start), Math.max(a.end, b.end), a.file)
    }
  }
}

object Spanned {
    private var counter: AstID = 0
    def apply[A](node: A, span: Span): Spanned[A] = {
      counter += 1
      new Spanned(node, span, counter)
    }
    def unapply[A](s: Spanned[A]): Some[(A, Span)] = Some((s.node, s.span))
}

enum ErrorKind {
  case Generic
  case Syntax
  case Type

  override def toString(): String = this match {
    case Generic => "Generic Error"
    case Syntax  => "Syntax Error"
    case Type    => "Type Error"
  }
}

case class CompilerError(val message: String, val span: Span, kind: ErrorKind = ErrorKind.Generic) extends Exception(message) {
  override def toString: String = kind match {
    case ErrorKind.Generic => s"Error at ${span.start}-${span.end}: $message"
    case t@_ => s"${t.toString}: Error at ${span.start}-${span.end}: $message"
  }
}
