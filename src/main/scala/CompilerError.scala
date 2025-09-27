package tpagu.compiler

class File(val name: String, val contents: String)

class Span(val start: Int, val end: Int, val file: File)

case class Spanned[A](node: A, span: Span) {
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

class CompilerError(val message: String, val span: Span) {
  override def toString: String =
    s"Error at ${span.start}-${span.end}: $message"
}
