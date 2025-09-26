class File(val name: String, val contents: String)

class Span(val start: Int, val end: Int, val file:File)

class CompilerError(val message : String, val span: Span) {
  override def toString: String = s"Error at ${span.start}-${span.end}: $message"
}
