package tpagu.compiler.ir

sealed trait IRType {
  def size: Long
  def alignment: Long
}

case class IntegerT(size: Long) extends IRType {
  def alignment = size
}

case class StructT(fields: List[(String, IRType)]) extends IRType {
  lazy val size: Long = {
    var offset = 0L
    for ((_, ty) <- fields) {
      offset = roundUp(offset, ty.alignment)
      offset += ty.size
    }
    roundUp(offset, alignment)
  }

  lazy val alignment: Long = {
    var maxAlign = 1L
    for ((_, ty) <- fields) {
      if (ty.alignment > maxAlign) maxAlign = ty.alignment
    }
    maxAlign

  }

  private def roundUp(x: Long, align: Long): Long =
    (x + align - 1) & -align
}
