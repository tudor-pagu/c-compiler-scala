package tpagu.compiler


sealed trait Instruction

sealed trait Operand

case class Register(name: String) extends Operand
case class Immediate(value: Long) extends Operand
case class Label(name: String) extends Operand

object IR {
  case class Add(l: Operand, r: Operand, result: Register) extends Instruction
  case class Neg(v: Operand, result: Register) extends Instruction
  case class Mov(src: Operand, dst: Register) extends Instruction
  case class LabelDecl(label: Label) extends Instruction
  case class Function(label: Label, params: List[Register]) extends Instruction
  case class Call(f: Label, args: List[Operand], result: Register) extends Instruction
  case class Return(v: Operand)
} 
