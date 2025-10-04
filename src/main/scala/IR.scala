package tpagu.compiler
import tpagu.compiler.typeChecker.Type



sealed trait Instruction

sealed trait Operand

type IntSize = 1 | 2 | 4 | 8 // Size of a register in bytes.

case class Register(name: String, size: IntSize = 8) extends Operand
case class Immediate(value: BigDecimal) extends Operand
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
