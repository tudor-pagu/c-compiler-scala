package tpagu.compiler
import tpagu.compiler.typeChecker.Type



sealed trait Instruction

sealed trait Operand

case class Register(id: Int, size: Long = 8) extends Operand
case class Immediate(value: Long) extends Operand
case class Label(id: String) extends Operand

object IR {
  case class Add(l: Operand, r: Operand, result: Register) extends Instruction
  case class Mult(l: Operand, r: Operand, result: Register) extends Instruction
  case class Neg(v: Operand, result: Register) extends Instruction
  case class Mov(src: Operand, dst: Register) extends Instruction
  case class LabelDecl(label: Label) extends Instruction
 
  // Functions
  case class Call(f: Label, args: List[Operand], result: Register) extends Instruction
  case class Function(label: Label, params: List[Register]) extends Instruction
  case class Return(v: Operand) extends Instruction

  // Memory
  case class Store(v: Operand, loc: Register, displacement:Long = 0, index:Long = 0, scale:Long = 1) extends Instruction
  case class Load(loc: Register, dst: Register, displacement:Long = 0, index:Long = 0, scale:Long = 1) extends Instruction
  case class StAlloc(size: Long, align: Long) extends Instruction


} 
