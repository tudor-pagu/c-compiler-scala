package tpagu.compiler.ir

import tpagu.compiler.typeChecker.Type



type Program = List[Instruction]

sealed trait Instruction

sealed trait Operand

// We're assuming that we're running on a 64 bit machine, and all registers are considered to be
// 64 bit. All immediate values must also be 64 bits signed integers.
// Memory is byte addressable. Store and Load operations will write to arbitrary locations in memory.
// By default, we write 8 bytes, but the width parameter can allow the store or load to store or load smaller values.
// Width must be either 1,2,4, or 8, any other value is invalid.
//
// A Register is an abstraction of a machine register and can simply hold a 64 bit value.
// Operations in my IR typicall deal either with two registers, or with storing or loading a register to memory
case class Register(id: Int) extends Operand {
  override def toString(): String = s"r$id"

}
case class Immediate(value: Long) extends Operand {
  override def toString(): String = s"$$$value"
}
case class Label(id: String) extends Operand { // note: labels are unique
  override def toString(): String = s"${id}"
}

object IR {
  // Rresult <= l + r
  case class Add(l: Operand, r: Operand, result: Register) extends Instruction {
    override def toString(): String = s"$result <= $l + $r"
  }
  // Rresult <= l * r
  case class Mult(l: Operand, r: Operand, result: Register) extends Instruction {
    override def toString(): String = s"$result <= $l * $r"
  }
  // Rresult <= -v
  case class Neg(v: Operand, result: Register) extends Instruction {
    override def toString(): String = s"$result <= -$v"
  }
  // Rdst = src
  case class Mov(src: Operand, dst: Register) extends Instruction {
    override def toString(): String = s"mov $src => $dst"
  }
  // label:
  case class LabelDecl(label: Label) extends Instruction {
    override def toString(): String = s"\n$label:"
  }
 
  // Functions: To simplify writing IR, we provide the function abstraction.
  // Rresult <= f(args...)
  // f must be a label that was previously defined as a functoin taking exactly len(args) arguments
  case class Call(f: Label, args: List[Operand], result: Register) extends Instruction {
    override def toString: String =
      s"$result <= $f(${args.mkString(", ")})"
  }

  // define label to be a function taking a list of parameters in some specific registers.
  case class Fun(label: Label, params: List[Register]) extends Instruction {
    override def toString: String =
      s"$label(${params.mkString(", ")}):"
  }

  // statement inside a function, meaning that the function will yield this value.
  case class Return(v: Operand) extends Instruction {
    override def toString(): String = s"return $v"
  }

  // Memory
  // store v => [loc]
  // meaning you store the value v inside the place in memory pointed to by loc.
  case class Store(v: Operand, loc: Register, displacement:Long = 0, index:Long = 0, scale:Long = 1, width:Int = 8) extends Instruction {
    override def toString(): String = s"store $v => [$loc]"
  }

  case class Load(loc: Register, dst: Register, displacement:Long = 0, index:Long = 0, scale:Long = 1, width:Int = 8) extends Instruction {
    override def toString(): String = s"load [$loc] => $dst"
  }

  // allocates a slot of length size, with specified alignment, in the stack, and makes result be a pointer to its address.
  // result <= alloc(size, alignment)
  case class Alloc(result:Register, size: Long, alignment: Long) extends Instruction {
    override def toString(): String = s"$result <= alloc($size, $alignment)"
  }
} 
