package tpagu.compiler.ir

import tpagu.compiler.typeChecker.Type


sealed trait Instruction

sealed trait Operand

case class Register(id: Long, t: IRType) extends Operand {
  override def toString(): String = s"r$id"

}
case class Immediate(value: Long, t: IRType) extends Operand {
  override def toString(): String = s"$$$value"
}
// A label may be assigned to a register. It is like the label can be encoded
// as a particular unique 64 bit integer (in assembly it would be the real)
// address of the label.
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
  case class Call(f: Operand, args: List[Operand], result: Register) extends Instruction {
    override def toString: String =
      s"$result <= $f(${args.mkString(", ")})"
  }

  // define label to be a function taking a list of parameters in some specific registers.
  // static option needs to be in the IR, like for all top level declarations, to tell the Assembly gen
  // if it should emit a symbol here or not.
  //
  // The semantics of a function/call is that it is a label with a list of parameter registers, such that
  // the caller will provide a list of registers of equivalent length, and they are automatically "copied"
  // into the new registers
  case class Fun(label: Label, params: List[Register], static:Boolean = false) extends Instruction {
    override def toString: String =
      s"$label(${params.mkString(", ")}):"
  }

  // statement inside a function, meaning that the function will yield this value.
  // We can only return a Register
  case class Return(v: Operand) extends Instruction {
    override def toString(): String = s"return $v"
  }
  
  // Memory
  // store v => [loc]
  // meaning you store the value v inside the place in memory pointed to by loc.
  case class Store(v: Operand, loc: Operand, displacement:Long = 0, index:Option[Register] = None, scale:Long = 1, width:Int = 8) extends Instruction {
    override def toString(): String = s"store $v => [$loc]"
  }

  case class Load(loc: Operand, dst: Register, displacement:Long = 0, index:Option[Register] = None, scale:Long = 1, width:Int = 8) extends Instruction {
    override def toString(): String = s"load [$loc] => $dst"
  }

  // allocates a slot of length size, with specified alignment, in the stack, and makes result be a pointer to its address.
  // result <= alloc(size, alignment)
  case class Alloc(result:Register, size: Long, alignment: Long) extends Instruction {
    override def toString(): String = s"$result <= alloc($size, $alignment)"
  }

  case class Jump(location:Operand) extends Instruction {
    override def toString(): String = s"jump $location"
  }


  //TODO: For global definitions and declarations of functions, as well as simple declarations of functions, we will add this:
  // GlobalAlloc(result: Register, size: Long, Alignment: Long, name: String, static: Boolean = false, extern: Boolean = false) -> in the assembly it can emit a proper .dss or .data allocation
  // for the symbol with name 'name'. 
  // This also handles extern declarations. They will still create a register that points to their address, but we wont really need to allocate
  // space for them in the assembly. If the registers are optimized away, in the assembly we can go directly to the symbol itself. Otherwise we
  // need to hold a register with the address of the global variable, because our program must take the reference of it somewhere.
  //
  // We wont need any instruction for function declarations, since we can't interpret them, and when going to assembly we just emit the call instruction
  // without needing to consider the function itself.
} 
