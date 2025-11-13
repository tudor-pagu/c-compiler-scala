package tpagu.compiler.ir

import scala.collection.mutable.ListBuffer
import IR.LabelDecl
import tpagu.compiler.ir.IR.Fun
import tpagu.compiler.ir.IR.Return
import tpagu.compiler.ir.IR.Add
import tpagu.compiler.ir.IR.Mult
import tpagu.compiler.ir.IR.Neg
import tpagu.compiler.ir.IR.Mov
import tpagu.compiler.ir.IR.Call
/**
 * We include an IR interpreter for two reasons:
 * 1. It is useful for tests
 * 2. It is useful for optimizations like constant folding
 * It also shouldn't be very complex to write
 */
type DebugValues = List[Long]

val MAX_MEMORY_SIZE = 268435456 // 256 MB

private class IrFunction(val name: String, val decl: Fun, val body: Program)

class Memory(val mem:Array[Byte]) {
  def nthByte(value:Long, n: Int): Byte = ((value >>> (8 * n)) & 0xFF).toByte

  def loadAt(location:Int, width: Int): Long = {
    assert(0 <= location && location + width <= mem.length)
    assert(isValidWidth(width))
    var x:Long = 0;
    for (i <- location until location + width) {
      x = x | ( ( mem(i).toLong & 0xFFL) << ((i - location) * 8) )
    }

    width match {
      case 1 => x.toByte
      case 2 => x.toShort
      case 4 => x.toInt
      case 8 => x.toLong
    }  
  }

  def storeAt(location:Int, value: Long, width: Int):Unit = {
    assert(0 <= location && location + width <= mem.length)
    assert(-(1L << (8 * width - 1  ) ) <= value && value <= (1L << (8 * width - 1 )) - 1)
    assert(isValidWidth(width))

    for (i <- location until location + width) {
      mem(i) = nthByte(value, i - location)
    }
  }
}

class IrInterpreter {
  // Returns a tuple of (DebugValues, exitCode: Long)
  // Where debug values is a list of values we printed out
  // with the dbg() instruction. They are Long since they must fit
  // inside a register. We also yield the exit code of the main
  // function. In my IR, all functions must return exactly on register
  // sized value. If no value is explicitly returned, 0 is implicitly returned.
  def interp(program: Program): (Long) = {
    println(program)
    val functions = splitProgramIntoFunctions(program)
    val mainFunction = functions.find(_.name == "main")
    implicit val funs = functions

    mainFunction match {
      case Some(main) => {
        this.interpFunction(main, List(), Map(), Memory(Array.ofDim(MAX_MEMORY_SIZE)))
      }
      case None => {
        throw new RuntimeException("Tried to interpret iR with no main function. Can't interpret a translation unit with no entry point.")
      }
    }
  }


  // interpret executing this function, given some arguments (which are all 64 bit register values), 
  // and yield the return value of the function, as well as any executed debug stataments.
  def interpFunction(f: IrFunction, args: List[Long], registersState: Map[Long, Long], memory: Memory)(implicit funs:List[IrFunction]): (Long) = {
    def helper(ops:Program, regs: Map[Long, Long], mem: Memory): Long = {
      ops.head match {
        case Return(v) => regs.get(v.id).get
        case _ => {
          val exec = this.interpInstr(ops.head, regs, mem)
          helper(ops.tail, exec._1, exec._2)
        }
      }
    }
    val newRegisters = registersState ++ f.decl.params.map(_.id).zip(args)
    helper(f.body, newRegisters, memory)
  }

  // operand to value
  def opValue(op:Operand, regs:Map[Long,Long]) = op match {
    case Immediate(value) => value
    case Label(_) => throw RuntimeException("Cannot get value of label")
    case Register(id) => regs.get(id).get
  }

  // instruction will never be return, that is handled separately
  def interpInstr(i: Instruction, regs: Map[Long, Long], mem: Memory)(implicit funs:List[IrFunction]):(Map[Long,Long], Memory) = {
    i match {
      case Add(l, r, result) => (regs.updated(result.id, opValue(l, regs) + opValue(r, regs)) , mem)
      case Mult(l, r, result) => (regs.updated(result.id, opValue(l, regs) * opValue(r, regs)) , mem)
      case Neg(v, result) => (regs.updated(result.id, -opValue(v, regs)), mem)
      case Mov(src, dst) => (regs.updated( dst.id , opValue(src, regs) ), mem)
      case LabelDecl(label) => (regs, mem)
      case Call(f, args, result) => {
        val fun = funs.find(_.name == f.id).get
        val x = interpFunction(fun, args.map(op => opValue(op, regs)), regs, mem)
        ???
      }
      case Fun(label: Label, params: List[Register], static) => {
        throw RuntimeException("Not expecting Fun instruction here")
      }
      case Return(v) => {
        throw RuntimeException("Not expecting return here")
      }

    }
  }

  def splitProgramIntoFunctions(program: Program):List[IrFunction] = {
    if (program.length == 0) {
      return List()
    }

    println(program(0))
    program(0) match {
      case Fun(_, _ , _) => {}
      case _ => {
        throw RuntimeException("First statement of program must be label in IR.")
      }
    }

    // will be overriden for sure
    var currentFunction:Option[IrFunction] = None
    val listBuffer:ListBuffer[IrFunction] = ListBuffer()
    
    program.foreach((instr) => {
      instr match {
        case decl@Fun(label, _, _) => {
          if (currentFunction.isDefined) {
            listBuffer += currentFunction.get
          }
          currentFunction = Some(IrFunction(label.id, decl, List()))
        }
        case _ => {
          currentFunction = Some(IrFunction(currentFunction.get.name, currentFunction.get.decl, currentFunction.get.body.appended(instr)))
        }
      }
    })
    listBuffer += currentFunction.get
    listBuffer.toList
  }

}
