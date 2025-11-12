package tpagu.compiler.ir

import scala.collection.mutable.ListBuffer
import IR.LabelDecl
import tpagu.compiler.ir.IR.Fun
/**
 * We include an IR interpreter for two reasons:
 * 1. It is useful for tests
 * 2. It is useful for optimizations like constant folding
 * It also shouldn't be very complex to write
 */
type DebugValues = List[Long]

private class IrFunction(val name: String, val decl: Fun, val body: Program)

class Memory(val mem:Array[Byte]) {
  def nthByte(value: Long, n: Int): Byte = ((value >>> (8 * n)) & 0xFF).toByte
  def loadAt(location:Int, width: Int): Long = {
    assert(0 <= location && location + width <= mem.length)
    assert(isValidWidth(width))
    var x:Long = 0;
    for (i <- location until location + width) {
      x = x | ( ( mem(i).toLong & 0xFFL) << ((i - location) * 8) )
    }
    // assert(-(1L << (8 * width - 1  ) ) <= x && x <= (1L << (8 * width - 1 )) - 1)
    x
  }

  def storeAt(location:Int, value: Long, width: Int):Unit = {
    assert(0 <= location && location + width <= mem.length)
    // assert(-(1L << (8 * width - 1  ) ) <= value && value <= (1L << (8 * width - 1 )) - 1)
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
  def interp(program: Program): (DebugValues, Long) = {
    println(program)
    val functions = splitProgramIntoFunctions(program)
    val mainFunction = functions.find(_.name == "main")
    mainFunction match {
      case Some(main) => {
        this.interp(program)
      }
      case None => {
        throw new RuntimeException("Tried to interpret iR with no main function. Can't interpret a translation unit with no entry point.")
      }
    }
  }

  // interpret executing this function, given some arguments (which are all 64 bit register values), 
  // and yield the return value of the function, as well as any executed debug stataments.
  def interpFunction(f: IrFunction, args: List[Long]): (DebugValues, Long) = {
    ???
  }

  def interpInstr(i: Instruction, registersState: Map[Long, Long], memory: Memory) = {
    ???
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
