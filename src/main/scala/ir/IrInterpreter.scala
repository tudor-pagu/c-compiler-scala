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
    println(functions.map(_.body))
    ???
  }

  def splitProgramIntoFunctions(program: Program):List[IrFunction] = {
    if (program.length == 0) {
      return List()
    }

    println(program(0))
    program(0) match {
      case Fun(_, _) => {}
      case _ => {
        throw RuntimeException("First statement of program must be label in IR.")
      }
    }

    // will be overriden for sure
    var currentFunction:Option[IrFunction] = None
    val listBuffer:ListBuffer[IrFunction] = ListBuffer()
    
    program.foreach((instr) => {
      instr match {
        case decl@Fun(label, _) => {
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
