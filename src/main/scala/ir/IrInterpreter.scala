package tpagu.compiler.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import IR.LabelDecl
import tpagu.compiler.ir.IR.Fun
import tpagu.compiler.ir.IR.Return
import tpagu.compiler.ir.IR.Add
import tpagu.compiler.ir.IR.Mult
import tpagu.compiler.ir.IR.Neg
import tpagu.compiler.ir.IR.Mov
import tpagu.compiler.ir.IR.Call
import tpagu.compiler.ir.IR.Store
import tpagu.compiler.ir.IR.Load
import tpagu.compiler.ir.IR.Alloc
import scala.annotation.tailrec

/** We include an IR interpreter for two reasons:
  *   1. It is useful for tests 2. It is useful for optimizations like constant
  *      folding It also shouldn't be very complex to write
  */
type DebugValues = List[Long]

val MAX_MEMORY_SIZE = 10000

// we don't reuse stack slots for our interpreter.. this is technically fine and just easier to implement
final case class Memory(mem: Array[Byte], var stackLocation: Long) {

  private def nthByte(value: Long, n: Int): Byte =
    ((value >>> (8 * n)) & 0xffL).toByte

  def size: Int = mem.length

  def loadAt(location: Long, width: Int): Long = {
    assert(0 <= location && location + width <= mem.length)
    assert(isValidWidth(width))

    // little-endian reconstruction
    var x = 0L
    var offset = 0L
    while (offset < width) {
      val b = mem((location + offset).toInt)
      x |= ((b.toLong & 0xffL) << (offset * 8))
      offset += 1
    }

    // sign-extend to Long according to width
    width match {
      case 1 => x.toByte.toLong
      case 2 => x.toShort.toLong
      case 4 => x.toInt.toLong
      case 8 => x
    }
  }

  def getStackSlot(size: Long): Long = {
    val slot = this.stackLocation
    this.stackLocation += size
    slot
  }

  def storeAt(location: Long, value: Long, width: Int): Unit = {
    assert(0 <= location && location + width <= mem.length)
    assert(isValidWidth(width))

    var off = 0
    while (off < width) {
      mem((location + off).toInt) = nthByte(value, off)
      off += 1
    }
  }
}

object Memory {
  def zeroed(size: Int): Memory =
    Memory(Array.fill(size)(0.toByte), 0L)
}

class IrInterpreter {
  // Returns a tuple of (DebugValues, exitCode: Long)
  // Where debug values is a list of values we printed out
  // with the dbg() instruction. They are Long since they must fit
  // inside a register. We also yield the exit code of the main
  // function. In my IR, all functions must return exactly on register
  // sized value. If no value is explicitly returned, 0 is implicitly returned.
  def interp(program: List[Instruction]): (Long) = {
    implicit val prog: Array[Instruction] = program.toArray
    implicit val memory = Memory.zeroed(MAX_MEMORY_SIZE)
    // state of registers
    implicit val regs: mutable.Map[Long, Long] = mutable.Map()

    implicit val labelMap: Map[Label, Int] = createLabelMap()

    // None => you are top level function, when you return you stop execution.
    // Some(x) => returning from this function means a jump to address x.
    implicit val returnAddresses: mutable.Stack[Int] = mutable.Stack()

    labelMap.get(Label("main")) match {
      case Some(entry) => {
        this.interpInstr(entry + 1)
      }

      case None => {
        throw new RuntimeException(
          "Tried to interpret iR with no main function. Can't interpret a translation unit with no entry point."
        )
      }
    }
  }

  def createLabelMap()(implicit program: Array[Instruction]): Map[Label, Int] =
    program.zipWithIndex.collect {
      case (LabelDecl(label), idx) => label -> idx
      case (Fun(label, _, _), idx) => label -> idx
    }.toMap

  // operand to value
  def opValue(
      op: Operand
  )(implicit regs: mutable.Map[Long, Long], labelMap: Map[Label, Int]) =
    op match {
      case Immediate(value) => value
      case l @ Label(_)     => labelMap(l).toLong
      case Register(id)     => regs.get(id).get
    }

  // instruction will never be return, that is handled separately
  @tailrec
  final def interpInstr(line: Int)(implicit
      memory: Memory,
      regs: mutable.Map[Long, Long],
      program: Array[Instruction],
      labelMap: Map[Label, Int],
      returnAddresses: mutable.Stack[Int]
  ): Long = {
    val instr = program(line)
    instr match {
      case Add(l, r, result) =>
        regs(result.id) = opValue(l) + opValue(r)
      case Mult(l, r, result) =>
        regs(result.id) = opValue(l) * opValue(r)
      case Neg(v, result)   => regs(result.id) = -opValue(v)
      case Mov(src, dst)    => regs(dst.id) = opValue(src)
      case LabelDecl(label) => {}
      case Call(f, args, result) => {
        val nextLine = (f match {
          case l @ Label(_) => {
            labelMap(l)
          }
          case Register(id) => {
            regs(id).toInt
          }
        })

        returnAddresses.push(line)
        val functionDefinition = program(nextLine)
        functionDefinition match {
          case Fun(label, params, static) => {
            for ((r, arg) <- params.zip(args)) {
              regs(r.id) = opValue(arg)
            }
          }
          case _ => {
            throw RuntimeException(
              s"IR interperter error: called to instruction that is not function: line ${line}. Program:\n${program.mkString("\n")}"
            )
          }
        }
        return interpInstr(nextLine + 1)
      }
      case Fun(label: Label, params: List[Register], static) => {
        throw RuntimeException("Not expecting Fun instruction here")
      }
      case Return(v) => {
        if (returnAddresses.isEmpty) {
          return opValue(v)
        }

        val retLine = returnAddresses.pop()
        val callInstr = program(retLine)
        callInstr match {
          case Call(f, args, result) => {
            regs(result.id) = opValue(v)
            return interpInstr(retLine + 1)
          }
          case _ => {
            throw RuntimeException(
              "IR interpreting error: Return does not lead to call instruction."
            )
          }

        }
        throw RuntimeException("Not expecting return here")
      }
      case Store(v, loc, displacement, index, scale, width) => {
        val location = opValue(loc)
        val indexValue = index match {
          case None      => 1
          case Some(ind) => regs.get(ind.id).get
        }
        memory.storeAt(
          location + displacement + indexValue * scale,
          opValue(v),
          width
        )
      }

      case Load(loc, dst, displacement, index, scale, width) => {
        val location = opValue(loc)
        val indexValue = index match {
          case None      => 1
          case Some(ind) => regs.get(ind.id).get
        }
        val value =
          memory.loadAt(location + displacement + indexValue * scale, width)
        regs(dst.id) = value
      }

      case Alloc(result, size, alignment) => {
        val addr = memory.getStackSlot(size)
        regs(result.id) = addr
      }
    }
    return interpInstr(line + 1)
  }
}
