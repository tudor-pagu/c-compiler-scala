package tpagu.compiler.desugar

import tpagu.compiler.Instruction
import tpagu.compiler.Operand
import tpagu.compiler.Register
import tpagu.compiler.Immediate
import tpagu.compiler.IR
import tpagu.compiler.typeChecker.FunT
import tpagu.compiler.Label
import tpagu.compiler.typeChecker.NumT

class IRGenerator(lastRegister:Int = 0, variableMap:Map[String, Register] = Map()) {
  def getNewRegister(size: Long): (Register, IRGenerator) = {
    (Register(lastRegister + 1, size), IRGenerator(lastRegister + 1))
  }
  // creates a new expression, possibly by 
  def produceExpression(e: AstC): (List[Instruction], Operand, IRGenerator) = {
    e match {
      case IntLiteral(value, t) => {
        val (register, irgen2) = getNewRegister(e.t.size())
        (List(), Immediate(value), irgen2)
      }
      case Add(l, r, t) => {
        val (lpre, lop, ir2) = this.produceExpression(l)
        val (rpre, rop, ir3) = ir2.produceExpression(r)
        val (reg, ir4) = ir3.getNewRegister(t.size())
        val fop = IR.Add(lop, rop, reg)
        (lpre ++ rpre :+ fop, reg, ir3)
      }
      case Mult(l, r, t) => {
        val (lpre, lop, ir2) = this.produceExpression(l)
        val (rpre, rop, ir3) = ir2.produceExpression(r)
        val (reg, ir4) = ir3.getNewRegister(t.size())
        val fop = IR.Mult(lop, rop, reg)
        (lpre ++ rpre :+ fop, reg, ir3)
      }
      case Neg(inner, t) => {
        val (lpre, lop, ir2) = this.produceExpression(inner)
        val (reg, ir3) = ir2.getNewRegister(t.size())
        val fop = IR.Neg(lop, reg)
        (lpre :+ fop, reg, ir3)
      }
      case FunctionCall(callee, args, t) => {
        (callee,t) match {
          case (Identifier(name, idt), FunT(ret, params)) => {
            val preops = args.foldLeft((Nil:List[Instruction], this, Nil:List[Operand]))((acc, x) => {
              val (ins, op, newIr) = (acc._2).produceExpression(x)
              (acc._1 ++ ins, newIr, (acc._3) :+ op)
            })
            val (reg, irFinal) = preops._2.getNewRegister(ret.size())
            val op = IR.Call(Label(name), preops._3, reg)
            (preops._1 :+ op, reg, irFinal)
          }
          case _ => throw RuntimeException("Tried to call function pointer. Not supported yet. TODO!")
        }
      }
      case Identifier(name, t) => {
        t match {
          case NumT(width, signed) => {
            val reg = variableMap(name)
            val (destReg, ir2) = this.getNewRegister(t.size())
            val ops = List(
              IR.Load(reg, destReg)
            )
            (ops, destReg, ir2)
          }
          case _ => {
            throw RuntimeException("Was asked to produce expression of non-variable identifier.")
          }
        }

      }
      case _ => {
        throw RuntimeException("Tried to produce expression for non-expression ast node.")
      }
    }
  }

  def lower(e: AstC): (List[Instruction], IRGenerator) = {
    e match {
      // evaluate the expression, in case it has side effects,
      // but ignore the result.
      case IntLiteral | Add | Mult | Neg | FunctionCall | Identifier => {
        val res = produceExpression(e)
        (res._1, res._3)
      }
    }
  }


}
