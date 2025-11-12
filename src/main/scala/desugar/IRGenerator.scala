package tpagu.compiler.desugar

import tpagu.compiler.Instruction
import tpagu.compiler.Operand
import tpagu.compiler.Register
import tpagu.compiler.Immediate
import tpagu.compiler.IR
import tpagu.compiler.typeChecker.FunT
import tpagu.compiler.Label
import tpagu.compiler.typeChecker.NumT
import tpagu.compiler.IR.Alloc
import tpagu.compiler.Program
import tpagu.compiler.IR.Store
import tpagu.compiler.IR.Fun

def isValidWidth(width:Int) = width == 1 || width == 2 || width == 4 || width == 8
object IRGenerator {
  def newIrGenerator(): IRGenerator = IRGenerator(0, Map(), Map())
}

// ***
// IRGenerator will take an AstC (Core AST) and generate a list of instructions that will correspond to this AstC,
// as well as a new IRGenerator. We need to return a new IRGenerator because lowering an AstC may introduce bindings
// within some scopes, which we keep inside IRGenerator. Also, it may use up certain global resources, like "Registers"
//
// ***
class IRGenerator(
    val lastRegister: Int,
    // map from a name that should be in scope to a register containing an address to it (either in the stack
    // or in global scope)
    val variableMap: Map[String, Register],
    val functionMap: Map[String, Label],
) {
  def nameLookup(name:String) : Register = {
    val x= variableMap.get(name)
    if (x.isEmpty) {
      throw new RuntimeException(f"Tried to lookup $name but it was not set in the variableMap.")
    }
    x.get
  }

  def funLookup(name:String) : Label = {
    val x= functionMap.get(name)
    if (x.isEmpty) {
      throw new RuntimeException(f"Tried to lookup $name but it was not set in the variableMap.")
    }
    x.get
  }

  def withLastRegisterOf(otherIrGenerator:IRGenerator): IRGenerator = {
    IRGenerator(otherIrGenerator.lastRegister, variableMap, functionMap)
  }

  def withNewVariableMap(name: String, register: Register):IRGenerator = {
    IRGenerator(lastRegister, variableMap.updated(name, register), functionMap)
  }
  
  def withNewFunMap(name: String, label: Label):IRGenerator = {
    IRGenerator(lastRegister, variableMap, functionMap.updated(name, label))
  }

  def getNewRegister(): (Register, IRGenerator) = {
    (Register(lastRegister + 1), IRGenerator(lastRegister + 1, variableMap, functionMap))
  }
  // creates a new expression, possibly by adding more instructions before it.
  // This returns a list of instructions, followed by an operand which will equal the expression
  // we want to generate and can be embedded in future codegens.
  def produceExpression(e: AstC): (List[Instruction], Operand, IRGenerator) = {
    e match {
      case IntLiteral(value, t) => {
        (List(), Immediate(value), this)
      }
      case Add(l, r, t) => {
        val (lpre, lop, ir2) = this.produceExpression(l)
        val (rpre, rop, ir3) = ir2.produceExpression(r)
        val (reg, ir4) = ir3.getNewRegister()
        val fop = IR.Add(lop, rop, reg)
        (lpre ++ rpre :+ fop, reg, ir4)
      }
      case Mult(l, r, t) => {
        val (lpre, lop, ir2) = this.produceExpression(l)
        val (rpre, rop, ir3) = ir2.produceExpression(r)
        val (reg, ir4) = ir3.getNewRegister()
        val fop = IR.Mult(lop, rop, reg)
        (lpre ++ rpre :+ fop, reg, ir4)
      }
      case Neg(inner, t) => {
        val (lpre, lop, ir2) = this.produceExpression(inner)
        val (reg, ir3) = ir2.getNewRegister()
        val fop = IR.Neg(lop, reg)
        (lpre :+ fop, reg, ir3)
      }
      case FunctionCall(callee, args, t) => {
        (callee, callee.t) match {
          case (Identifier(name, idt), FunT(ret, params)) => {
            val preops = args.foldLeft(
              (Nil: List[Instruction], this, Nil: List[Operand])
            )((acc, x) => {
              val (ins, op, newIr) = (acc._2).produceExpression(x)
              (acc._1 ++ ins, newIr, (acc._3) :+ op)
            })
            val (reg, irFinal) = preops._2.getNewRegister()
            val op = IR.Call(Label(name), preops._3, reg)
            (preops._1 :+ op, reg, irFinal)
          }
          case _ =>
            throw RuntimeException(
              "Tried to call function pointer. Not supported yet. TODO!"
            )
        }
      }
      case Identifier(name, t) => {
        t match {
          case NumT(width, signed) => {
            val reg = variableMap(name)
            val (destReg, ir2) = this.getNewRegister()
            val ops = List(
              IR.Load(reg, destReg)
            )
            (ops, destReg, ir2)
          }
          case _ => {
            throw RuntimeException(
              "Was asked to produce expression of non-variable identifier."
            )
          }
        }

      }
      case _ => {
        throw RuntimeException(
          "Tried to produce expression for non-expression ast node."
        )
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
      case VarDefinition(name, t) => {
        val (reg,ir2) = this.getNewRegister()
        (List(
          Alloc(reg, t.size(), t.alignment()),
        ), ir2.withNewVariableMap(name, reg))
      }
      case Assignment(leftSide, rightSide) => {
        val reg = nameLookup(leftSide)
        val (ops, op, ir2) = this.produceExpression(rightSide)
        // TODO: breaks for types with different widths, structs, etc.
        (ops :+ Store(op, reg), ir2)
      }

      case FunctionDefinition(name, paramNames, body, ofType) => {
        val paramTypes = ofType match {
          case FunT(returnType, paramTypes) => paramTypes
          case _ => throw RuntimeException("type of function is not FunT.")
        }

        val (ops, registers,ir2) = (paramNames.zip(paramTypes)).foldLeft(List():Program, List():List[Register], this)( (acc, param) => {
            val (name, t) = param
            val (newReg, newIr) = acc._3.getNewRegister()
            // TODO breaks for types with different widths, structs, etc.
            (acc._1 :+ Alloc(newReg, t.size(), t.alignment()), acc._2 :+ newReg, newIr.withNewVariableMap(name, newReg))
        })

        val execBody = ir2.lower(body)

        val funLabel = Label(name)

        val finalOps = List(
          Fun(
            funLabel, registers
            )
        ) ++ ops ++ execBody._1

        val finalIrGen = this.withLastRegisterOf(execBody._2)
        (finalOps, finalIrGen)
      }
      case Block(statements) => {
        val (innerOps, innerIrGen) = statements.foldLeft(List():List[Instruction], this)((acc, current) => {
          val (previousOps, previousIrGen) = acc
          val x = previousIrGen.lower(current)
          (previousOps ++ x._1, x._2)
        })
        // we don't want to take the IRGen (scope, etc.) of the block. Just take the register count
        (innerOps, this.withLastRegisterOf(innerIrGen))
      }
      
      case TranslationUnit(statements) => {
        val (innerOps, innerIrGen) = statements.foldLeft(List():List[Instruction], this)((acc, current) => {
          val (previousOps, previousIrGen) = acc
          val x = previousIrGen.lower(current)
          (previousOps ++ x._1, x._2)
        })
        // we don't want to take the IRGen (scope, etc.) of the block. Just take the register count
        (innerOps, this.withLastRegisterOf(innerIrGen))
      }

      case Return(e) => {
        val (ops, op, ir2) = this.produceExpression(e)
        (ops :+ tpagu.compiler.IR.Return(op), ir2)
      }

      case Cast(e, t) => {
        ???
      }

      case Seq(statements) => {
        val (innerOps, innerIrGen) = statements.foldLeft(List():List[Instruction], this)((acc, current) => {
          val (previousOps, previousIrGen) = acc
          val x = previousIrGen.lower(current)
          (previousOps ++ x._1, x._2)
        })
        // here we dont want to keep the same IrGen since Seq doesnt create a new scope
        (innerOps, innerIrGen)

      }

    }
  }

}
