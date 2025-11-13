package tpagu.compiler.ir

import IR.{Alloc, Fun, Store}
import tpagu.compiler.*
import tpagu.compiler.desugar.*
import tpagu.compiler.typeChecker.{FunT, NumT}

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

  def withNewVariableMap(newMap:Map[String, Register]):IRGenerator = {
    IRGenerator(lastRegister, newMap, functionMap)
  }
  
  def withNewFunMap(name: String, label: Label):IRGenerator = {
    IRGenerator(lastRegister, variableMap, functionMap.updated(name, label))
  }

  def getNewRegister(): (Register, IRGenerator) = {
    (Register(lastRegister + 1), IRGenerator(lastRegister + 1, variableMap, functionMap))
  }

  def getNewRegisters(n: Int): (List[Register], IRGenerator) = {
    if (n == 0) then {
      return (List(), this)
    }

    val registers = (lastRegister + 1).to(lastRegister + n).map(id => Register(id)).toList
    assert(registers.size == n)
    assert(registers.last.id == lastRegister + n)
    (registers, IRGenerator(lastRegister + n, variableMap, functionMap))
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

        assert(paramNames.size == paramTypes.size)

        val (paramRegs, ir2) = this.getNewRegisters(paramNames.size)
        val (stackParamRegs, ir3) = ir2.getNewRegisters(paramNames.size)

        val addedOps = paramRegs.lazyZip(stackParamRegs).lazyZip(paramTypes).flatMap((paramReg, stackParamReg, t) => {
          List(
              Alloc(stackParamReg, t.size(), t.alignment()),
              Store(paramReg, stackParamReg)
            )
        })

        val addedRegs = paramNames.zip(stackParamRegs).map((paramName, stackParamReg) => {
          (paramName, stackParamReg)
        })

        val newRegs = variableMap ++ addedRegs

        val newIr = ir3.withNewVariableMap(newRegs)

        val execBody = newIr.lower(body)

        val funLabel = Label(name)

        val finalOps = List(
          Fun(
            funLabel, paramRegs
            )
        ) ++ addedOps ++ execBody._1

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
        (ops :+ IR.Return(op), ir2)
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
