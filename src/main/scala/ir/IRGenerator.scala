package tpagu.compiler.ir

import IR.{Alloc, Fun, Store}
import tpagu.compiler.*
import tpagu.compiler.desugar.*
import tpagu.compiler.typeChecker.{FunT, NumT}
import tpagu.compiler.ir.IR.Load
import tpagu.compiler.typeChecker.PtrT

def isValidWidth(width: Int) =
  width == 1 || width == 2 || width == 4 || width == 8

/** State monad we can use to handle composing stateful operations where
  * IRGenerator encapsulates the state, and we also want to maintain the
  * operations we generate as a list.
  */
case class IRMonad[A](run: IRGenerator => (List[Instruction], IRGenerator, A)) {
  type ReturnType = (List[Instruction], IRGenerator, A)
  def map[B](f: A => B): IRMonad[B] = {
    IRMonad(ir => {
      val (instr, irRes, a) = run(ir)
      (instr, irRes, f(a))
    })
  }

  def flatMap[B](f: A => IRMonad[B]): IRMonad[B] = {
    IRMonad(ir => {
      val (instr1, irRes1, a) = run(ir)
      val (instr2, irRes2, b) = f(a).run(irRes1)
      (instr1 ++ instr2, irRes2, b)
    })
  }
}

object IRMonad {
  def emit(instr: Instruction) = IRMonad[Unit](ir => {
    (List(instr), ir, ())
  })
  def emit(instr: List[Instruction]) = IRMonad[Unit](ir => {
    (instr, ir, ())
  })
  def just[A](a: A) = IRMonad[A](ir => {
    (Nil, ir, a)
  })

  def sequence[A](monads: List[IRMonad[A]]): IRMonad[List[A]] = {
    monads.foldRight(just(List[A]())) { (currentMonad, accMonad) =>
      for {
        current <- currentMonad
        acc <- accMonad
      } yield current :: acc
    }
  }

  def nameLookup(name: String) = {

    IRMonad(ir => {
      (Nil, ir, ir.nameLookup(name))
    })
  }

  def withNewVariable(name: String, register: Register): IRMonad[Unit] = {
    IRMonad(
      { ir =>
        {
          (Nil, ir.withNewVariable(name, register), ())
        }
      }
    )
  }

  def withNewVariables(variables: List[(String, Register)]): IRMonad[Unit] = {
    IRMonad { ir =>
      {
        (Nil, ir.withNewVariableMap(ir.variableMap ++ variables), ())
      }
    }
  }

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
    val functionMap: Map[String, Label]
) {
  def nameLookup(name: String): Register = {
    val x = variableMap.get(name)
    if (x.isEmpty) {
      throw new RuntimeException(
        f"Tried to lookup $name but it was not set in the variableMap."
      )
    }
    x.get
  }

  def funLookup(name: String): Label = {
    val x = functionMap.get(name)
    if (x.isEmpty) {
      throw new RuntimeException(
        f"Tried to lookup $name but it was not set in the variableMap."
      )
    }
    x.get
  }

  def withLastRegisterOf(otherIrGenerator: IRGenerator): IRGenerator = {
    IRGenerator(otherIrGenerator.lastRegister, variableMap, functionMap)
  }

  def withLastRegister(lastRegister: Int): IRGenerator = {
    IRGenerator(lastRegister, variableMap, functionMap)
  }

  def withNewVariable(name: String, register: Register): IRGenerator = {
    IRGenerator(lastRegister, variableMap.updated(name, register), functionMap)
  }

  def withNewVariableMap(newMap: Map[String, Register]): IRGenerator = {
    IRGenerator(lastRegister, newMap, functionMap)
  }

  def withNewFunMap(name: String, label: Label): IRGenerator = {
    IRGenerator(lastRegister, variableMap, functionMap.updated(name, label))
  }

}

object IRGenerator {
  def newIrGenerator(): IRGenerator = IRGenerator(0, Map(), Map())

  def getNewRegister(): IRMonad[Register] = {
    IRMonad { ir =>
      {
        (
          Nil,
          ir.withLastRegister(ir.lastRegister + 1),
          Register(ir.lastRegister + 1)
        )
      }
    }
    // (Register(lastRegister + 1), IRGenerator(lastRegister + 1, variableMap, functionMap))
  }

  def getNewRegisters(n: Int): IRMonad[List[Register]] = {
    IRMonad(ir => {
      if (n == 0) {
        (Nil, ir, Nil)
      } else {
        val registers = (ir.lastRegister + 1)
          .to(ir.lastRegister + n)
          .map(id => Register(id))
          .toList
        assert(registers.size == n)
        assert(registers.last.id == ir.lastRegister + n)
        (
          Nil,
          IRGenerator(ir.lastRegister + n, ir.variableMap, ir.functionMap),
          registers
        )
      }
    })
  }

  def getAddressOfLvalue(e: AstC): IRMonad[Operand] = e match {
    case Identifier(name, t) =>
      IRMonad(ir => { (Nil, ir, ir.variableMap.get(name).get) })
    case Dereference(ptr, t) => {
      for {
        innerAddress <- getAddressOfLvalue(ptr)
        reg <- getNewRegister()
        _ <- IRMonad.emit(IR.Load(innerAddress, reg, width = t.size().toInt))
      } yield reg
    }
    case _ =>
      throw RuntimeException(
        s"Did not manage to get address of expression ${e}, maybe its not an lvalue."
      )
  }

  // creates a new expression, possibly by adding more instructions before it.
  // This returns a list of instructions, followed by an operand which will equal the expression
  // we want to generate and can be embedded in future codegens.

  def produceExpression(e: AstC): IRMonad[Operand] = {
    e match {
      case IntLiteral(value, t) => {
        IRMonad.just(Immediate(value))
      }
      case Add(l, r, t) => {
        for {
          lop <- produceExpression(l)
          rop <- produceExpression(r)
          reg <- getNewRegister()
          _ <- IRMonad.emit(IR.Add(lop, rop, reg))
        } yield (reg)
      }
      case Mult(l, r, t) => {
        for {
          lop <- produceExpression(l)
          rop <- produceExpression(r)
          reg <- getNewRegister()
          _ <- IRMonad.emit(IR.Mult(lop, rop, reg))
        } yield (reg)
      }
      case Neg(inner, t) => {
        for {
          lop <- produceExpression(inner)
          reg <- getNewRegister()
          _ <- IRMonad.emit(IR.Neg(lop, reg))
        } yield reg
      }

      case Dereference(e, t) => {
        for {
          lop <- produceExpression(e)
          reg <- getNewRegister()
          _ <- IRMonad.emit(IR.Load(lop, reg))
        } yield reg
      }

      case AddressOf(e, t) => {
        for {
          lop <- produceExpression(e)
          addr <- getAddressOfLvalue(e)
        } yield (addr)
      }

      case FunctionCall(callee, args, t) => {
        (callee, callee.t) match {
          case (Identifier(name, idt), FunT(ret, params)) => {
            for {
              computedArgs <- IRMonad.sequence(
                args.map(arg => produceExpression(arg))
              )
              reg <- getNewRegister()
              _ <- IRMonad.emit(IR.Call(Label(name), computedArgs, reg))
            } yield (reg)
          }
          case _ =>
            throw RuntimeException(
              "Tried to call function pointer. Not supported yet. TODO!"
            )
        }
      }
      case Identifier(name, t) => {
        t match {
          case NumT(_, _, _) | PtrT(_, _) => {
            for {
              idReg <- IRMonad.nameLookup(name)
              destReg <- getNewRegister()
              _ <- IRMonad.emit(IR.Load(idReg, destReg, width = t.size().toInt))
            } yield destReg
          }
          case _ => {
            throw RuntimeException(
              "Was asked to produce expression of non-variable identifier."
            )
          }
        }
      }
      case Assignment(leftSide, rightSide) => {
        for {
          loweredAssignment <- lower(e)
          addr <- getAddressOfLvalue(leftSide)
          destReg <- getNewRegister()
          _ <- IRMonad.emit(
            Load(addr, destReg, width = leftSide.t.size().toInt)
          )
        } yield destReg
      }
      case _ => {
        throw RuntimeException(
          "Tried to produce expression for non-expression ast node."
        )
      }
    }
  }
  def pub_lower(e: AstC): List[Instruction] = {
    val ir = IRGenerator.newIrGenerator()
    val monad = lower(e)
    monad.run(ir)._1
  }

  def lower(e: AstC): IRMonad[Unit] = {
    e match {
      // evaluate the expression, in case it has side effects, but ignore the result.
      case IntLiteral(_, _) | Add(_, _, _) | Mult(_, _, _) | Neg(_, _) |
          FunctionCall(_, _, _) | Identifier(_, _) => {
        for {
          res <- produceExpression(e)
        } yield ()
      }
      case VarDefinition(name, t) => {
        for {
          reg <- getNewRegister()
          _ <- IRMonad.emit(Alloc(reg, t.size(), t.alignment()))
          _ <- IRMonad.withNewVariable(name, reg)
        } yield ()
      }
      case Assignment(leftSide, rightSide) => {
        for {
          lop <- getAddressOfLvalue(leftSide)
          rop <- produceExpression(rightSide)
          _ <- IRMonad.emit(Store(rop, lop, width = rightSide.t.size().toInt))
        } yield ()
      }

      case FunctionDefinition(name, paramNames, body, ofType) => {
        val paramTypes = ofType match {
          case FunT(returnType, paramTypes) => paramTypes
          case _ => throw RuntimeException("type of function is not FunT.")
        }

        assert(paramNames.size == paramTypes.size)

        // TODO this might mess up scope but I'm not 100% sure.
        for {
          paramRegs <- getNewRegisters(paramNames.size)
          stackParamRegs <- getNewRegisters(paramNames.size)
          addedOps = paramRegs
            .lazyZip(stackParamRegs)
            .lazyZip(paramTypes)
            .flatMap((paramReg, stackParamReg, t) => {
              List(
                Alloc(stackParamReg, t.size(), t.alignment()),
                Store(paramReg, stackParamReg, width = t.size().toInt)
              )
            })
          addedRegs = paramNames
            .zip(stackParamRegs)
            .map((paramName, stackParamReg) => {
              (paramName, stackParamReg)
            })
          _ <- IRMonad.withNewVariables(addedRegs) // add the variable names
          funLabel = Label(name)
          _ <- IRMonad.emit(Fun(funLabel, paramRegs))
          _ <- IRMonad.emit(addedOps)
          execBody <- lower(body)
        } yield ()
      }

      // TODO: This definitely messes up scope. Fix it.
      case Block(statements) => {
        for {
          loweredStatements <- IRMonad.sequence(statements.map(stmt => {
            lower(stmt)
          }))
        } yield ()
      }

      case TranslationUnit(statements) => {
        for {
          loweredStatements <- IRMonad.sequence(statements.map(stmt => lower(stmt)))
        } yield ()
      }

      case Return(e) => {
        for {
          loweredE <- produceExpression(e)
          _ <- IRMonad.emit(IR.Return(loweredE))
        } yield ()
      }

      case Cast(e, t) => {
        ???
      }

      case Seq(statements) => {
        for {
          loweredStatements <- IRMonad.sequence(
            statements.map(stmt => lower(stmt))
          )
        } yield ()
      }

    }
  }
}
