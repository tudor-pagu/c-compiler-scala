package tpagu.compiler.parser
import tpagu.compiler.Spanned

type AstExt = Spanned[AstExtKind]
// Extended Ast
// (as parsed directly from the grammar, before desugaring)
enum AstExtKind:
  case IntLiteral(value: Int)
  case StringLiteral(value: String)
  case CharLiteral(value: Char)
  case Binary(op: BinaryOp, l: AstExt, r: AstExt)
  case PrefixOperation(op: PrefixOp, e: AstExt)
  case PostfixOperation(op: PostfixOp, e: AstExt)
  case Identifier(name: String)
  override def toString(): String = this match {
    case AstExtKind.IntLiteral(value)    => s"Int($value)"
    case AstExtKind.StringLiteral(value) => s"String(\"$value\")"
    case AstExtKind.CharLiteral(value)   => s"Char('$value')"
    case AstExtKind.Identifier(name)     => s"Id($name)"
    case AstExtKind.Binary(op, left, right) =>
      s"${op.toString}(${left.toString}, ${right.toString})"
    case AstExtKind.PrefixOperation(op, expr) =>
      s"${op.toString}(${expr.toString})"
  }

enum BinaryOp:
  case Add
  case Sub
  case Mult
  case Div
  override def toString: String = this match {
    case BinaryOp.Add  => "Add"
    case BinaryOp.Sub  => "Sub"
    case BinaryOp.Mult => "Mult"
    case BinaryOp.Div  => "Div"
  }

enum PrefixOp:
  case Negation

  /** unary plus (e.g. +2) must be parsed into the AST and can't simply be
    * ignored because it will cause the expression to be converted to int.
    * Consider: ```char c = 'a'; printf("test %d\n", sizeof(c));``` prints 1,
    * but ```char c = 'a'; printf("test %d\n", sizeof(+c));``` prints 4
    */
  case UnaryPlus
  override def toString: String = this match {
    case PrefixOp.Negation  => "Neg"
    case PrefixOp.UnaryPlus => "Plus"
  }

enum PostfixOp:
  case Increment // e.g. x++
  case Decrement // e.g. x--
  case FunctionCall(args: List[(AstExt)]) // e.g. f(2, a)
  override def toString: String = this match {
    case Increment => "Inc"
    case Decrement => "Dec"
    case PostfixOp.FunctionCall(args) =>
      val argStrings = args.map(_.toString).mkString(", ")
      s"Call([$argStrings])"
  }
