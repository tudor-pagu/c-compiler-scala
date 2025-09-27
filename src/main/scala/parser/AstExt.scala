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
  case Unary(op: UnaryOp, e: AstExt)
  case Identifier(name: String)
  override def toString(): String = this match {
    case AstExtKind.IntLiteral(value)    => s"Int($value)"
    case AstExtKind.StringLiteral(value) => s"String(\"$value\")"
    case AstExtKind.CharLiteral(value)   => s"Char('$value')"
    case AstExtKind.Identifier(name)     => s"Id($name)"
    case AstExtKind.Binary(op, left, right) =>
      s"${op.toString}(${left.toString}, ${right.toString})"
    case AstExtKind.Unary(op, expr) => s"${op.toString}(${expr.toString})"
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

enum UnaryOp:
  case FunctionCall(args: List[(AstExt)])
  case Negation
  override def toString: String = this match {
    case UnaryOp.Negation  => "Neg"
    case UnaryOp.UnaryPlus => "Plus"
    case UnaryOp.FunctionCall(args) =>
      val argStrings = args.map(_.toString).mkString(", ")
      s"Call([$argStrings])"
  }

  /** unary plus (e.g. +2) must be parsed into the AST and can't simply be
    * ignored because it will cause the expression to be converted to int.
    * Consider: ```char c = 'a'; printf("test %d\n", sizeof(c));``` prints 1,
    * but ```char c = 'a'; printf("test %d\n", sizeof(+c));``` prints 4
    */
  case UnaryPlus
