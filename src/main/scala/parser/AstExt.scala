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
  case Identifier(name:String)


enum BinaryOp:
  case Add
  case Sub
  case Mult
  case Div

enum UnaryOp:
  case FunctionCall(args: List[(AstExt)])
  case Negation
  /** unary plus (e.g. +2) must be parsed into the AST and can't simply be ignored because it will cause the expression to be converted to int.
   *  Consider: ```char c = 'a'; printf("test %d\n", sizeof(c));``` prints 1, but ```char c = 'a'; printf("test %d\n", sizeof(+c));``` prints 4

  */
  case UnaryPlus
