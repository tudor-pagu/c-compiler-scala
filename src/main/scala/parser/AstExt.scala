// Extended Ast 
// (as parsed directly from the grammar, before desugaring)
enum AstNodeExt:
  case IntLiteral(value: Int)
  case StringLiteral(value: String)
  case CharLiteral(value: Char)
  case Binary(op: BinaryOp, l: AstNodeExt, r: AstNodeExt)
  case Unary(op: UnaryOp, e: AstNodeExt)
  case Identifier(name:String)


enum BinaryOp:
  case Add
  case Sub
  case Mult
  case Div

enum UnaryOp:
  case FunctionCall(args: List[(AstNodeExt)])
  case Negation
  /** unary plus (e.g. +2) must be parsed into the AST and can't simply be ignored because it will cause the expression to be converted to int.
   *  Consider: ```char c = 'a'; printf("test %d\n", sizeof(c));``` prints 1, but ```char c = 'a'; printf("test %d\n", sizeof(+c));``` prints 4

  */
  case UnaryPlus
