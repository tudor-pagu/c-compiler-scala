package tpagu.compiler.parser
import tpagu.compiler.Spanned
import tpagu.compiler.typeChecker.Type

type AstExt = Spanned[AstExtKind]
// Extended Ast
// (as parsed directly from the grammar, before desugaring)

case class Declaration(
    declarationSpecifiers: List[DeclarationSpecifier],
    declarator: Declarator
) {

  override def toString(): String = {
    val specs = declarationSpecifiers.map(_.toString).mkString(" ")
    s"$specs ${declarator.toString}"
  }
}

enum AstExtKind:
  case IntLiteral(value: Int)
  case StringLiteral(value: String)
  case CharLiteral(value: Char)
  case Binary(op: BinaryOp, l: AstExt, r: AstExt)
  case PrefixOperation(op: PrefixOp, e: AstExt)
  case PostfixOperation(op: PostfixOp, e: AstExt)
  case Identifier(name: String)
  case DeclarationList(
      declSpecifiers: List[DeclarationSpecifier],
      initDeclaratorList: List[(Declarator, Option[AstExt])]
  )
  case FunctionDefinition(declaration: Declaration, body: AstExt)
  case Block(statements: List[AstExt])
  case TranslationUnit(statements: List[AstExt])
  case ExprStatement(e: AstExt)

  case Assignment(left: AstExt, right: AstExt)

  case Nothing
  case Return(e: AstExt)

  override def toString(): String = this match {
    case AstExtKind.IntLiteral(value)    => s"Int($value)"
    case AstExtKind.StringLiteral(value) => s"String(\"$value\")"
    case AstExtKind.CharLiteral(value)   => s"Char('$value')"
    case AstExtKind.Identifier(name)     => s"Id($name)"
    case AstExtKind.Binary(op, left, right) =>
      s"${op.toString}(${left.toString}, ${right.toString})"
    case AstExtKind.PrefixOperation(op, expr) =>
      s"${op.toString}(${expr.toString})"
    case AstExtKind.PostfixOperation(op, expr) =>
      s"${expr.toString}{${op.toString}}"
    case AstExtKind.DeclarationList(declSpecifiers, decls) =>
      val declStrings = decls.map { case (decl, init) =>
        init match {
          case Some(initExpr) => s"${decl.toString} = ${initExpr.toString}"
          case None           => decl.toString
        }
      }
      s"Declaration((${declSpecifiers.mkString(",")}),[${declStrings.mkString(", ")}])"
    case AstExtKind.Block(statements) =>
      val stmtStrings = statements.map(_.toString).mkString("; ")
      s"Block({ $stmtStrings })"
    case AstExtKind.FunctionDefinition(declaration, body) =>
      s"FuncDef(${declaration.toString}, ${body.toString})"
    case AstExtKind.TranslationUnit(statements) =>
      val stmtStrings = statements.map(_.toString).mkString("; ")
      s"TU({ $stmtStrings })"
    case AstExtKind.ExprStatement(e) =>
      s"ExprStmt(${e.toString})"
    case AstExtKind.Nothing                 => "Nothing"
    case Return(e)                          => s"Return(${e.toString})"
    case AstExtKind.Assignment(left, right) => s"Assignment($left, $right)"
  }

enum DirectDeclarator:
  case Variable(
      name: Option[String]
  ) // declarator can be absent in abstract declarators (e.g. "int" by itself)
  case Function(name: Option[String], params: List[Declaration])
  case InnerDeclarator(decl: Declarator)

  def getName(): Option[String] = this match {
    case Variable(name)         => name
    case Function(name, params) => name
    case InnerDeclarator(decl)  => decl.direct.getName()
  }

  override def toString: String = this match {
    case DirectDeclarator.Variable(name) => s"Var($name)"
    case DirectDeclarator.Function(name, params) =>
      val paramStrings = params.map(_.toString).mkString(", ")
      s"Func($name, [$paramStrings])"
    case DirectDeclarator.InnerDeclarator(decl) => s"(${decl.toString})"
  }

case class Pointer(qualifiers: List[TypeQualifier]) {
  override def toString: String = {
    val quals =
      if (qualifiers.isEmpty) ""
      else "(" + qualifiers.map(_.toString).mkString(",") + ")"
    s"*$quals"
  }
}

case class Declarator(pointers: List[Pointer], direct: DirectDeclarator) {
  override def toString: String = {
    val ptrs = pointers.map(_.toString).mkString("")
    s"$ptrs${direct.toString}"
  }
}

sealed trait DeclarationSpecifier

sealed trait TypeSpecifier extends DeclarationSpecifier

object TypeSpecifier {
  case object Int extends TypeSpecifier {
    override def toString() = "int"
  }

  case object Double extends TypeSpecifier {
    override def toString() = "double"
  }
  case object Long extends TypeSpecifier {
    override def toString() = "long"
  }
  case object Short extends TypeSpecifier {
    override def toString() = "short"
  }
  case object Struct extends TypeSpecifier {
    override def toString() = "struct"
  }
  case object Signed extends TypeSpecifier {
    override def toString() = "signed"
  }
  case object Unsigned extends TypeSpecifier {
    override def toString() = "unsigned"
  }

  case class TypedefName(name: String) extends TypeSpecifier {
    override def toString() = s"${name}(typedef)"
  }
}

sealed trait StorageClassSpecifier extends DeclarationSpecifier

object StorageClassSpecifier {
  case object Typedef extends StorageClassSpecifier {
    override def toString() = "typedef"
  }
}

sealed trait TypeQualifier extends DeclarationSpecifier

object TypeQualifier {
  case object Const extends TypeQualifier {
    override def toString(): String = "const"
  }

  case object Volatile extends TypeQualifier {
    override def toString(): String = "volatile"
  }
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
  case Dereference // *a
  case AddressOf // &a
  override def toString: String = this match {
    case PrefixOp.Negation    => "Neg"
    case PrefixOp.UnaryPlus   => "Plus"
    case PrefixOp.Dereference => "*"
    case PrefixOp.AddressOf   => "&"
  }

enum PostfixOp:
  // case Increment // e.g. x++
  // case Decrement // e.g. x--
  case FunctionCall(args: List[AstExt]) // e.g. f(2, a)
  override def toString: String = this match {
    // case Increment => "Inc"
    // case Decrement => "Dec"
    case PostfixOp.FunctionCall(args) =>
      val argStrings = args.map(_.toString).mkString(", ")
      s"Call([$argStrings])"
  }

// This represents a type as it appears in the source code. It is used for diagnostics, etc.
// enum TypeExtKind:
//   case Int
//   case Char
//   case Function(returnType: TypeExt, paramTypes: List[TypeExt])
//   case Array(elementType: TypeExt, size: Int)
//   case Pointer(to: TypeExt)
//
//
// case class TypeExt(baseType: TypeExtKind, qualifiers: List[TypeQualifier] = List())
