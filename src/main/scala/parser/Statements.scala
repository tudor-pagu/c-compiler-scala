package tpagu.compiler.parser

import tpagu.compiler.lexer.Token
import tpagu.compiler.lexer.Lexer
import tpagu.compiler.CompilerError

def declarator: ParseRule[Declarator] = ???
  //directDeclarator

def typeSpecifier: ParseRule[DeclarationSpecifier] = 
  new ParseRule[DeclarationSpecifier] {
    def parse(lexer: Lexer): Either[CompilerError, Out[DeclarationSpecifier]] = 
      lexer.nextToken() match {
        case Left(err) => Left(err)
        case Right((Token.Type, lexer))

      }
  }

def declarationSpecifier: ParseRule[DeclarationSpecifier] = {
  typeSpecifier
}

def parameterDeclaration: ParseRule[Declaration] = 
  for {
    specs <- listOf(declarationSpecifier)
    decl <- declarator
  } yield Right(Declaration(specs, decl))


def functionDeclarator: ParseRule[DirectDeclarator] = 
  for {
    case AstExt(AstExtKind.Identifier(name),_) <- identifier
    _ <- Just(Token.OpenParen)
    params <- listOf(parameterDeclaration)
  } yield Right(DirectDeclarator.Function(name, params))

def directDeclarator: ParseRule[DirectDeclarator] = 
  val paranthesizedDeclaration = for {
    _ <- Just(Token.OpenParen)
    d <- declarator
    _ <- Just(Token.CloseParen)
  } yield Right(DirectDeclarator.InnerDeclarator(d))

  val variableDeclarator = for {
    case AstExt(AstExtKind.Identifier(name),_) <- identifier
  } yield Right(DirectDeclarator.Variable(name))
  
  variableDeclarator <|> paranthesizedDeclaration <|> functionDeclarator
