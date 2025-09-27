package tpagu.compiler.parser

import tpagu.compiler.lexer.Token

def declarator: ParseRule[Declarator] = ???
  //directDeclarator


def parameterDeclaration: ParseRule[Declaration] = ???

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
  
  throw new NotImplementedError()

  variableDeclarator <|> paranthesizedDeclaration <|> functionDeclarator
