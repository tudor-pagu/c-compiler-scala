def expression: ParseRule[AstNodeExt] =
  primaryExpression

def primaryExpression: ParseRule[AstNodeExt] = for {
  first <- unaryExpression
  repeating <- (OneOf(Token.Times))
}
