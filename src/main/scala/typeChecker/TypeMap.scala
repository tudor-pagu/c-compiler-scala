package tpagu.compiler.typeChecker

import tpagu.compiler.AstID
import tpagu.compiler.parser.AstExt

/*
 * The declaration map has a very specific purpose:
 * The (currently) first type checking pass is the ID pass, where
 * we maintain a type environment and compute the type of all identifiers.
 * We *cannot* check assignments which are part of init declarations at this stage,
 * since we haven't done conversions and decay passes yet.
 *
 * However, by the time we get to the assignment checking pass, we need to reconstruct
 * the declared types of the declarations. But we've already lost this information, and
 * the typemap can't hold it since the "declarators" are not themselves AST nodes.
 *
 * So, declaration map maintains specifically the types declared in every declaration (which
 * is itself an AST node) so that it can later be used by the assignment stage.
 *
 */
class TypeMap(
    map: Map[AstID, Type],
    declarationMap: Map[AstID, List[Type]],
    functionMap: Map[AstID, Type]
) {
  def this() = this(Map.empty, Map.empty, Map.empty)

  def apply(key: AstExt): Type =
    map.getOrElse(
      key.id,
      throw new NoSuchElementException(s"No type for AST Node: ${key}")
    )

  def +(kv: (AstExt, Type)): TypeMap =
    TypeMap(map + (kv._1.id -> kv._2), declarationMap, functionMap)

  def updateDeclarationMap(key: AstExt, types: List[Type]) =
    TypeMap(map, declarationMap + (key.id -> types), functionMap)

  def getTypesOfDeclaration(key: AstExt) = declarationMap(key.id)

  def get(key: AstExt): Option[Type] = map.get(key.id)

  def updateFunctionMap(key: AstExt, t : Type) =
    TypeMap(map, declarationMap, functionMap + (key.id -> t))

  def getTypeOfFunction(key: AstExt) = functionMap(key.id)
}

type TypeEnvironment = Map[String, Type]
