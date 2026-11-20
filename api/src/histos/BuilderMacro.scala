package histos

import _root_.scala.quoted.*

/** Expands `module`, `aggregate`, and `ship`. */
object BuilderMacro:
  def module(
      name: Expr[String],
      platform: Expr[Platform],
      platforms: Expr[Seq[Platform]],
      version: Expr[ScalaVersion],
      versions: Expr[Seq[ScalaVersion]],
  )(using Quotes): Expr[ModuleDef[?]] =
    import quotes.reflect.*
    val at    = pos
    val union = unite(distinct(read(platform) :: readAll(platforms)))
    union.asType match
      case '[p] =>
        '{
          ModuleDef.make[p](
            $name,
            Platforms($platform, $platforms.toList),
            ScalaVersions($version, $versions.toList),
            $at,
          )
        }.asTerm.asExprOf[ModuleDef[?]]

  /** First-seen order. A repeated case stays in the runtime list and is still [[BuildError.DuplicatePlatform]]. */
  private def distinct(using Quotes)(parts: List[quotes.reflect.TypeRepr]): List[quotes.reflect.TypeRepr] =
    parts.foldLeft(List.empty[quotes.reflect.TypeRepr]): (seen, part) =>
      if seen.exists(_ =:= part) then seen else seen :+ part

  private def unite(using Quotes)(parts: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    parts match
      case first :: rest => rest.foldLeft(first)((left, right) => OrType(left, right))
      case Nil           => report.errorAndAbort("module takes Jvm, Js, or Native.")

  private def readAll(platforms: Expr[Seq[Platform]])(using Quotes): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    def loop(term: Term): List[TypeRepr] =
      term match
        case Inlined(_, _, inner)                         => loop(inner)
        case Typed(inner, _)                              => loop(inner)
        case Repeated(elems, _)                           => elems.map(platformType)
        case Apply(_, List(Typed(Repeated(elems, _), _))) => elems.map(platformType)
        case other                                        =>
          report.errorAndAbort("module takes Jvm, Js, or Native.", other.pos)
    platforms match
      case Varargs(args) => args.map(read).toList
      case other         => loop(other.asTerm)

  private def read(platform: Expr[Platform])(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    def loop(term: Term): TypeRepr =
      term match
        case Inlined(_, _, inner) => loop(inner)
        case Typed(inner, _)      => loop(inner)
        case other                => platformType(other)
    loop(platform.asTerm)

  /** An enum case, or the package export whose result type is that case. Anything else is not a platform. */
  private def platformType(using Quotes)(term: quotes.reflect.Term): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val found = term.symbol.tree match
      case defn: DefDef => defn.returnTpt.tpe
      case _            => term.tpe
    if found =:= TypeRepr.of[Platform.Jvm.type] then TypeRepr.of[Platform.Jvm.type]
    else if found =:= TypeRepr.of[Platform.Js.type] then TypeRepr.of[Platform.Js.type]
    else if found =:= TypeRepr.of[Platform.Native.type] then TypeRepr.of[Platform.Native.type]
    else report.errorAndAbort("module takes Jvm, Js, or Native.", term.pos)

  def aggregate(
      name: Expr[String],
      project: Expr[ModuleDef[?]],
      projects: Expr[Seq[ModuleDef[?]]],
  )(using Quotes): Expr[AggregateDef] =
    val at = pos
    '{ AggregateDef.make($name, $project :: $projects.toList, $at) }

  def ship(
      name: Expr[String],
      number: Expr[histos.version.ShipNumber],
      member: Expr[ProjectDef],
      members: Expr[Seq[ProjectDef]],
  )(using Quotes): Expr[ShipDef] =
    val at = pos
    '{ ShipDef.line($name, $number, ::($member, $members.toList), $at) }

  private def pos(using Quotes): Expr[SourcePos] =
    import quotes.reflect.Position
    val at = Position.ofMacroExpansion
    '{ SourcePos(${ Expr(at.sourceFile.path) }, ${ Expr(at.startLine + 1) }, ${ Expr(at.startColumn + 1) }) }
end BuilderMacro
