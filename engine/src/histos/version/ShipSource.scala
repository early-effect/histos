package histos.version

import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

/** Rewrite each named ship's `.next` literal arguments.
  *
  * The receiver may be `Ship("name")`, a val, or any other expression of that ship. The three arguments are integer
  * literals, on one line or several. A comment outside a literal stays. The caller formats the file afterwards.
  */
private[version] object ShipSource:
  def rewrite(source: String, edits: List[(ShipName, ShipNumber)]): Either[ShipBump.Rewrite, String] =
    if edits.isEmpty then Right(source)
    else
      val reporter  = new StoreReporter()
      given Context = (new ContextBase).initialCtx.fresh.setReporter(reporter)
      scala.util
        .Try(new Parser(SourceFile.virtual("histos.scala", source)).parse())
        .toEither
        .left
        .map(thrown => ShipBump.Rewrite.Unparsed(reason(thrown)))
        .flatMap: tree =>
          if reporter.errorsReported then Left(ShipBump.Rewrite.Unparsed("histos.scala did not parse"))
          else applyEdits(source, edits, collect(tree))

  private def reason(thrown: Throwable): String =
    Option(thrown.getMessage).filter(_.nonEmpty).getOrElse(thrown.getClass.getName)

  private def applyEdits(
      source: String,
      edits: List[(ShipName, ShipNumber)],
      found: List[Found],
  ): Either[ShipBump.Rewrite, String] =
    edits
      .foldLeft[Either[ShipBump.Rewrite, List[Piece]]](Right(Nil)): (acc, edit) =>
        val (name, number) = edit
        acc.flatMap: seen =>
          val ready = found.collect:
            case Found.Ready(ship, major, minor, patch) if ship == name.value =>
              List(
                Piece(name, major, number.major.toString),
                Piece(name, minor, number.minor.toString),
                Piece(name, patch, number.patch.toString),
              )
          val broken = found.exists:
            case Found.Broken(ship)      => ship == name.value
            case Found.Ready(_, _, _, _) => false
          if broken then Left(ShipBump.Rewrite.Shape(name))
          else
            ready.flatten match
              case Nil    => Left(ShipBump.Rewrite.Missing(name))
              case pieces => Right(seen ++ pieces)
      .flatMap: pieces =>
        problem(source, pieces) match
          case Some(ship) => Left(ShipBump.Rewrite.Shape(ship))
          case None       => Right(splice(source, pieces))

  private def problem(source: String, pieces: List[Piece]): Option[ShipName] =
    val broken = pieces.collectFirst:
      case piece if piece.at.start < 0 || piece.at.end > source.length || piece.at.end <= piece.at.start =>
        piece.ship
    broken.orElse:
      val ordered = pieces.sortBy(_.at.start)
      ordered
        .zip(ordered.drop(1))
        .collectFirst:
          case (left, right) if right.at.start < left.at.end => right.ship

  private def splice(source: String, pieces: List[Piece]): String =
    pieces
      .sortBy(piece => -piece.at.start)
      .foldLeft(source): (text, piece) =>
        text.substring(0, piece.at.start) + piece.value + text.substring(piece.at.end)

  private def collect(tree: Tree)(using Context): List[Found] =
    tree match
      case PackageDef(_, stats) => statements(stats, Map.empty)._1
      case other                => scan(other, Map.empty)

  private def statements(trees: List[Tree], env: Env)(using Context): (List[Found], Env) =
    trees.foldLeft((List.empty[Found], env)): (acc, tree) =>
      val (found, bound) = acc
      val (more, next)   = one(tree, bound)
      (found ++ more, next)

  private def one(tree: Tree, env: Env)(using Context): (List[Found], Env) =
    tree match
      case valDef: ValDef =>
        val found = scan(valDef.tpt, env) ++ scan(valDef.rhs, env)
        (found, bind(valDef.name.toString, valDef.rhs, env))
      case defDef: DefDef =>
        val found = scan(defDef.tpt, env) ++ scan(defDef.rhs, env)
        (found, bind(defDef.name.toString, defDef.rhs, env))
      case typeDef: TypeDef =>
        (scan(typeDef.rhs, env), env)
      case other =>
        (scan(other, env), env)

  private def bind(name: String, rhs: Tree, env: Env)(using Context): Env =
    if name.nonEmpty && name.head.isLetter then
      shipOf(rhs, env) match
        case Some(ship) => env.updated(name, ship)
        case None       => env
    else env

  private def scan(tree: Tree, env: Env)(using Context): List[Found] =
    tree match
      case Apply(Select(recv, name), args) if name.toString == "next" =>
        val nested = scan(recv, env) ++ args.flatMap(scan(_, env))
        shipOf(recv, env) match
          case Some(ship) => found(ship, args) :: nested
          case None       => nested
      case Apply(fun, args)     => scan(fun, env) ++ args.flatMap(scan(_, env))
      case TypeApply(fun, args) => scan(fun, env) ++ args.flatMap(scan(_, env))
      case Select(qualifier, _) => scan(qualifier, env)
      case Parens(inner)        => scan(inner, env)
      case Typed(expr, tpt)     => scan(expr, env) ++ scan(tpt, env)
      case NamedArg(_, arg)     => scan(arg, env)
      case Assign(lhs, rhs)     => scan(lhs, env) ++ scan(rhs, env)
      case Block(stats, expr)   =>
        val (found, bound) = statements(stats, env)
        found ++ scan(expr, bound)
      case If(cond, thenp, elsep)    => scan(cond, env) ++ scan(thenp, env) ++ scan(elsep, env)
      case Match(selector, cases)    => scan(selector, env) ++ cases.flatMap(scan(_, env))
      case CaseDef(pat, guard, body) =>
        scan(pat, env) ++ scan(guard, env) ++ scan(body, env)
      case Try(expr, cases, finalizer) =>
        scan(expr, env) ++ cases.flatMap(scan(_, env)) ++ scan(finalizer, env)
      case WhileDo(cond, body)   => scan(cond, env) ++ scan(body, env)
      case Return(expr, from)    => scan(expr, env) ++ scan(from, env)
      case Annotated(arg, annot) => scan(arg, env) ++ scan(annot, env)
      case New(tpt)              => scan(tpt, env)
      case Super(qual, _)        => scan(qual, env)
      case Thicket(trees)        => statements(trees, env)._1
      case template: Template    =>
        val (found, _) = statements(template.body, env)
        found ++ scan(template.constr, env) ++ template.parents.flatMap(scan(_, env))
      case typeDef: TypeDef                               => scan(typeDef.rhs, env)
      case valDef: ValDef                                 => scan(valDef.tpt, env) ++ scan(valDef.rhs, env)
      case defDef: DefDef                                 => scan(defDef.tpt, env) ++ scan(defDef.rhs, env)
      case Ident(_) | Literal(_) | Number(_, _) | This(_) => Nil
      case other                                          =>
        other.productIterator
          .flatMap:
            case child: Tree       => scan(child, env)
            case children: List[?] =>
              children.collect { case child: Tree => scan(child, env) }.flatten
            case _ => Nil
          .toList

  private def found(ship: String, args: List[Tree])(using Context): Found =
    args match
      case List(major, minor, patch) =>
        (literalAt(major), literalAt(minor), literalAt(patch)) match
          case (Some(majorAt), Some(minorAt), Some(patchAt)) => Found.Ready(ship, majorAt, minorAt, patchAt)
          case _                                             => Found.Broken(ship)
      case _ => Found.Broken(ship)

  private def literalAt(tree: Tree)(using Context): Option[At] =
    tree match
      case Parens(inner)    => literalAt(inner)
      case NamedArg(_, arg) => literalAt(arg)
      case Typed(expr, _)   => literalAt(expr)
      case Number(_, kind)  =>
        kind match
          case NumberKind.Whole(_)                      => placed(tree)
          case NumberKind.Decimal | NumberKind.Floating => None
      case _ => None

  private def placed(tree: Tree)(using Context): Option[At] =
    val span = tree.span
    if span.exists && span.start >= 0 && span.end > span.start then Some(At(span.start, span.end)) else None

  private def shipOf(tree: Tree, env: Env)(using Context): Option[String] =
    tree match
      case Apply(fun, List(arg)) if isShip(fun) => literalString(arg)
      case Ident(name)                          => env.get(name.toString)
      case Parens(inner)                        => shipOf(inner, env)
      case Typed(expr, _)                       => shipOf(expr, env)
      case Block(stats, expr)                   =>
        val (_, bound) = statements(stats, env)
        shipOf(expr, bound)
      case _ => None

  private def isShip(fun: Tree): Boolean =
    fun match
      case Ident(name)     => name.toString == "Ship"
      case Select(_, name) => name.toString == "Ship"
      case Parens(inner)   => isShip(inner)
      case Typed(expr, _)  => isShip(expr)
      case _               => false

  private def literalString(tree: Tree): Option[String] =
    tree match
      case Literal(const) =>
        const.value match
          case text: String => Some(text)
          case _            => None
      case Parens(inner) => literalString(inner)
      case _             => None

  private enum Found:
    case Ready(ship: String, major: At, minor: At, patch: At)
    case Broken(ship: String)

  private final case class At(start: Int, end: Int)
  private final case class Piece(ship: ShipName, at: At, value: String)
  private type Env = Map[String, String]
end ShipSource
