package histos.version

import histos.central.MavenModule
import histos.doctor.ShipChecks
import histos.ProjectGraph

import zio.{IO, ZIO}

/** Compare each ship's catalog number with the newest stable Coursier lists, and rewrite `.next` when that stable has
  * caught the catalog. The launcher pin is not this effect.
  */
object ShipBumpRun:
  final case class Report(text: String, source: Option[String])

  def run(
      graph: ProjectGraph,
      part: BumpPart,
      names: List[ShipName],
      source: String,
      list: MavenModule => IO[BumpFault, List[ListedStable]],
  ): IO[BumpFault, Report] =
    ZIO
      .fromEither(select(graph, names))
      .flatMap: ships =>
        ZIO
          .foreach(ships)(row(graph, part, _, list))
          .flatMap: rows =>
            ZIO.fromEither(render(source, rows))

  private def row(
      graph: ProjectGraph,
      part: BumpPart,
      ship: Ship.Line,
      list: MavenModule => IO[BumpFault, List[ListedStable]],
  ): IO[BumpFault, Row] =
    ShipChecks.module(graph, ship) match
      case None         => ZIO.succeed(Row.NoCoordinate(ship))
      case Some(module) =>
        list(module).map: listed =>
          Row.Moved(ship, ShipBump.move(ship.number, ShipBump.choose(listed), part))

  private def select(graph: ProjectGraph, names: List[ShipName]): Either[BumpFault, List[Ship.Line]] =
    val lines = graph.catalog.ships.collect { case line: Ship.Line => line }
    names match
      case Nil   => Right(lines)
      case asked =>
        asked.foldLeft[Either[BumpFault, List[Ship.Line]]](Right(Nil)): (acc, name) =>
          acc.flatMap: seen =>
            lines.find(_.name == name) match
              case Some(line) => Right(seen :+ line)
              case None       => Left(BumpFault.MissingShip(name))

  private def render(source: String, rows: List[Row]): Either[BumpFault, Report] =
    val due = rows
      .collect { case row: Row.Moved => row }
      .collect:
        case Row.Moved(ship, due: ShipMove.Due) => (ship, due)
    due
      .foldLeft[Either[BumpFault, String]](Right(source)): (acc, pair) =>
        val (ship, move) = pair
        acc.flatMap: current =>
          ShipBump.rewrite(current, ship.name, move.next).left.map(BumpFault.Rewrite(_))
      .map: rewritten =>
        val body = rows.map(line) match
          case Nil   => "no catalog"
          case found => found.mkString("\n")
        val written = if due.isEmpty then None else Some(rewritten)
        Report(s"$body\n", written)

  private def line(row: Row): String =
    row match
      case Row.NoCoordinate(ship) =>
        s"${ship.name.value} publishes no coordinate"
      case Row.Moved(ship, ShipMove.Unpublished) =>
        s"${ship.name.value} has no stable release"
      case Row.Moved(ship, ShipMove.Ahead(catalog, published, place)) =>
        s"${ship.name.value} ${catalog.render} is ahead of ${published.render} from ${place.word}"
      case Row.Moved(ship, ShipMove.Due(published, place, next)) =>
        s"${ship.name.value} ${published.render} from ${place.word} -> ${next.render}"

  private enum Row:
    case NoCoordinate(ship: Ship.Line)
    case Moved(ship: Ship.Line, move: ShipMove)
end ShipBumpRun
