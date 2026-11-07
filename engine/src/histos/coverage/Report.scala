package histos.coverage

import histos.CoverageFloors

/** Hits over a counted set. An empty set meets only a minimum of 0. */
final case class Rate(hit: Int, total: Int):
  def meets(minimum: Int): Boolean =
    if total == 0 then minimum <= 0
    else hit.toLong * 100 >= minimum.toLong * total

  /** Two decimal places, half up. `2/3` is `66.67`. An empty rate is `0.00`. */
  def percent: String =
    val scaled =
      if total == 0 then 0L
      else (hit.toLong * 10000 + total.toLong / 2) / total.toLong
    val whole = scaled / 100
    val frac  = (scaled % 100).toInt
    f"$whole%d.$frac%02d"
end Rate

final case class Slice(name: String, statements: Rate, branches: Rate)

final case class Report(
    statements: Rate,
    branches: Rate,
    files: List[Slice],
    packages: List[Slice],
)

object Report:
  /** Ignored statements are in neither rate. An id that is not in the inventory does not count. */
  def of(statements: List[Statement], invoked: Set[Int]): Report =
    val live = statements.filterNot(_.ignored)
    Report(
      rate(live.filterNot(_.branch), invoked),
      rate(live.filter(_.branch), invoked),
      slices(live, invoked)(_.source),
      slices(live, invoked)(_.packageName),
    )

  private def slices(live: List[Statement], invoked: Set[Int])(name: Statement => String): List[Slice] =
    live
      .groupBy(name)
      .toList
      .map: (label, rows) =>
        Slice(label, rate(rows.filterNot(_.branch), invoked), rate(rows.filter(_.branch), invoked))
      .sortBy(_.name)

  private def rate(rows: List[Statement], invoked: Set[Int]): Rate =
    Rate(rows.count(stmt => invoked.contains(stmt.id)), rows.size)

  /** Sum hits and totals. Statement ids are not unioned: each report already counted its own ids. */
  def sum(reports: List[Report]): Report =
    Report(
      add(reports.map(_.statements)),
      add(reports.map(_.branches)),
      merge(reports.flatMap(_.files)),
      merge(reports.flatMap(_.packages)),
    )

  private def add(rates: List[Rate]): Rate =
    Rate(rates.map(_.hit).sum, rates.map(_.total).sum)

  private def merge(slices: List[Slice]): List[Slice] =
    slices
      .groupBy(_.name)
      .toList
      .map: (name, rows) =>
        Slice(name, add(rows.map(_.statements)), add(rows.map(_.branches)))
      .sortBy(_.name)
end Report

enum Miss:
  case Statements(rate: Rate, minimum: Int)
  case Branches(rate: Rate, minimum: Int)
  case FileStatements(name: String, rate: Rate, minimum: Int)
  case FileBranches(name: String, rate: Rate, minimum: Int)
  case PackageStatements(name: String, rate: Rate, minimum: Int)
  case PackageBranches(name: String, rate: Rate, minimum: Int)

object Floor:
  def shortfalls(report: Report, floor: CoverageFloors): List[Miss] =
    val totals =
      List(
        floor.statements.filterNot(report.statements.meets).map(Miss.Statements(report.statements, _)),
        floor.branches.filterNot(report.branches.meets).map(Miss.Branches(report.branches, _)),
      ).flatten
    val files = report.files.flatMap(slice => fileMisses(slice, floor))
    val packs = report.packages.flatMap(slice => packageMisses(slice, floor))
    totals ++ files ++ packs

  private def fileMisses(slice: Slice, floor: CoverageFloors): List[Miss] =
    List(
      floor.fileStatements.filterNot(slice.statements.meets).map(Miss.FileStatements(slice.name, slice.statements, _)),
      floor.fileBranches.filterNot(slice.branches.meets).map(Miss.FileBranches(slice.name, slice.branches, _)),
    ).flatten

  private def packageMisses(slice: Slice, floor: CoverageFloors): List[Miss] =
    List(
      floor.packageStatements
        .filterNot(slice.statements.meets)
        .map(Miss.PackageStatements(slice.name, slice.statements, _)),
      floor.packageBranches
        .filterNot(slice.branches.meets)
        .map(Miss.PackageBranches(slice.name, slice.branches, _)),
    ).flatten
end Floor
