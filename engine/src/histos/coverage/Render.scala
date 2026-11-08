package histos.coverage

import histos.CoverageFloors

/** Text, scoverage XML, and HTML of one report. Ignored statements are omitted from all three. Skipped cells are named
  * in the text and the HTML. They are not written into the scoverage document.
  */
object Render:
  def text(
      statements: List[Statement],
      invoked: Set[Int],
      floor: CoverageFloors = CoverageFloors(),
      skipped: List[Skipped] = Nil,
  ): String =
    show(Report.of(statements, invoked), floor, skipped)

  def show(report: Report, floor: CoverageFloors, skipped: List[Skipped] = Nil): String =
    val head = List(
      line("statements", report.statements),
      line("branches", report.branches),
    )
    val files  = report.files.map(slice => sliceLine("file", slice))
    val packs  = report.packages.map(slice => sliceLine("package", slice))
    val named  = skipped.map(cell => s"not measured ${cell.module.value} ${cell.version.value} ${cell.platform.token}")
    val misses = Floor.shortfalls(report, floor).map(missLine)
    (head ++ files ++ packs ++ named ++ misses).mkString("\n")

  def xml(statements: List[Statement], invoked: Set[Int], timestamp: Long): String =
    xmlParts(List((statements, invoked)), timestamp)

  /** Each pair is one compilation. Hits are not unioned across pairs, because statement ids collide. */
  def xmlParts(parts: List[(List[Statement], Set[Int])], timestamp: Long): String =
    val report = Report.sum(parts.map((rows, invoked) => Report.of(rows, invoked)))
    val marked =
      parts.flatMap((rows, invoked) => rows.filterNot(_.ignored).map(stmt => (stmt, invoked.contains(stmt.id))))
    val packs = report.packages.map(pack => packageXml(pack, marked.filter(_._1.packageName == pack.name))).mkString
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<scoverage statement-count="${report.statements.total}" statements-invoked="${report.statements.hit}" statement-rate="${report.statements.percent}" branch-rate="${report.branches.percent}" version="1.0" timestamp="$timestamp">
       |  <packages>
       |$packs  </packages>
       |</scoverage>
       |""".stripMargin

  def html(statements: List[Statement], invoked: Set[Int], skipped: List[Skipped] = Nil): String =
    page(Report.of(statements, invoked), skipped)

  def page(report: Report, skipped: List[Skipped] = Nil): String =
    val rows = report.files.map: slice =>
      s"<tr><td>${escape(slice.name)}</td><td>${slice.statements.percent}</td><td>${slice.branches.percent}</td></tr>"
    val named = skipped.map: cell =>
      s"<li>not measured ${escape(cell.module.value)} ${escape(cell.version.value)} ${escape(cell.platform.token)}</li>"
    s"""<!DOCTYPE html>
       |<html><head><meta charset="utf-8"><title>coverage</title></head>
       |<body>
       |<p>statements ${report.statements.hit}/${report.statements.total} ${report.statements.percent}</p>
       |<p>branches ${report.branches.hit}/${report.branches.total} ${report.branches.percent}</p>
       |<ul>
       |${named.mkString("\n")}
       |</ul>
       |<table>
       |${rows.mkString("\n")}
       |</table>
       |</body></html>
       |""".stripMargin

  private def packageXml(slice: Slice, members: List[(Statement, Boolean)]): String =
    val classes = members
      .groupBy(_._1.fullClassName)
      .toList
      .sortBy(_._1)
      .map((name, rows) => classXml(name, rows))
      .mkString
    s"""    <package name="${escape(
        slice.name
      )}" statement-count="${slice.statements.total}" statements-invoked="${slice.statements.hit}" statement-rate="${slice.statements.percent}">
       |      <classes>
       |$classes      </classes>
       |    </package>
       |""".stripMargin

  private def classXml(name: String, members: List[(Statement, Boolean)]): String =
    val source  = members.map(_._1.source).distinct.sorted.mkString(" ")
    val rateS   = counted(members, branch = false)
    val rateB   = counted(members, branch = true)
    val methods = members
      .groupBy(_._1.method)
      .toList
      .sortBy(_._1)
      .map((method, rows) => methodXml(method, rows))
      .mkString
    s"""        <class name="${escape(name)}" filename="${escape(
        source
      )}" statement-count="${rateS.total}" statements-invoked="${rateS.hit}" statement-rate="${rateS.percent}" branch-rate="${rateB.percent}">
       |          <methods>
       |$methods          </methods>
       |        </class>
       |""".stripMargin

  private def methodXml(name: String, rows: List[(Statement, Boolean)]): String =
    val rateS = counted(rows, branch = false)
    val rateB = counted(rows, branch = true)
    val body  = rows.sortBy(_._1.id).map((stmt, hit) => statementXml(stmt, hit)).mkString("\n")
    s"""            <method name="${escape(
        name
      )}" statement-count="${rateS.total}" statements-invoked="${rateS.hit}" statement-rate="${rateS.percent}" branch-rate="${rateB.percent}">
       |              <statements>
       |$body
       |              </statements>
       |            </method>
       |""".stripMargin

  private def statementXml(stmt: Statement, hit: Boolean): String =
    val count = if hit then 1 else 0
    s"""                <statement package="${escape(stmt.packageName)}" class="${escape(
        stmt.className
      )}" class-type="${escape(stmt.classType)}" full-class-name="${escape(stmt.fullClassName)}" source="${escape(
        stmt.source
      )}" method="${escape(
        stmt.method
      )}" start="${stmt.start}" end="${stmt.end}" line="${stmt.line}" branch="${stmt.branch}" invocation-count="$count" ignored="false"/>"""

  private def counted(rows: List[(Statement, Boolean)], branch: Boolean): Rate =
    val kept = rows.filter((stmt, _) => stmt.branch == branch)
    Rate(kept.count(_._2), kept.size)

  private def line(label: String, rate: Rate): String =
    s"$label ${rate.hit}/${rate.total} ${rate.percent}"

  private def sliceLine(kind: String, slice: Slice): String =
    s"$kind ${slice.name} statements ${slice.statements.hit}/${slice.statements.total} ${slice.statements.percent} branches ${slice.branches.hit}/${slice.branches.total} ${slice.branches.percent}"

  private def missLine(miss: Miss): String =
    miss match
      case Miss.Statements(rate, minimum)           => s"miss statements ${rate.percent} is below $minimum"
      case Miss.Branches(rate, minimum)             => s"miss branches ${rate.percent} is below $minimum"
      case Miss.FileStatements(name, rate, minimum) => s"miss file $name statements ${rate.percent} is below $minimum"
      case Miss.FileBranches(name, rate, minimum)   => s"miss file $name branches ${rate.percent} is below $minimum"
      case Miss.PackageStatements(name, rate, minimum) =>
        s"miss package $name statements ${rate.percent} is below $minimum"
      case Miss.PackageBranches(name, rate, minimum) =>
        s"miss package $name branches ${rate.percent} is below $minimum"

  private def escape(raw: String): String =
    raw
      .flatMap: char =>
        val code = char.toInt
        val kept =
          char == '\t' || char == '\n' || char == '\r' || (code >= 0x20 && code <= 0xd7ff) ||
            (code >= 0xe000 && code <= 0xfffd)
        if kept then String.valueOf(char) else ""
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
end Render
