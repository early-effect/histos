package histos.coverage

import histos.jdk.Sha256
import histos.resolve.{Repositories, ResolvedArtifact, Repository, ToolId, ToolRole, ToolVariant, Transitivity}
import histos.workspace.Workspace
import histos.{CoverageFloors, Module, Platform, ProjectName, ScalaVersion, ToolRevision}

import java.nio.file.Path

import zio.test.*

object CoverageLawSpec extends ZIOSpecDefault:
  private val inventory =
    """# Coverage data, format version: 3.0
      |# Statement data:
      |1
      |src/A.scala
      |demo
      |A
      |Class
      |demo.A
      |m
      |0
      |4
      |3
      |demo.A.m
      |Apply
      |false
      |0
      |false
      |call
      |
      |2
      |src/A.scala
      |demo
      |A
      |Class
      |demo.A
      |m
      |5
      |8
      |4
      |demo.A.m
      |If
      |true
      |0
      |false
      |branch
      |
      |3
      |src/B.scala
      |other
      |B
      |Object
      |other.B
      |n
      |0
      |1
      |1
      |other.B.n
      |Ident
      |false
      |0
      |true
      |skip
      |
      |""".stripMargin

  def spec = suite("coverage laws")(
    test("a branch is only a branch, and an unknown id does not count"):
      val report = Inventory.parse(inventory).map(rows => Report.of(rows, Set(1, 99)))
      assertTrue(
        report.map(_.statements) == Right(Rate(1, 1)),
        report.map(_.branches) == Right(Rate(0, 1)),
        report.map(_.files.map(_.name)) == Right(List("src/A.scala")),
        report.map(_.packages.map(_.name)) == Right(List("demo")),
      )
    ,
    test("a bad version, a cut statement, and a bad measurement are typed errors"):
      assertTrue(
        Inventory.parse("nope") == Left(CoverageError.BadVersion("nope")),
        Inventory.parse(s"${Inventory.Header}\n1\n") == Left(CoverageError.Truncated),
        Measurements.parse("1\n\n2\n") == Right(Set(1, 2)),
        Measurements.parse("no") == Left(CoverageError.BadNumber("measurement", "no")),
      )
    ,
    test("an empty inventory meets only a zero total floor"):
      val empty = Report.of(Nil, Set.empty)
      val asked = CoverageFloors(
        statements = Some(1),
        branches = Some(1),
        fileStatements = Some(1),
        fileBranches = Some(1),
        packageStatements = Some(1),
        packageBranches = Some(1),
      )
      assertTrue(
        empty.statements.percent == "0.00",
        Rate(1, 3).percent == "33.33",
        Rate(2, 3).percent == "66.67",
        Floor.shortfalls(empty, CoverageFloors()) == Nil,
        Floor.shortfalls(empty, CoverageFloors(statements = Some(0))) == Nil,
        Floor.shortfalls(empty, CoverageFloors(fileStatements = Some(90))) == Nil,
        Floor.shortfalls(empty, asked) == List(
          Miss.Statements(Rate(0, 0), 1),
          Miss.Branches(Rate(0, 0), 1),
        ),
      )
    ,
    test("a branch-only file is a branch rate and an empty statement rate"):
      val row    = statement(1, "src/C.scala", "demo", branch = true, ignored = false)
      val report = Report.of(List(row), Set(1))
      assertTrue(
        report.statements == Rate(0, 0),
        report.branches == Rate(1, 1),
        Floor.shortfalls(report, CoverageFloors(fileStatements = Some(1))) == List(
          Miss.FileStatements("src/C.scala", Rate(0, 0), 1)
        ),
      )
    ,
    test("text, xml, and html are three views of one report"):
      Inventory.parse(inventory) match
        case left @ Left(_) => assertTrue(left == Right(List.empty[Statement]))
        case Right(rows)    =>
          val floor = CoverageFloors(branches = Some(1))
          val text  = Render.text(rows, Set(1), floor)
          val xml   = Render.xml(rows, Set(1), 7L)
          val html  = Render.html(rows, Set(1))
          val named = List(Skipped(ProjectName.trusted("core"), ScalaVersion.trusted("3.10.0"), Platform.Js))
          val shown = Render.show(Report.of(rows, Set(1)), CoverageFloors(), named)
          val page  = Render.page(Report.of(rows, Set(1)), named)
          assertTrue(
            text.contains("statements 1/1 100.00"),
            text.contains("branches 0/1 0.00"),
            text.contains("file src/A.scala statements 1/1 100.00 branches 0/1 0.00"),
            text.contains("miss branches 0.00 is below 1"),
            xml.contains("""statement-count="1""""),
            xml.contains("""statement-rate="100.00""""),
            xml.contains("""branch-rate="0.00""""),
            xml.contains("""timestamp="7""""),
            xml.contains("""branch="true""""),
            !xml.contains("other.B"),
            !xml.contains("not measured"),
            html.contains("src/A.scala"),
            html.contains("100.00"),
            !html.contains("src/B.scala"),
            shown.contains("not measured core 3.10.0 js"),
            page.contains("not measured core 3.10.0 js"),
          )
    ,
    test("xml escapes the characters the document forbids"):
      val row = statement(1, "a&b<c>\"", "demo", branch = false, ignored = false)
      val xml = Render.xml(List(row), Set.empty, 7L)
      assertTrue(xml.contains("a&amp;b&lt;c&gt;&quot;"))
    ,
    test("two compilations add rates and do not union ids"):
      val one    = Report.of(List(statement(1, "src/A.scala", "demo", branch = false, ignored = false)), Set(1))
      val summed = Report.sum(List(one, one))
      assertTrue(summed.statements == Rate(2, 2), summed.files.map(_.statements) == List(Rate(2, 2)))
    ,
    test("scala 3 keeps its classpath and scala 2.13 gains the runtime"):
      val classes = List(Path.of("/classes"))
      val jar     = Path.of("/runtime.jar")
      val scala3  = ScalaVersion.trusted("3.10.0")
      val scala2  = ScalaVersion.trusted("2.13.18")
      assertTrue(
        Classpath.tests(classes, scala3, Some(jar)) == Right(classes),
        Classpath.tests(classes, scala2, Some(jar)) == Right(classes :+ jar),
        Classpath.tests(classes, scala2, None) == Left(ClasspathGap.MissingRuntime),
      )
    ,
    test("scala 3 and scala 2.13 instrument, and the other cells are named"):
      val repo    = Path.of("/repo")
      val name    = ProjectName.trusted("core")
      val scala3  = ScalaVersion.trusted("3.10.0")
      val scala2  = ScalaVersion.trusted("2.13.18")
      val data    = Flags.classesDir(repo, name, scala3, Platform.Jvm)
      val plugin  = CoveragePlugin(scala2, Path.of("/plugin.jar"), Nil, Path.of("/runtime.jar"))
      val third   = CoverageCompile(Excludes(Nil, Nil), ToolRevision("2.5.2"), Nil)
      val second  = CoverageCompile(Excludes(List("a"), List("c")), ToolRevision("2.5.2"), List(plugin))
      val scala3d = Cell.instrumented(repo, name, scala3, Platform.Jvm, third)
      val scala2d = Cell.instrumented(repo, name, scala2, Platform.Jvm, second)
      val script  = Cell.instrumented(repo, name, scala3, Platform.Js, third)
      val missing = Cell.instrumented(repo, name, scala2, Platform.Jvm, third)
      assertTrue(
        scala3d.map(_.argv) == Right(Flags.scala3(data, Excludes(Nil, Nil))),
        scala3d.map(done => tail(done.identity)) == Right(Some("")),
        scala3d.map(_.tools) == Right(Nil),
        scala2d.exists(_.argv.lastOption.contains("-Yrangepos")),
        scala2d.exists(_.argv.exists(_.contains("-Xplugin:"))),
        scala2d.map(done => tail(done.identity)) == Right(Some("2.5.2")),
        script == Left(CellFault.Unmeasured(scala3, Platform.Js)),
        missing == Left(CellFault.Plugin(scala2)),
      )
    ,
    test("the series exclusion flags use that series separator"):
      val data     = Path.of("/data")
      val root     = Path.of("/root")
      val plugin   = Path.of("/plugin.jar")
      val excludes = Excludes(List("a", "b"), List("c"))
      val third    = Flags.scala3(data, excludes)
      val second   = Flags.scala2(data, root, plugin, excludes)
      val empty    = Excludes(Nil, Nil)
      assertTrue(
        third == List(
          s"-coverage-out:${data.toAbsolutePath.normalize}",
          "-coverage-exclude-classlikes:a,b",
          "-coverage-exclude-files:c",
        ),
        second.contains("-P:scoverage:excludedPackages:a;b"),
        second.contains("-P:scoverage:excludedFiles:c"),
        !second.exists(_.contains("-P:scoverage:reportTestName")),
        Flags.identity("3", data, root, empty, "") != Flags.identity("3", Path.of("/other"), root, empty, ""),
      )
    ,
    test("a bad exclusion regex is the kind that failed"):
      assertTrue(
        Excludes.check(List("("), Nil) == Left(CoverageError.BadRegex(ExcludeKind.Classlike, "(")),
        Excludes.check(Nil, List("[")) == Left(CoverageError.BadRegex(ExcludeKind.File, "[")),
        Excludes.check(List("a.+"), List("b")) == Right(Excludes(List("a.+"), List("b"))),
      )
    ,
    test("the scala 2.13 request is the full plugin and the binary runtime"):
      val version = ScalaVersion.trusted("2.13.18")
      val request = Tools.request(version, ToolRevision("2.5.2"), Repositories.central)
      assertTrue(
        request.id == ToolId(ToolRole.Scoverage, version, ToolVariant.Jvm),
        request.roots.map(root => (root.organization, root.name, root.revision, root.transitivity)) == List(
          ("org.scoverage", "scalac-scoverage-plugin_2.13.18", "2.5.2", Transitivity.Closure),
          ("org.scoverage", "scalac-scoverage-runtime_2.13", "2.5.2", Transitivity.Single),
        ),
      )
    ,
    test("the plugin selection keeps the compiler jars and drops the runtime and the other platforms"):
      val version    = ScalaVersion.trusted("2.13.18")
      val plugin     = artifact("scalac-scoverage-plugin_2.13.18")
      val domain     = artifact("scalac-scoverage-domain_2.13")
      val serializer = artifact("scalac-scoverage-serializer_2.13")
      val runtime    = artifact("scalac-scoverage-runtime_2.13")
      val javascript = artifact("scalac-scoverage-runtime_sjs1_2.13")
      val native     = artifact("scalac-scoverage-plugin_native0.5_2.13")
      val library    = artifact("scala-library")
      val selected   = CoveragePlugin.from(
        version,
        List(plugin, domain, serializer, runtime, javascript, native, library),
      )
      assertTrue(
        selected.map(_.jar) == Right(plugin.path),
        selected.map(_.runtime) == Right(runtime.path),
        selected.map(_.tools.flatMap(_.artifacts.map(_.name))) == Right(
          List(plugin.name, domain.name, serializer.name)
        ),
        CoveragePlugin.from(version, List(plugin)) == Left(CellFault.Plugin(version)),
      )
    ,
    test("coverage classes are not the product classes"):
      val repo = Path.of("/repo")
      val dir  = Flags.classesDir(repo, ProjectName.trusted("core"), ScalaVersion.trusted("3.10.0"), Platform.Jvm)
      assertTrue(dir.endsWith(Path.of(".histos/coverage/classes/core/3.10.0/jvm")))
    ,
    test("a missing directory is an empty report, and class files without an inventory fail"):
      Workspace
        .work("histos-coverage")
        .flatMap: root =>
          val classes = root.resolve("classes")
          for
            missing <- Gather.read(root.resolve("absent"))
            _       <- Workspace.createDirectories(classes)
            _       <- Workspace.writeString(classes.resolve("A.class"), "x")
            bare    <- Gather.read(classes).either
          yield assertTrue(
            missing == ((Nil, Set.empty)),
            bare == Left(CoverageError.MissingInventory(classes)),
          )
    ,
    test("forget drops measurements and keeps the inventory"):
      Workspace
        .work("histos-coverage")
        .flatMap: root =>
          val inventory   = root.resolve(Gather.InventoryName)
          val measurement = root.resolve(s"${Measurements.Prefix}0")
          for
            _    <- Workspace.writeString(inventory, "keep")
            _    <- Workspace.writeString(measurement, "1\n")
            _    <- Measurements.forget(root.resolve("absent"))
            _    <- Measurements.forget(root)
            kept <- Workspace.readIfPresent(inventory)
            gone <- Workspace.readIfPresent(measurement)
          yield assertTrue(kept.contains("keep"), gone.isEmpty)
    ,
    test("restore puts back the measurements a later compile appended"):
      Workspace
        .work("histos-coverage")
        .flatMap: root =>
          val kept  = root.resolve(s"${Measurements.Prefix}kept")
          val added = root.resolve(s"${Measurements.Prefix}added")
          for
            _        <- Workspace.writeString(kept, "1\n")
            held     <- Measurements.hold(List(root))
            _        <- Workspace.writeString(kept, "1\n9\n")
            _        <- Workspace.writeString(added, "4\n")
            _        <- Measurements.restore(held)
            original <- Workspace.readIfPresent(kept)
            extra    <- Workspace.readIfPresent(added)
          yield assertTrue(original.contains("1\n"), extra.isEmpty)
    ,
    test("javascript and native are named and not instrumented"):
      val built = histos.Projects(
        histos
          .module("core", Platform.Jvm, Platform.Js, Platform.Native)(scala3, scala2)
          .clang(histos.ClangMajor(21))
      )
      built match
        case Left(error)  => assertTrue(error.productPrefix == "rendered")
        case Right(graph) =>
          val modules = graph.projects.collect { case module: Module => module }
          val named   = Cell.skipped(modules)
          assertTrue(
            named.map(_.platform) == List(Platform.Js, Platform.Native, Platform.Js, Platform.Native),
            Cell.versions(graph) == Set(scala3.value, scala2.value),
          ),
  )

  private val scala3 = ScalaVersion.trusted("3.10.0")
  private val scala2 = ScalaVersion.trusted("2.13.18")

  private def tail(text: String): Option[String] =
    text.split("\n", -1).toList.lastOption

  private def statement(
      id: Int,
      source: String,
      packageName: String,
      branch: Boolean,
      ignored: Boolean,
  ): Statement =
    Statement(
      id,
      source,
      packageName,
      "A",
      "Class",
      s"$packageName.A",
      "m",
      0,
      1,
      1,
      "sym",
      "Apply",
      branch,
      ignored,
      "call",
    )

  private def artifact(name: String): ResolvedArtifact =
    ResolvedArtifact(
      "org.scoverage",
      name,
      "2.5.2",
      Path.of(s"/$name.jar"),
      Sha256.trusted("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
      Repository.Central,
    )
end CoverageLawSpec
