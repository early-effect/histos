package histos.load

import histos.jdk.Sha256
import histos.plugin.{DeclaredTask, PluginDetail, PluginRunError, Seal, TaskDep, TaskName}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.GraphCompile
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion, SourcePos}

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object LoaderLawSpec extends ZIOSpecDefault:
  private val v213 = ScalaVersion.trusted("2.13.18")
  private val v3   = ScalaVersion.trusted("3.10.0")
  private val core = ProjectName.trusted("core")

  def spec = suite("load laws")(
    suite("a class home and a reserved package are closed sets")(
      test("the api root is a boundary, a deeper histos package is refused, and anything else is open"):
        assertTrue(
          ClassHome.of("histos.HistosBuild") == ClassHome.Boundary,
          ClassHome.of("zio.ZIO") == ClassHome.Boundary,
          ClassHome.of("scala.Predef") == ClassHome.Boundary,
          ClassHome.of("izumi.reflect.Tag") == ClassHome.Boundary,
          ClassHome.of("histos.version.Ship") == ClassHome.Boundary,
          ClassHome.of("histos.plugin.Need") == ClassHome.Boundary,
          ClassHome.of("histos.supply.Plan") == ClassHome.Boundary,
          ClassHome.of("histos.doctor.Check") == ClassHome.Boundary,
          ClassHome.of("histos.schedule.Sendable") == ClassHome.Refused,
          ClassHome.of("histos") == ClassHome.Open,
          ClassHome.of("demo.A") == ClassHome.Open,
        )
      ,
      test("the supplied jars are the api surface, and the list starts with the api jar"):
        val api = Path.of("/api.jar")
        for found <- Boundary.jars(api)
        yield assertTrue(
          Boundary.supplied("scala-library"),
          Boundary.supplied("scala3-library_3"),
          Boundary.supplied("zio_3"),
          Boundary.supplied("zio-stacktracer_3"),
          Boundary.supplied("izumi-reflect_3"),
          Boundary.supplied("histos-api_3"),
          !Boundary.supplied("scala-library-2.13"),
          !Boundary.supplied("zio"),
          found.headOption.contains(api),
          found.distinct == found,
          found.nonEmpty,
        )
      ,
      test("reserved binaries and paths are histos, zio, and scala"):
        assertTrue(
          ReservedPackage.binary("histos"),
          ReservedPackage.binary("histos.A"),
          ReservedPackage.binary("zio"),
          ReservedPackage.binary("zio.A"),
          ReservedPackage.binary("scala"),
          ReservedPackage.binary("scala.Predef"),
          !ReservedPackage.binary("history"),
          !ReservedPackage.binary("demo.A"),
          ReservedPackage.path("histos/A.class"),
          ReservedPackage.path("zio/A.class"),
          ReservedPackage.path("scala/A.class"),
          !ReservedPackage.path("demo/A.class"),
        ),
    ),
    suite("plugin cells and their inputs")(
      test("compile expands the module matrix, link forces javascript, and a missing cell is a failure"):
        val module = mod(core, Platform.Jvm, List(Platform.Js), v213, v3)
        val graph  = ProjectGraph.trusted(List(module))
        val both   = task(
          TaskDep.Same(TaskName.trusted("other")),
          TaskDep.Compile(core, None, None),
          TaskDep.Link(core, None),
        )
        val compiled = List(
          PluginCells.Cell(module, v213, Platform.Js),
          PluginCells.Cell(module, v213, Platform.Jvm),
          PluginCells.Cell(module, v3, Platform.Js),
          PluginCells.Cell(module, v3, Platform.Jvm),
        )
        val linked = compiled.filter(_.platform == Platform.Js)
        val jvm    = mod(core, Platform.Jvm, Nil, v3)
        val only   = ProjectGraph.trusted(List(jvm))
        assertTrue(
          PluginCells.selected(graph, both) == Right(compiled),
          PluginCells.links(graph, both) == Right(linked),
          PluginCells.id(PluginCells.Cell(module, v213, Platform.Js)) == "compile:core:2.13.18:js",
          PluginCells.scriptRecord(Path.of("/repo"), core, v3) ==
            Path.of("/repo/.histos/link/core/3.10.0/script"),
          PluginCells.selected(graph, task(TaskDep.Compile(ProjectName.trusted("missing"), None, None))) ==
            Left(failed("module missing is not in the graph")),
          PluginCells.selected(only, task(TaskDep.Compile(core, Some(v3), Some(Platform.Js)))) ==
            Left(failed("'core' does not build the requested cell")),
          PluginCells.links(only, task(TaskDep.Link(core, Some(v3)))) ==
            Left(failed("'core' does not build the requested cell")),
        )
      ,
      test("a missing link record fails, and changing the script changes the digest"):
        Workspace
          .work("histos-link-digest")
          .flatMap: root =>
            val module = mod(core, Platform.Js, Nil, v3)
            val graph  = ProjectGraph.trusted(List(module))
            val script = root.resolve("app.js")
            val record = PluginCells.scriptRecord(root, core, v3)
            for
              missing <- PluginCells.scriptDigest(root, graph, core, Some(v3)).either
              _       <- Workspace.writeString(script, "one").mapError(error => failed(error.message))
              _       <- Workspace.writeString(record, script.toString).mapError(error => failed(error.message))
              first   <- PluginCells.scriptDigest(root, graph, core, Some(v3))
              _       <- Workspace.writeString(script, "two").mapError(error => failed(error.message))
              second  <- PluginCells.scriptDigest(root, graph, core, Some(v3))
            yield assertTrue(
              missing == Left(failed("link of core 3.10.0 was not written")),
              first != second,
            )
      ,
      test(
        "a compile input names the cell, a missing directory is still a hash, and another platform moves only the star"
      ):
        Workspace
          .work("histos-plugin-inputs")
          .flatMap: root =>
            val module = mod(core, Platform.Jvm, List(Platform.Js), v213, v3)
            val graph  = ProjectGraph.trusted(List(module))
            val asked  = task(
              TaskDep.Same(TaskName.trusted("other")),
              TaskDep.Compile(core, None, None),
              TaskDep.Compile(core, Some(v3), Some(Platform.Jvm)),
            )
            val star = "in:dep:core:*:*"
            val pin  = "in:dep:core:3.10.0:jvm"
            val js   = GraphCompile.classesDir(root, core, v3, Platform.Js)
            for
              same    <- PluginInputs.compileInputs(root, graph, task(TaskDep.Same(TaskName.trusted("other"))))
              before  <- PluginInputs.compileInputs(root, graph, asked)
              _       <- writeBytes(js.resolve("A.class"), Array(1)).mapError(error => failed(error.message))
              after   <- PluginInputs.compileInputs(root, graph, asked)
              missing <- PluginInputs
                .compileInputs(root, graph, task(TaskDep.Compile(ProjectName.trusted("missing"), None, None)))
                .either
              jvmOnly = mod(ProjectName.trusted("web"), Platform.Jvm, Nil, v3)
              linked <- PluginInputs
                .compileInputs(
                  root,
                  ProjectGraph.trusted(List(jvmOnly)),
                  task(TaskDep.Link(ProjectName.trusted("web"), Some(v3))),
                )
                .either
            yield assertTrue(
              same == Nil,
              hashOf(before, star).isDefined,
              hashOf(before, pin).isDefined,
              hashOf(before, star) != hashOf(before, pin),
              hashOf(after, pin) == hashOf(before, pin),
              hashOf(after, star) != hashOf(before, star),
              missing == Left(failed("module missing is not in the graph")),
              linked == Left(failed("'web' does not build the requested cell")),
            )
      ,
      test("a link input is the script digest, and a class file moves that digest"):
        Workspace
          .work("histos-link-input")
          .flatMap: root =>
            val module = mod(core, Platform.Js, Nil, v3)
            val graph  = ProjectGraph.trusted(List(module))
            val script = root.resolve("app.js")
            val record = PluginCells.scriptRecord(root, core, v3)
            val label  = "in:link:core:3.10.0"
            for
              _      <- Workspace.writeString(script, "one").mapError(error => failed(error.message))
              _      <- Workspace.writeString(record, script.toString).mapError(error => failed(error.message))
              first  <- PluginInputs.compileInputs(root, graph, task(TaskDep.Link(core, Some(v3))))
              _      <- Workspace.writeString(script, "two").mapError(error => failed(error.message))
              second <- PluginInputs.compileInputs(root, graph, task(TaskDep.Link(core, Some(v3))))
            yield assertTrue(
              hashOf(first, label).isDefined,
              hashOf(first, label) != hashOf(second, label),
            ),
    ),
    suite("a compiled plugin may not define an engine class")(
      test("the first reserved class is reported, and a demo class is not"):
        Workspace
          .work("histos-engine-class")
          .flatMap: root =>
            val histosTree = root.resolve("histos")
            val demoTree   = root.resolve("demo")
            val scalaTree  = root.resolve("scala")
            val zioTree    = root.resolve("zio")
            val missing    = root.resolve("missing")
            for
              _      <- writeBytes(histosTree.resolve("histos/A.class"), Array(1)).mapError(LoadError.Io(_))
              own    <- ReservedPackage.engineClass(histosTree)
              _      <- writeBytes(demoTree.resolve("demo/B.class"), Array(1)).mapError(LoadError.Io(_))
              other  <- ReservedPackage.engineClass(demoTree)
              absent <- ReservedPackage.engineClass(missing)
              _      <- writeBytes(scalaTree.resolve("scala/Predef.class"), Array(1)).mapError(LoadError.Io(_))
              scala  <- ReservedPackage.engineClass(scalaTree)
              _      <- writeBytes(zioTree.resolve("zio/Task.class"), Array(1)).mapError(LoadError.Io(_))
              zio    <- ReservedPackage.engineClass(zioTree)
            yield assertTrue(
              own.contains("histos.A"),
              other == None,
              absent == None,
              scala.contains("scala.Predef"),
              zio.contains("zio.Task"),
            )
    ),
  )

  private def failed(message: String): PluginRunError =
    PluginRunError.Failed(PluginDetail.trusted(message))

  private def task(deps: TaskDep*): DeclaredTask =
    DeclaredTask(TaskName.trusted("pack"), Seal.Hermetic, Set.empty, Nil, Nil, deps.toList)

  private def mod(
      name: ProjectName,
      platform: Platform,
      more: List[Platform],
      first: ScalaVersion,
      rest: ScalaVersion*
  ): Module =
    Module(
      name,
      histos.Platforms(platform, more),
      histos.ScalaVersions(first, rest.toList),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )

  private def hashOf(inputs: List[histos.schedule.Input], label: String): Option[Sha256] =
    inputs.collectFirst:
      case histos.schedule.Input(name, histos.schedule.InputValue.Hash(sha)) if name.value == label => sha

  private def writeBytes(path: Path, bytes: Array[Byte]): ZIO[Any, IoFailure, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit
end LoaderLawSpec
