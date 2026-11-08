package histos.load

import histos.jdk.{JdkRuntime, JdkVersion, Sha256, Sha256Digest}
import histos.plugin.{DeclaredTask, PluginDetail, PluginRunError, Seal, TaskDep, TaskName}
import histos.resolve.{DependencyLock, ResolutionService, Resolve}
import histos.toolchain.RuntimeIdentity
import histos.workspace.Workspace
import histos.native.NativeOrigin
import histos.zinc.CompileSession
import histos.{
  MainClass,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import java.nio.file.{Files, Path}

import slayer.stubbed

import zio.{Chunk, ZIO}
import zio.test.*

object PluginCellLawSpec extends ZIOSpecDefault:
  private val core = ProjectName.trusted("core")
  private val v3   = ScalaVersion.trusted("3.10.0")

  def spec = suite("plugin cell laws")(
    test("a same-task dependency does not compile, and a missing module fails before the lock"):
      val graph   = ProjectGraph.trusted(List(jvm))
      val same    = task(TaskDep.Same(TaskName.trusted("other")))
      val gone    = task(TaskDep.Compile(ProjectName.trusted("missing"), None, None))
      val program =
        for
          idle    <- PluginCells.classpath(Path.of("repo"), graph, same, ZIO.dieMessage("lock"))
          missing <- PluginCells.classpath(Path.of("repo"), graph, gone, ZIO.dieMessage("lock")).either
        yield assertTrue(
          idle == Nil,
          missing == Left(PluginFault.Task(failed("module missing is not in the graph"))),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Resolve])
    ,
    test("a jvm cell is not a script, and a bad main is refused as a main"):
      val graph   = ProjectGraph.trusted(List(jvm))
      val named   = jvm.copy(application = Some(MainClass.trusted("demo.Main")))
      val bad     = jvm.copy(application = Some(MainClass.trusted("not a class")))
      val program =
        for
          version <- ZIO.fromEither(JdkVersion.from("21")).catchAll(error => ZIO.dieMessage(error.message))
          runtime = JdkRuntime(
            Path.of("jdk"),
            version,
            RuntimeIdentity.Local(Path.of("jdk"), Sha256Digest.of(Chunk.empty)),
          )
          session = CompileSession(Path.of("repo"), graph, runtime, NativeOrigin.Central, DependencyLock(Nil))
          plain <- PluginCells.link(session, PluginCells.Cell(jvm, v3, Platform.Jvm)).either
          main  <- PluginCells
            .link(session.copy(graph = ProjectGraph.trusted(List(named))), PluginCells.Cell(named, v3, Platform.Jvm))
            .either
          junk <- PluginCells
            .link(session.copy(graph = ProjectGraph.trusted(List(bad))), PluginCells.Cell(bad, v3, Platform.Jvm))
            .either
        yield assertTrue(
          plain == Left(PluginFault.Task(failed("'core' has no main class to link"))),
          main == Left(PluginFault.Task(failed("'core' is not a JavaScript module"))),
          junk == Left(PluginFault.Task(failed(histos.MainClassError.Bad("not a class").message))),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Resolve] ++ stubbed[ResolutionService])
    ,
    test("a missing script record is unread, and the record's text is the script path"):
      Workspace
        .work("histos-cell-script")
        .flatMap: root =>
          val graph  = ProjectGraph.trusted(List(js))
          val record = PluginCells.scriptRecord(root, core, v3)
          val asked  = task(TaskDep.Link(core, Some(v3)))
          for
            missing <- PluginCells.scripts(root, graph, asked).either
            _     <- Workspace.writeString(record, "  /opt/app.js \n").catchAll(error => ZIO.dieMessage(error.message))
            found <- PluginCells.scripts(root, graph, asked)
            none  <- PluginCells.scripts(root, graph, task())
            lost  <- PluginCells.scripts(root, graph, task(TaskDep.Link(ProjectName.trusted("missing"), None))).either
          yield
            val linked = Files.isRegularFile(record)
            assertTrue(
              missing == Left(failed("link of core 3.10.0 was not written")),
              found == List(histos.plugin.LinkedScript(core, v3, Path.of("/opt/app.js"))),
              none == Nil,
              lost == Left(failed("module missing is not in the graph")),
              linked,
            ),
  )

  private val jvm: Module =
    Module(core, Platforms(Platform.Jvm, Nil), ScalaVersions(v3, Nil), Nil, Nil, SourcePos("law.scala", 1, 1))

  private val js: Module =
    Module(core, Platforms(Platform.Js, Nil), ScalaVersions(v3, Nil), Nil, Nil, SourcePos("law.scala", 1, 1))

  private def task(deps: TaskDep*): DeclaredTask =
    DeclaredTask(TaskName.trusted("pack"), Seal.Hermetic, Set.empty, Nil, Nil, deps.toList)

  private def failed(message: String): PluginRunError =
    PluginRunError.Failed(PluginDetail.trusted(message))
end PluginCellLawSpec
