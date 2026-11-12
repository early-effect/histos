package histos.bsp

import histos.cache.ContentStore
import histos.load.{BuildNotes, LoadError}
import histos.publish.LauncherPin
import histos.resolve.{DependencyLock, ResolutionService, Resolve, ResolveError}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, CompileProblem, CompileSeverity, Compilers, PositionBase}
import histos.{
  LawMachine,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}
import slayer.stubbed

import java.nio.file.Path

import zio.json.*
import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** The build target compiles the build effect. A failed note and a load error are status 2. Nothing compiles a module,
  * so every compile service is a stub that dies if asked.
  */
object BspRemainLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("remainlaw")

  def spec = suite("bsp build target laws")(
    test("a quiet build is status 1, a failed note and a load error are status 2"):
      Workspace
        .work("histos-bsp-remain")
        .flatMap: dir =>
          val uri     = Bsp.buildUri(dir)
          val problem = CompileProblem(
            "build law",
            None,
            CompileSeverity.Error,
            None,
            None,
            None,
            None,
            PositionBase.Dotty,
          )
          val program =
            for
              machine  <- LawMachine.at(dir)
              _        <- Workspace.writeString(dir.resolve(LauncherPin.file), "histos.version=9.8.7\n")
              runtimes <- Runtimes.read(machine, None)
              quiet    <- compileBuild(dir, runtimes, uri, ZIO.succeed(BuildNotes(false, Nil)))
              failed   <- compileBuild(dir, runtimes, uri, ZIO.succeed(BuildNotes(true, List(problem))))
              missing  <- compileBuild(dir, runtimes, uri, ZIO.fail(LoadError.NoEntry))
            yield assertTrue(
              statusOf(quiet.answer) == Right(1),
              statusOf(failed.answer) == Right(2),
              failed.notes.exists(_.contains("build law")),
              statusOf(missing.answer) == Right(2),
              missing.notes.exists(_.contains(LoadError.NoEntry.message)),
            )
          program.provideSomeLayer[Scope](sessionLayer(dir))
  )

  private final case class Heard(answer: Bsp.Answer, notes: Chunk[String])

  private def compileBuild(
      dir: Path,
      runtimes: Runtimes,
      uri: String,
      build: ZIO[Any, LoadError, BuildNotes],
  ): ZIO[
    Resolve & ContentStore & histos.zinc.Compilers & histos.zinc.CompileMemo & ResolutionService,
    ResolveError,
    Heard,
  ] =
    for
      notes   <- Ref.make(Chunk.empty[String])
      session <- Bsp.open(
        dir,
        graph,
        LawMachine.fixtureJdk,
        runtimes,
        Nil,
        line => notes.update(_ :+ line),
        build,
        compile => compile,
        None,
      )
      _      <- session.answer("""{"id":1,"method":"build/initialize"}""")
      _      <- session.answer("""{"method":"build/initialized"}""")
      answer <- session.answer(asked(2, List(uri)))
      heard  <- notes.get
    yield Heard(answer, heard)

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion("3.10.0"), Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    ProjectGraph.trusted(List(law), Splits.aggregate, Catalog.empty, None, Nil, resolution = Resolution.Lock)

  private def sessionLayer(dir: Path) =
    stubbed[Resolve] >+>
      (ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++ stubbed[Compilers] ++ stubbed[CompileMemo] ++
        ResolutionService.pinned(DependencyLock(Nil)))

  private def asked(id: Int, uris: List[String]): String =
    val body = Asked(uris.map(Rpc.TargetId(_))).toJson
    s"""{"id":$id,"method":"buildTarget/compile","params":$body}"""

  private def statusOf(answer: Bsp.Answer): Either[String, Int] =
    answer match
      case Bsp.Answer.Reply(line) =>
        line.fromJson[Rpc.RpcOk[Rpc.CompileResult]].map(_.result.statusCode)
      case other => Left(other.toString)

  private final case class Asked(targets: List[Rpc.TargetId]) derives JsonEncoder
end BspRemainLawSpec
