package histos.bsp

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.load.{BuildNotes, LoadError}
import histos.publish.LauncherPin
import histos.resolve.{DependencyLock, ResolutionService, Resolve, ResolveError}
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, CompileProblem, CompileSeverity, Compilers, GraphCompile, PositionBase}
import histos.{Module, Platform, Platforms, ProjectGraph, ProjectName, Resolution, ScalaVersion, ScalaVersions, SourcePos, Splits}

import java.nio.file.{Files, Path}

import zio.json.*
import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** The build target compiles the build effect. A failed note and a load error are status 2. */
object BspRemainLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("remainlaw")

  def spec = suite("bsp build target laws")(
    test("a quiet build is status 1, a failed note and a load error are status 2"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace.work("histos-bsp-remain").flatMap: dir =>
          val pin = dir.resolve(LauncherPin.file)
          val uri = Bsp.buildUri(dir)
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
              loaded <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              found <- GraphCompile.jdk(machine).either
              jdk <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              _ <- Workspace.writeString(pin, "histos.version=9.8.7\n")
              runtimes <- Runtimes.read(machine, None)
              quiet <- compileBuild(dir, jdk, runtimes, uri, ZIO.succeed(BuildNotes(false, Nil)))
              failed <- compileBuild(dir, jdk, runtimes, uri, ZIO.succeed(BuildNotes(true, List(problem))))
              missing <- compileBuild(dir, jdk, runtimes, uri, ZIO.fail(LoadError.NoEntry))
            yield
              val quietStatus = statusOf(quiet.answer)
              val failedStatus = statusOf(failed.answer)
              val missingStatus = statusOf(missing.answer)
              assertTrue(
                quietStatus == Right(1),
                failedStatus == Right(2),
                failed.notes.exists(_.contains("build law")),
                missingStatus == Right(2),
                missing.notes.exists(_.contains(LoadError.NoEntry.message)),
              )
          program.provideSomeLayer[Resolve & Scope](sessionLayer(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private final case class Heard(answer: Bsp.Answer, notes: Chunk[String])

  private def compileBuild(
      dir: Path,
      jdk: histos.jdk.JdkRuntime,
      runtimes: Runtimes,
      uri: String,
      build: ZIO[Any, LoadError, BuildNotes],
  ): ZIO[Resolve & ContentStore & histos.zinc.Compilers & histos.zinc.CompileMemo & ResolutionService, ResolveError, Heard] =
    for
      notes <- Ref.make(Chunk.empty[String])
      session <- Bsp.open(dir, graph, jdk, runtimes, notify = line => notes.update(_ :+ line), build = build)
      _ <- ask(session, """{"id":1,"method":"build/initialize"}""")
      _ <- ask(session, """{"method":"build/initialized"}""")
      answer <- ask(session, asked(2, List(uri)))
      heard <- notes.get
    yield Heard(answer, heard)

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def sessionLayer(dir: Path, lock: DependencyLock) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      Compilers.live(1) ++
      CompileMemo.live ++
      ResolutionService.pinned(lock)

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(Resolve.readLock(repository))

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)

  private def ask(session: Bsp.Session, payload: String) =
    session.answer(payload)

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
