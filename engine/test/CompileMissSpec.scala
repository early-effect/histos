package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.native.NativeOrigin
import histos.workspace.Workspace
import histos.{Jvm, Platform, ProjectName, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object CompileMissSpec extends histos.SuiteClock:
  private val scala3 = ScalaVersion("3.9.0")
  private val v21    = JdkVersion.trusted("21.0.2", JdkMajor.trusted(21))
  private val salt   = Sha256.of(Chunk.fromArray("salt".getBytes))

  private val named = suite("named inputs")(
    test("the line is no prior key, or the input whose value changed"):
      val request = sample(ScalacFlags.empty)
      val one     = Sha256.of(Chunk.fromArray("one".getBytes))
      val two     = Sha256.of(Chunk.fromArray("two".getBytes))
      val left    = CompileInputs.declared(request, one)
      val right   = CompileInputs.declared(request, two)
      val jars    = CompileInputs.withCompiler(left, List(one))
      val moved   = CompileInputs.withCompiler(left, List(two))
      val core    = ProjectName("core")
      assertTrue(
        CompileKey.of(request, one) == Sha256.of(CompileInputs.encode(left)),
        CompileKey.of(request, one) != CompileKey.of(request, two),
        CompileInputs.diff(left, right) == List(CompileInput.Sources),
        CompileInputs.diff(jars, moved) == List(CompileInput.Compiler),
        CompileKey.withTools(CompileKey.of(request, one), List(one)) !=
          CompileKey.withTools(CompileKey.of(request, one), List(two)),
        CompileInputs.decode(CompileInputs.encode(left)) == Some(left),
        CompileInputs.decode(Chunk.fromArray("nope".getBytes)) == None,
        KeyMiss.line(core, scala3, Platform.Jvm, KeyMiss.NoPrior) == "core 3.9.0 jvm no prior key",
        KeyMiss.line(core, scala3, Platform.Jvm, KeyMiss.Changed(List(CompileInput.Sources))) ==
          "core 3.9.0 jvm sources",
      )
  )

  private val live = suite("a compile miss")(
    test("the first compile says no prior key and a comment names sources"):
      ZIO.scoped:
        for
          jdk   <- GraphCompile.jdkHere
          root  <- temp("histos-miss")
          cas   <- temp("histos-miss-cas")
          count <- Ref.make(0)
          store = ContentStore.in(cas)
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
          _     <- ZIO.attempt {
            write(root, "histos.scala", buildFile)
            write(root, "core/src/main/scala/Core.scala", "object Core:\n  def n: Int = 1\n")
            write(root, ".histos/dependencies.lock", "cell core 3.9.0 jvm\n")
          }
          env = held(store, count)
          first  <- cells(root, built, jdk, env)
          opened <- count.get
          _      <- ZIO.attempt {
            write(root, "core/src/main/scala/Core.scala", "object Core:\n  def n: Int = 1 // note\n")
          }
          second <- cells(root, built, jdk, env)
          edited <- count.get
          third  <- cells(root, built, jdk, env)
          again  <- count.get
        yield assertTrue(
          changes(first) == List(KeyMiss.NoPrior),
          spelled(first) == List("core 3.9.0 jvm no prior key"),
          missBeforeDone(first),
          opened == 1,
          changes(second) == List(KeyMiss.Changed(List(CompileInput.Sources))),
          spelled(second) == List("core 3.9.0 jvm sources"),
          missBeforeDone(second),
          edited == 2,
          changes(third).isEmpty,
          again == 2,
        )
  )

  private def sample(flags: ScalacFlags): CompileRequest =
    CompileRequest(
      CompileToolchain(
        scala3,
        Platform.Jvm,
        JdkRuntime(
          Path.of("/jdks").resolve(v21.value),
          v21,
          histos.toolchain.RuntimeIdentity.Local(Path.of("/jdks").resolve(v21.value), salt),
        ),
        flags,
      ),
      Nil,
      Nil,
      Path.of("/out"),
      Path.of("/analysis"),
      Nil,
      Nil,
    )

  private def held(store: ContentStore, count: Ref[Int]) =
    ZLayer.fromZIO(ZIO.serviceWith[histos.resolve.LockedTools](_.resolve)) ++
      ZLayer.succeed(store) ++
      Compiler.counted(count)

  private def cells(
      root: Path,
      built: histos.ProjectGraph,
      jdk: histos.jdk.JdkRuntime,
      env: ZLayer[histos.resolve.LockedTools, Nothing, histos.resolve.Resolve & ContentStore & Compiler],
  ) =
    histos.ToolFixture
      .streamAt(root, built)(
        GraphCompile.consoleCounted(root, built, Nil, jdk, NativeOrigin.Central, None)
      )
      .runCollect
      .provideSomeLayer[histos.resolve.LockedTools](env)

  private def changes(feeds: Chunk[CompileFeed]): List[KeyMiss] =
    feeds.collect { case CompileFeed.Miss(_, _, _, change) => change }.toList

  private def spelled(feeds: Chunk[CompileFeed]): List[String] =
    feeds
      .collect:
        case CompileFeed.Miss(module, version, platform, change) =>
          KeyMiss.line(module, version, platform, change)
      .toList

  private def missBeforeDone(feeds: Chunk[CompileFeed]): Boolean =
    val miss = feeds.indexWhere:
      case CompileFeed.Miss(_, _, _, _) => true
      case _                            => false
    val done = feeds.indexWhere:
      case CompileFeed.Done(_) => true
      case _                   => false
    miss >= 0 && done > miss

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp(prefix: String): ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory(prefix)).orDie)(dir => Workspace.delete(dir).ignore)

  private val buildFile =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph = Projects(module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  def spec = suite("miss names the input")(
    named,
    live.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.jvm(scala3)
    ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes),
  )
end CompileMissSpec
