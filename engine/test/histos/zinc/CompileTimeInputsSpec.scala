package histos.zinc

import histos.*
import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256Digest}
import histos.load.{PluginCompileError, PluginCompileSupport}
import histos.native.NativeOrigin
import histos.resolve.{LockedTools, Resolve, ResolutionService}
import histos.schedule.RemoteCache
import histos.toolchain.RuntimeIdentity
import histos.workspace.Workspace

import java.nio.file.Path

import zio.{Chunk, ZIO, ZLayer, durationInt}
import zio.test.*

object CompileTimeInputsSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("3.10.1")

  private val tests = suite("compile-time implementation inputs")(
    test("an ordinary macro helper body invalidates unchanged downstream typeCheckErrors") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-macro-helper-input")
            _    <- ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")) { key =>
              ZIO.foreachDiscard(sys.props.get(key))(TestSystem.putProperty(key, _))
            }
            upstream   = module("validator", Jvm)(version)
            downstream = module("consumer", Jvm)(version).dependsOn(upstream)
            graph <- ZIO.fromEither(Projects(upstream, downstream))
            helper = repo.resolve("validator/src/lib/Validator.scala")
            _          <- Workspace.writeString(helper, validator("old"))
            _          <- Workspace.writeString(repo.resolve("consumer/src/demo/Use.scala"), consumer)
            cas        <- Workspace.createDirectories(repo.resolve("cas"))
            resolution <- ResolutionService.open(repo, graph)
            lock       <- resolution.current
            tools      <- LockedTools.make(lock, graph, resolution)
            home       = Path.of(sys.props.getOrElse("java.home", "."))
            jdkVersion = JdkVersion.trusted("25", JdkMajor.trusted(25))
            jdk        = JdkRuntime(
              home,
              jdkVersion,
              RuntimeIdentity.Local(home, Sha256Digest.of(Chunk.fromArray(jdkVersion.value.getBytes))),
            )
            session = CompileSession(repo, graph, jdk, NativeOrigin.Central, lock)
            resident <- (Compilers.live ++ CompileMemo.live).build
            compiled = GraphCompile
              .strict(GraphCompile.held(session, List(ProjectName("consumer")), _ => ZIO.unit))
              .runCollect
              .map(_.collect { case CompileFeed.Done(cell) => cell }.toList)
              .provideSome[Resolve](
                ZLayer.succeed(resolution),
                ZLayer.succeed(ContentStore.in(cas)),
                RemoteCache.off,
                ZLayer.succeed(resident.get[Compilers]),
                ZLayer.succeed(resident.get[CompileMemo]),
              )
            first   <- compiled
            library <- JvmCompile.runtime(version).provide(ZLayer.succeed(tools))
            classes = GraphCompile.classesDir(repo, ProjectName("consumer"), version, Platform.Jvm)
            before <- PluginCompileSupport.invoke(classes, library, "demo.Use$", "text")
            analysis = GraphCompile.analysisDirOf(
              GraphCompile.classesDir(repo, ProjectName("validator"), version, Platform.Jvm)
            )
            recorded  <- ZIO.fromOption(analysis).orElseFail(PluginCompileError.Linked("missing analysis"))
            apiBefore <- ZIO.fromEither(ApiDigest.read(recorded))
            _         <- Workspace.writeString(helper, validator("new"))
            second    <- compiled
            apiAfter  <- ZIO.fromEither(ApiDigest.read(recorded))
            after     <- PluginCompileSupport.invoke(classes, library, "demo.Use$", "text")
            third     <- compiled
          yield assertTrue(
            first.exists(_.module == ProjectName("consumer")),
            before == "false",
            apiBefore == apiAfter,
            after == "true",
            second.exists(cell => cell.module == ProjectName("consumer") && cell.sources.exists(_.compiled == 1)),
            third.exists(_.module == ProjectName("consumer")),
            third.filter(_.module == ProjectName("consumer")).forall(!_.changed),
          )
        }
        .provide(Resolve.coursier)
    },
    test("Main compilation accepts a scoped module diamond without inspecting broken Test code") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-main-scoped-module-diamond")
            _    <- ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")) { key =>
              ZIO.foreachDiscard(sys.props.get(key))(TestSystem.putProperty(key, _))
            }
            base       = module("a", Jvm)(version)
            dependency = module("b", Jvm)(version).testDependsOn(base)
            graph <- ZIO.fromEither(Projects(base.dependsOn(dependency), dependency))
            _     <- Workspace
              .writeString(repo.resolve("a/src/demo/A.scala"), "package demo\nobject A:\n  val value: Int = B.value\n")
            _ <- Workspace.writeString(repo.resolve("b/src/demo/B.scala"), "package demo\nobject B:\n  val value = 1\n")
            _ <- Workspace.writeString(
              repo.resolve("a/test/demo/Broken.scala"),
              "package demo\nobject Broken:\n  val value: Int = \"broken\"\n",
            )
            cas        <- Workspace.createDirectories(repo.resolve("cas"))
            resolution <- ResolutionService.open(repo, graph)
            lock       <- resolution.current
            home  = Path.of(sys.props.getOrElse("java.home", "."))
            floor = JdkVersion.trusted("25", JdkMajor.trusted(25))
            jdk   = JdkRuntime(
              home,
              floor,
              RuntimeIdentity.Local(home, Sha256Digest.of(Chunk.fromArray(floor.value.getBytes))),
            )
            result <- GraphCompile
              .run(
                CompileSession(repo, graph, jdk, NativeOrigin.Central, lock),
                List(ProjectName("a")),
                Some(Platform.Jvm),
              )
              .provideSome[Resolve](ZLayer.succeed(resolution), ZLayer.succeed(ContentStore.in(cas)), RemoteCache.off)
          yield assertTrue(
            result.map(_.module).toSet == Set(ProjectName("a"), ProjectName("b")),
            result.forall(_.classFiles > 0),
          )
        }
        .provide(Resolve.coursier)
    },
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private def validator(accepted: String): String =
    s"""|package lib
        |import scala.quoted.*
        |object Validator:
        |  def accepts(raw: String): Boolean = raw == "$accepted"
        |object Literal:
        |  inline def apply(inline raw: String): String = $${literal('raw)}
        |  def literal(raw: Expr[String])(using Quotes): Expr[String] =
        |    raw.value match
        |      case Some(value) if Validator.accepts(value) => Expr(value)
        |      case _ => quotes.reflect.report.errorAndAbort("refused")
        |""".stripMargin

  private val consumer =
    """|package demo
       |object Use:
       |  private val accepted = scala.compiletime.testing.typeCheckErrors("lib.Literal(\"new\")").isEmpty
       |  def text: String = accepted.toString
       |""".stripMargin
end CompileTimeInputsSpec
