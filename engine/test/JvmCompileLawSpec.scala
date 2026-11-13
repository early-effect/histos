package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.resolve.{DependencyLock, LockedArtifact, LockedTools, Repositories, Resolve}
import histos.schedule.Digest
import histos.toolchain.RuntimeIdentity
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{Platform, ScalaVersion, ScalacFlags, ToolCatalog}

import java.nio.file.Path

import slayer.{Stubbed, any, stub, stubbed}

import zio.*
import zio.test.*

object JvmCompileLawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion.trusted("3.10.0")
  private val js     = CellPlatform.Js(ToolCatalog.default.js.revision)
  private val jdk    = JdkRuntime(
    Path.of("/jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
  )

  def spec = suite("jvm compile refusals")(
    test("a directory that holds a plugin file is not histos, so empty sources are NoSources"):
      Workspace
        .work("histos-jvm-plugin")
        .flatMap: dir =>
          val plugin  = dir.resolve("plugin")
          val marker  = plugin.resolve("META-INF").resolve("histos").resolve("plugin")
          val program =
            for
              _     <- Workspace.writeString(marker, "entry")
              found <- counted(
                JvmCompile
                  .compileWith(request(scala3, Platform.Js, Nil, Nil, Path.of("checkout")), js, Nil, List(plugin), Nil)
              )
            yield assertTrue(
              found.result == Left(CompileError.NoSources),
              found.idle,
            )
          program.provideSomeLayer[TestEnvironment & Scope](gate)
    ,
    test("javascript with scala 3 and no sources is NoSources"):
      val program =
        counted(JvmCompile.compileWith(request(scala3, Platform.Js, Nil, Nil, Path.of("checkout")), js, Nil, Nil, Nil))
          .map: found =>
            assertTrue(found.result == Left(CompileError.NoSources), found.idle)
      program.provideSomeLayer[TestEnvironment](gate)
    ,
    test("a classpath file that is not a zip fails the stat before the compiler"):
      Workspace
        .work("histos-jvm-stat")
        .flatMap: dir =>
          val source  = dir.resolve("A.scala")
          val plain   = dir.resolve("plain.txt")
          val program =
            for
              _     <- Workspace.writeString(source, "class A")
              _     <- Workspace.writeString(plain, "not a zip")
              found <- counted(
                JvmCompile.compilePooled(request(scala3, Platform.Jvm, List(source), List(plain), dir), Nil, _ => ())
              )
            yield assertTrue(
              found.result == Left(CompileError.Io(IoFailure(plain, IoFault.Stat))),
              found.idle,
            )
          program.provideSomeLayer[TestEnvironment & Scope](gate)
    ,
    test("a missing source fails the read after a clean classpath"):
      Workspace
        .work("histos-jvm-read")
        .flatMap: dir =>
          val source  = dir.resolve("Gone.scala")
          val program =
            counted(JvmCompile.compilePooled(request(scala3, Platform.Jvm, List(source), Nil, dir), Nil, _ => ())).map:
              found =>
                assertTrue(
                  found.result == Left(CompileError.Io(IoFailure(source, IoFault.Read))),
                  found.idle,
                )
          program.provideSomeLayer[TestEnvironment & Scope](gate)
    ,
    test("scala versions besides 3 and 2.13 refuse before a compiler fetch"):
      val versions = List("2.12.20", "2.11.12", "2.14.0", "4.0.0")
      val program  =
        for
          _    <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.die(new IllegalStateException("fetched")))
          rows <- ZIO.foreach(versions): raw =>
            val version = ScalaVersion.trusted(raw)
            for
              jars    <- JvmCompile.scalaJars(version).either
              runtime <- JvmCompile.runtime(version).either
            yield (jars, runtime)
        yield assertTrue(
          rows == versions.map { raw =>
            val error = Left(CompileError.UnsupportedScala(ScalaVersion.trusted(raw)))
            (error, error)
          }
        )
      program.provideSomeLayer[TestEnvironment](gate),
  )

  private val opened =
    stubbed[Resolve] ++ stubbed[ContentStore] ++ stubbed[Compilers] ++ CompilerStarts.live

  private val gate =
    opened >+> (
      ZLayer.fromFunction((resolve: Resolve) =>
        LockedTools(DependencyLock(Nil), ToolCatalog.default, Repositories.central, resolve)
      ) ++
        ZLayer.environment[
          Resolve & Stubbed[Resolve] & ContentStore & Stubbed[ContentStore] & Compilers & Stubbed[Compilers] &
            CompilerStarts
        ]
    )

  private final case class Counted(result: Either[CompileError, CompileOutput], idle: Boolean)

  private type Ready =
    LockedTools & ContentStore & CompilerStarts & Compilers & Resolve & Stubbed[Resolve] & Stubbed[ContentStore] &
      Stubbed[Compilers]

  private def counted(
      effect: ZIO[LockedTools & ContentStore & CompilerStarts & Compilers, CompileError, CompileOutput]
  ): ZIO[Ready, Nothing, Counted] =
    for
      _ <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.die(new IllegalStateException("fetched")))
      _ <- stub[ContentStore](_.read(any[Digest]))(ZIO.die(new IllegalStateException("read")))
      _ <- stub[Compilers](
        _.compile(
          any[CompileRequest],
          any[CompilerArtifacts],
          any[List[String]],
          any[CompileProblem => Unit],
          any[Sha256],
          any[UIO[Unit]],
        )
      )(ZIO.die(new IllegalStateException("compiled")))
      before <- ZIO.serviceWithZIO[CompilerStarts](_.get)
      result <- effect.either
      after  <- ZIO.serviceWithZIO[CompilerStarts](_.get)
    yield Counted(result, before == after)

  private def request(
      version: ScalaVersion,
      platform: Platform,
      sources: List[Path],
      classpath: List[Path],
      checkout: Path,
  ): CompileRequest =
    CompileRequest(
      CompileToolchain(version, platform, jdk, ScalacFlags.empty),
      sources,
      classpath,
      Path.of("classes"),
      Path.of("analysis"),
      Nil,
      Nil,
      checkout = checkout,
    )
end JvmCompileLawSpec
