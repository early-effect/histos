package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.js.{JsKey, ScalaJs}
import histos.pack.MainClass as JsMain
import histos.resolve.{DependencyLock, LockedTools, Repositories, Resolve}
import histos.toolchain.RuntimeIdentity
import histos.{JsModuleKind, JsVersion, Platform, ScalaVersion, ToolCatalog}

import java.nio.file.Path

import slayer.stubbed
import zio.{Exit, UIO, ZIO, ZLayer}
import zio.test.*

object JsLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion.trusted("3.3.7")
  private val scala2  = ScalaVersion.trusted("2.13.14")
  private val linker  = JsVersion.trusted("1.18.2")
  private val newer   = JsVersion.trusted("1.19.0")
  private val main    = JsMain.trusted("demo.Exit")
  private val product = Sha256.trusted("ab")
  private val other   = Sha256.trusted("cd")

  def spec = suite("js laws")(
    test("a link key changes with the scala version, the linker, the module kind, the entry, and the products"):
      val base = JsKey.of(scala3, linker, JsModuleKind.NoModule, main, List(product))
      assertTrue(
        JsModuleKind.values.forall { kind =>
          val key = JsKey.of(scala3, linker, kind, main, List(product))
          JsModuleKind.values.forall(otherKind =>
            (otherKind == kind) == (JsKey.of(scala3, linker, otherKind, main, List(product)) == key)
          )
        },
        JsKey.of(ScalaVersion.trusted("3.4.0"), linker, JsModuleKind.NoModule, main, List(product)) != base,
        JsKey.of(scala3, newer, JsModuleKind.NoModule, main, List(product)) != base,
        JsKey.of(scala3, linker, JsModuleKind.NoModule, main, List(product), initialize = false) != base,
        JsKey.of(scala3, linker, JsModuleKind.NoModule, main, List(other)) != base,
        JsKey.of(scala3, linker, JsModuleKind.NoModule, main, Nil) != base,
      )
    ,
    test("a non-js platform and a non-scala-3 js compile fail before a compiler starts"):
      val others = Platform.values.toList.filter(_ != Platform.Js)
      ZIO
        .foreach(others)(platform => failed(request(platform, scala3)))
        .flatMap: exits =>
          failed(request(Platform.Js, scala2)).map: scala =>
            assertTrue(
              exits.zip(others).forall { (exit, platform) =>
                failedWith(exit, CompileError.UnsupportedPlatform(platform))
              },
              failedWith(scala, CompileError.UnsupportedScala(scala2)),
            ),
  )

  private def failed(request: CompileRequest): ZIO[Any, Nothing, Exit[CompileError, CompileOutput]] =
    ScalaJs
      .compile(request, CellPlatform.Js(ToolCatalog.default.js.revision))
      .exit
      .provide(
        ZLayer.succeed(LockedTools(DependencyLock(Nil), ToolCatalog.default, Repositories.central, dyingResolve)),
        stubbed[ContentStore],
        CompilerStarts.live,
        ZLayer.succeed(dyingCompilers),
      )

  private def failedWith(exit: Exit[CompileError, CompileOutput], error: CompileError): Boolean =
    exit match
      case Exit.Failure(cause) => !cause.isDie && cause.failureOption.contains(error)
      case Exit.Success(_)     => false

  private def request(platform: Platform, version: ScalaVersion): CompileRequest =
    CompileRequest(
      CompileToolchain(
        version,
        platform,
        JdkRuntime(
          Path.of("/jdk"),
          JdkVersion.trusted("21", JdkMajor.trusted(21)),
          RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
        ),
        histos.ScalacFlags.empty,
      ),
      Nil,
      Nil,
      Path.of("classes"),
      Path.of("analysis"),
      Nil,
      Nil,
    )

  private val dyingCompilers: Compilers = new Compilers:
    def compile(
        request: CompileRequest,
        artifacts: CompilerArtifacts,
        extra: List[String],
        hear: CompileProblem => Unit,
        digest: Sha256,
        admitted: UIO[Unit],
    ): zio.IO[CompileError, CompileOutput] =
      val touched = (artifacts, extra, hear, digest, admitted)
      ZIO.die(new IllegalStateException(s"compiled ${request.toolchain.platform} ${touched.hashCode}"))

    def openings: UIO[Int] = ZIO.die(new IllegalStateException("openings"))

    def drop: UIO[Unit] = ZIO.die(new IllegalStateException("drop"))

  private val dyingResolve: Resolve = new Resolve:
    def resolveTool(request: histos.resolve.ToolRequest) =
      ZIO.die(new IllegalStateException(s"resolveTool ${request.id}"))

    def resolve(cell: histos.resolve.ResolveCell) =
      ZIO.die(new IllegalStateException(s"resolve ${cell.module}"))

    def fetch(artifact: histos.resolve.LockedArtifact) =
      ZIO.die(new IllegalStateException(s"fetch ${artifact.name}"))

    def resolvePlugin(dependency: histos.Dep, repositories: Repositories) =
      ZIO.die(new IllegalStateException(s"plugin ${dependency.name}"))

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: histos.resolve.Repository,
    ) =
      ZIO.die(new IllegalStateException(s"sources $organization $name $revision $repository"))
end JsLawSpec
