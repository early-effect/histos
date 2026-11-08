package histos.bsp

import histos.LawDir
import histos.load.LoadError
import histos.program.ProgramError
import histos.resolve.ResolveError
import histos.testrun.ModuleTestError
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileError, CompileProblem, CompileSeverity, GraphCompileError, PositionBase}
import histos.{Platform, ProjectName, ScalaVersion}
import histos.jdk.ToolchainError

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

object BspLawSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("3.3.4")
  private val api     = ProjectName("api")

  def spec = suite("bsp laws")(
    test("every bsp method round-trips its wire token"):
      val wires = List(
        "build/exit",
        "build/initialize",
        "build/initialized",
        "build/shutdown",
        "workspace/buildTargets",
        "buildTarget/sources",
        "buildTarget/scalacOptions",
        "buildTarget/dependencySources",
        "buildTarget/compile",
        "buildTarget/scalaMainClasses",
        "buildTarget/scalaTestClasses",
        "buildTarget/jvmRunEnvironment",
        "buildTarget/jvmTestEnvironment",
      )
      val bad = Bsp.Method.all.collect { case method if Bsp.Method.from(method.wire) != Some(method) => method.wire }
      assertTrue(Bsp.Method.all.map(_.wire) == wires, bad == Nil, Bsp.Method.from("nope") == None)
    ,
    test("a bsp error code follows the json-rpc bucket"):
      val path    = Path.of("pin")
      val noLock  = ResolveError.NoLock(path)
      val missing = LoadError.MissingFile(path)
      val scope   = Bsp.ScopeFault.Unknown("raw")
      val rows    = List(
        Bsp.BspError.Parse                                   -> -32700,
        Bsp.BspError.NotRequest                              -> -32600,
        Bsp.BspError.NoMethod                                -> -32600,
        Bsp.BspError.ShutDown                                -> -32600,
        Bsp.BspError.UnknownMethod("nope")                   -> -32601,
        Bsp.BspError.Params(Bsp.ParamFault.Missing)          -> -32602,
        Bsp.BspError.Params(Bsp.ParamFault.Rejected)         -> -32602,
        Bsp.BspError.UnknownTarget                           -> -32602,
        Bsp.BspError.Target(Bsp.TargetFault.BadScope(scope)) -> -32602,
        Bsp.BspError.NotTest                                 -> -32602,
        Bsp.BspError.NotMain                                 -> -32602,
        Bsp.BspError.NotInitialized                          -> -32002,
        Bsp.BspError.Machine(ToolchainError.NoArchitecture)  -> -32603,
        Bsp.BspError.Lock(noLock)                            -> -32603,
        Bsp.BspError.LockRead(path)                          -> -32603,
        Bsp.BspError.Resolve(noLock)                         -> -32603,
        Bsp.BspError.Toolchain(CompileError.NoSources)       -> -32603,
        Bsp.BspError.Sources(GraphCompileError.NoRoot(api))  -> -32603,
        Bsp.BspError.Tests(ModuleTestError.NoneMatched(Nil)) -> -32603,
        Bsp.BspError.Run(ProgramError.Absent("api"))         -> -32603,
        Bsp.BspError.Build(missing)                          -> -32603,
        Bsp.BspError.Mains(IoFailure(path, IoFault.Read))    -> -32603,
        Bsp.BspError.Io(IoFailure(path, IoFault.Create))     -> -32603,
      )
      val bad = rows.collect { case (error, code) if error.code != code => error.productPrefix }
      assertTrue(
        bad == Nil,
        Bsp.ParamFault.Missing.message == Rpc.ParamsDecode.Missing.message,
        Bsp.ParamFault.Rejected.message == Rpc.ParamsDecode.Rejected.message,
        Bsp.BspError.Params(Bsp.ParamFault.Missing).message == "bsp params are required",
        Bsp.ScopeFault.Unknown("raw").message == "'raw' is not a bsp scope (main or test)",
        Bsp.BspError.Target(Bsp.TargetFault.BadScope(scope)).message == scope.message,
        Bsp.BspError.UnknownMethod("nope").message == "bsp does not handle nope",
        Bsp.BspError.LockRead(path).message == s"could not read $path",
      )
    ,
    test("a target uri is a build file, a cell, or a typed rejection"):
      val repo     = Path.of("repo").toAbsolutePath.normalize
      val modules  = Set("api")
      val main     = Bsp.mainUri(repo, "api", version, Platform.Jvm)
      val badScala = main.replace(s"scala=${version.value}", "scala=nope")
      val badJvm   = main.replace(s"platform=${Platform.Jvm.token}", "platform=bogus")
      val badScope = main.replace("scope=main", "scope=other")
      val checks   = List(
        "build" -> (Bsp.target(repo, Bsp.buildUri(repo), Set.empty) == Bsp.TargetRef.Build),
        "main"  -> (Bsp.target(repo, main, modules) == Bsp.TargetRef.Main("api", version, Platform.Jvm)),
        "test"  -> (
          Bsp.target(repo, Bsp.testUri(repo, "api", version, Platform.Jvm), modules) ==
            Bsp.TargetRef.Test("api", version, Platform.Jvm)
        ),
        "unknown module" -> (
          Bsp.target(repo, Bsp.mainUri(repo, "nope", version, Platform.Jvm), modules) == Bsp.TargetRef.Unknown
        ),
        "outside" -> (
          Bsp.target(repo, "file:///tmp/elsewhere/api?scala=3.3.4&platform=jvm&scope=main", modules) ==
            Bsp.TargetRef.Unknown
        ),
        "bare"     -> (Bsp.target(repo, main.takeWhile(_ != '?'), modules) == Bsp.TargetRef.Unknown),
        "scala"    -> scalaFault(repo, badScala, modules),
        "platform" -> platformFault(repo, badJvm, modules),
        "scope"    -> (Bsp.target(repo, badScope, modules) == Bsp.TargetRef.BadScope(Bsp.ScopeFault.Unknown("other"))),
      )
      val bad = checks.collect { case (label, ok) if !ok => label }
      assertTrue(bad == Nil)
    ,
    test("target events are created, then changed, then deleted, each sorted"):
      assertTrue(
        Bsp.events(List("d", "a", "b"), List("b", "c")) == List(
          Rpc.BuildTargetEvent(Rpc.TargetId("c"), 1),
          Rpc.BuildTargetEvent(Rpc.TargetId("b"), 2),
          Rpc.BuildTargetEvent(Rpc.TargetId("a"), 3),
          Rpc.BuildTargetEvent(Rpc.TargetId("d"), 3),
        ),
        Bsp.events(List("a"), List("a")) == List(Rpc.BuildTargetEvent(Rpc.TargetId("a"), 2)),
        Bsp.events(Nil, Nil) == Nil,
      )
    ,
    test("compiler problems survive a failed effect"):
      val first  = problem("one")
      val second = problem("two")
      for
        failed <- Bsp.problems { hear =>
          hear(first)
          ZIO.fail("stopped")
        }
        worked <- Bsp.problems { hear =>
          hear(first)
          hear(second)
          ZIO.succeed(1)
        }
      yield assertTrue(failed == (Left("stopped"), Chunk(first)), worked == (Right(1), Chunk(first, second)))
    ,
    suite("directories")(
      test("a file in the way is an io fault and a free path is created"):
        for
          root <- ZIO.service[Path]
          repo <- LawDir.child(root, "bsp")
          blocked = repo.resolve("blocked")
          _             <- ZIO.attemptBlocking(Files.writeString(blocked, "x")).orDie
          blockedResult <- Bsp.ensureDir(blocked).either
          created       <- Bsp.ensureDir(repo.resolve("free").resolve("child"))
          isDir         <- ZIO.attemptBlocking(Files.isDirectory(created)).orDie
        yield assertTrue(
          blockedResult match
            case Left(Bsp.BspError.Io(IoFailure(path, fault))) => path == blocked && fault == IoFault.Create
            case _                                             => false
          ,
          isDir,
        )
    ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer),
  )

  private def problem(message: String): CompileProblem =
    CompileProblem(message, None, CompileSeverity.Error, None, None, None, None, PositionBase.Zinc)

  private def scalaFault(repo: Path, uri: String, modules: Set[String]): Boolean =
    ScalaVersion.from("nope") match
      case Left(error) => Bsp.target(repo, uri, modules) == Bsp.TargetRef.BadVersion(error)
      case Right(_)    => false

  private def platformFault(repo: Path, uri: String, modules: Set[String]): Boolean =
    Platform.from("bogus") match
      case Left(error) => Bsp.target(repo, uri, modules) == Bsp.TargetRef.BadPlatform(error)
      case Right(_)    => false
end BspLawSpec
