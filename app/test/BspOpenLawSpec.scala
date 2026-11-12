package histos.bsp

import histos.{LawDir, LawMachine}
import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.publish.LauncherPin
import histos.resolve.{ResolutionService, Resolve}
import histos.toolchain.{RuntimeIdentity, Runtimes}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileMemo, Compilers}
import histos.ProjectGraph
import slayer.stubbed

import java.nio.file.{Files, Path}

import zio.*
import zio.json.*
import zio.test.*

object BspOpenLawSpec extends ZIOSpecDefault:
  private val jdk = JdkRuntime(
    Path.of("jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("jdk"), Sha256.trusted("ab")),
  )

  def spec = suite("bsp open laws")(
    test("a session stays new until initialize, and exit is 0 only after shutdown"):
      val program =
        for
          root     <- ZIO.service[Path]
          repo     <- LawDir.child(root, "open")
          session  <- open(repo)
          parsed   <- ask(session, "nope")
          array    <- ask(session, "[1]")
          nameless <- ask(session, """{"id":7}""")
          early    <- ask(session, request(7, "workspace/buildTargets"))
          noted    <- ask(session, note("build/initialize"))
          still    <- ask(session, request(8, "workspace/buildTargets"))
          exited   <- ask(session, note("build/exit"))
          after    <- ask(session, request(9, "workspace/buildTargets"))
          _        <- write(repo, "nope\n")
          bad      <- ask(session, request(7, "build/initialize"))
          _        <- write(repo, "histos.version=9.8.7\n")
          init     <- ask(session, request(7, "build/initialize"))
          ready    <- ask(session, note("build/initialized"))
          unknown  <- ask(session, request(3, "nope"))
          held     <- ask(session, note("build/shutdown"))
          stillOn  <- ask(session, request(5, "nope"))
          shutdown <- ask(session, request(4, "build/shutdown"))
          closed   <- ask(session, request(6, "workspace/buildTargets"))
          quiet    <- ask(session, note("workspace/buildTargets"))
          stop     <- ask(session, note("build/exit"))
        yield
          val pin = repo.resolve(LauncherPin.file)
          assertTrue(
            rpcError(parsed) == Right(err(Rpc.RpcId.Absent, -32700, "parse error")),
            rpcError(array) == Right(err(Rpc.RpcId.Absent, -32600, "bsp requires a json-rpc object")),
            rpcError(nameless) == Right(err(Rpc.RpcId.Num(7), -32600, "bsp requires a method")),
            rpcError(early) == Right(err(Rpc.RpcId.Num(7), -32002, "bsp is not initialized")),
            noted == Bsp.Answer.Quiet,
            rpcError(still) == Right(err(Rpc.RpcId.Num(8), -32002, "bsp is not initialized")),
            exited == Bsp.Answer.Stop(1),
            rpcError(after) == Right(err(Rpc.RpcId.Num(9), -32002, "bsp is not initialized")),
            rpcError(bad) == Right(err(Rpc.RpcId.Num(7), -32603, IoFailure(pin, IoFault.Read).message)),
            rpcOk[Rpc.InitializeResult](init) == Right(Rpc.RpcOk("2.0", Rpc.RpcId.Num(7), initialized)),
            ready == Bsp.Answer.Quiet,
            rpcError(unknown) == Right(err(Rpc.RpcId.Num(3), -32601, "bsp does not handle nope")),
            held == Bsp.Answer.Quiet,
            rpcError(stillOn) == Right(err(Rpc.RpcId.Num(5), -32601, "bsp does not handle nope")),
            rpcOk[Rpc.NullResult](shutdown) == Right(Rpc.RpcOk("2.0", Rpc.RpcId.Num(4), Rpc.NullResult())),
            rpcError(closed) == Right(err(Rpc.RpcId.Num(6), -32600, "bsp has shut down")),
            quiet == Bsp.Answer.Quiet,
            stop == Bsp.Answer.Stop(0),
          )
      program.provideSomeLayer[TestEnvironment & Scope & Path](services)
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def services =
    stubbed[Resolve] ++ stubbed[ContentStore] ++ stubbed[Compilers] ++ stubbed[CompileMemo] ++
      stubbed[ResolutionService]

  private val initialized = Rpc.InitializeResult(
    Bsp.ServerName,
    "9.8.7",
    Bsp.BspVersion,
    Rpc.Capabilities(
      Rpc.CompileProvider(List("scala")),
      dependencySourcesProvider = true,
      jvmRunEnvironmentProvider = true,
      jvmTestEnvironmentProvider = true,
      buildTargetChangedProvider = true,
    ),
  )

  private def open(repo: Path): IO[histos.resolve.ResolveError, Bsp.Session] =
    Runtimes.read(LawMachine.idle, None).flatMap(runtimes => Bsp.open(repo, ProjectGraph.trusted(Nil), jdk, runtimes))

  private def ask(session: Bsp.Session, payload: String): ZIO[
    Resolve & ContentStore & Compilers & CompileMemo & ResolutionService,
    Nothing,
    Bsp.Answer,
  ] =
    session.answer(payload)

  private def request(id: Int, method: String): String =
    s"""{"id":$id,"method":"$method"}"""

  private def note(method: String): String =
    s"""{"method":"$method"}"""

  private def write(repo: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking {
      Files.writeString(repo.resolve(LauncherPin.file), text)
      ()
    }.orDie

  private def err(id: Rpc.RpcId, code: Int, message: String): Rpc.RpcErr =
    Rpc.RpcErr("2.0", id, Rpc.ErrBody(code, message))

  private def rpcError(answer: Bsp.Answer): Either[String, Rpc.RpcErr] =
    answer match
      case Bsp.Answer.Reply(line) => line.fromJson[Rpc.RpcErr]
      case other                  => Left(other.toString)

  private def rpcOk[A: JsonCodec](answer: Bsp.Answer): Either[String, Rpc.RpcOk[A]] =
    answer match
      case Bsp.Answer.Reply(line) => line.fromJson[Rpc.RpcOk[A]]
      case other                  => Left(other.toString)
end BspOpenLawSpec
