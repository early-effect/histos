package histos.server

import histos.assembly.AssembleError
import histos.bsp.Frame
import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.load.{BuildNotes, LoadError, LoadedBuild, Loader}
import histos.program.ProgramError
import histos.publish.IvyRoot
import histos.report.{BuildFault, CallResult}
import histos.resolve.{DependencyLock, Resolve}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  HistosBuild,
  LawMachine,
  LawPlugins,
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

import java.net.UnixDomainSocketAddress
import java.nio.channels.{Channels, SocketChannel}
import java.nio.file.{Files, Path}

import zio.json.*
import zio.{Chunk, IO, Promise, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.{TestAspect, TestResult, TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** A server on its own socket answers version and cache, an empty clean removes products, reload of a missing build is
  * a fault, and a client that leaves ends its call. The checkout, the socket's home, the store, and the ivy root are
  * the law's own directory under `/tmp`, where a socket path fits. Nothing compiles.
  */
object DaemonOpenLawSpec extends ZIOSpecDefault:
  private val version = "0.0.0-law"
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("daemonlaw")

  def spec = suite("daemon open laws")(
    test("version is the server's version, cache names the store, and clean removes the product"):
      served(identity, Loader.live): repo =>
        for
          running <- stand(repo, _ => ZIO.unit)
          socket = running.record.socket
          versionFeeds <- Client.talk(socket, Call.Version).runCollect
          cacheFeeds   <- Client.talk(socket, Call.CachePath).runCollect
          clazz = repo.resolve(".histos").resolve("classes").resolve(lawName.value).resolve("Kept.class")
          _          <- Workspace.writeString(clazz, "kept\n")
          cleanFeeds <- Client.talk(socket, Call.Clean(Nil)).runCollect
          kept       <- ZIO.attempt(Files.exists(clazz)).orDie
          missed     <- Client.talk(socket, Call.PreviewCompile(List(lawName))).runCollect
          assembled  <- Client.talk(socket, Call.Assembly(None)).runCollect
          ran        <- Client.talk(socket, Call.Run(lawName.value, None)).runCollect
          rebooted   <- Client.talk(socket, Call.Reboot).runCollect
          stopped    <- running.done.either
          closed     <- ZIO.attempt(Files.exists(socket)).orDie
        yield assertTrue(
          outs(versionFeeds) == s"$version\nserver $version\nno local assembly",
          done(versionFeeds).contains(CallResult.reported()),
          outs(cacheFeeds) == repo.resolve("store").toString,
          outs(cleanFeeds) == "cleaned",
          !kept,
          outs(missed).contains("daemonlaw 3.10.0 jvm selected"),
          done(assembled) == Some(CallResult.Fault(BuildFault.Assemble(AssembleError.NoModule("app")))),
          done(ran) == Some(CallResult.Fault(BuildFault.Program(ProgramError.NoMain(lawName)))),
          done(rebooted).contains(CallResult.reported()),
          stopped == Right(()),
          !closed,
        )
    ,
    test("reload of a repo with no build file is a fault, and version still answers"):
      served(identity, Loader.live): repo =>
        for
          running  <- stand(repo, _ => ZIO.unit)
          reloaded <- Client.talk(running.record.socket, Call.Reload).runCollect
          answered <- Client.talk(running.record.socket, Call.Version).runCollect
          _        <- Client.talk(running.record.socket, Call.Reboot).runCollect
          _        <- running.done
        yield assertTrue(
          done(reloaded) == Some(
            CallResult.Fault(BuildFault.Load(LoadError.MissingFile(repo.resolve("histos.scala"))))
          ),
          outs(answered) == s"$version\nserver $version\nno local assembly",
        )
    ,
    test("reboot reaches the client before the successor runs"):
      served(identity, Loader.live): repo =>
        for
          entered <- Promise.make[Nothing, Unit]
          hold    <- Promise.make[Nothing, Unit]
          _       <- ZIO.addFinalizer(hold.succeed(()))
          running <- stand(repo, _ => entered.succeed(()) *> hold.await)
          heard   <- Client.talk(running.record.socket, Call.Reboot).runCollect
          _       <- entered.await
          _       <- hold.succeed(())
          ended   <- running.done.either
        yield assertTrue(done(heard).contains(CallResult.reported()), ended == Right(()))
    ,
    test("a checkout deeper than a Unix socket path holds still binds, answers, and removes its socket"):
      served(_.resolve(("d" * 60 + "/") * 4 + "repo"), Loader.live): repo =>
        for
          running <- stand(repo, _ => ZIO.unit)
          socket = running.record.socket
          heard   <- Client.talk(socket, Call.Version).runCollect
          _       <- Client.talk(socket, Call.Reboot).runCollect
          stopped <- running.done.either
          closed  <- ZIO.attempt(Files.exists(socket)).orDie
        yield assertTrue(
          repo.toString.length > 250,
          socket.toString.length < 104,
          outs(heard).startsWith(version),
          stopped == Right(()),
          !closed,
        )
    ,
    test("a client that closes its socket ends its call, and the next call is not held behind it"):
      for
        entered     <- Promise.make[Nothing, Unit]
        interrupted <- Promise.make[Nothing, Unit]
        loads       <- Ref.make(0)
        result      <- served(identity, ZLayer.succeed(hanging(loads, entered, interrupted))): repo =>
          val build = repo.resolve("histos.scala")
          for
            _       <- Workspace.writeString(build, "// first\n")
            running <- stand(repo, _ => ZIO.unit)
            socket = running.record.socket
            _     <- Workspace.writeString(build, "// second\n")
            _     <- leave(socket, Call.Graph, entered)
            _     <- interrupted.await
            graph <- Client.talk(socket, Call.Graph).runCollect
            _     <- Client.talk(socket, Call.Reboot).runCollect
            _     <- running.done
            asked <- loads.get
          yield assertTrue(
            outs(graph) == "daemonlaw jvm 3.10.0",
            done(graph).contains(CallResult.reported()),
            asked == 2,
          )
      yield result
    @@ TestAspect.timeout(30.seconds),
  ) @@ TestAspect.timeout(120.seconds)

  /** Send `call`, hear that the server is ready, wait until the call is running, then close the socket. */
  private def leave(socket: Path, call: Call, running: Promise[Nothing, Unit]): IO[Frame.FrameError, Unit] =
    ZIO.scoped:
      for
        channel <- ZIO.acquireRelease(
          ZIO.attemptBlocking(SocketChannel.open(UnixDomainSocketAddress.of(socket))).orDie
        )(channel => ZIO.attemptBlocking(channel.close()).ignore)
        _ <- Frame.write(Channels.newOutputStream(channel), call.toJson)
        _ <- Frame.read(Channels.newInputStream(channel))
        _ <- running.await
      yield ()

  /** The first load runs until it is interrupted. Every later load is the law's graph. */
  private def hanging(loads: Ref[Int], entered: Promise[Nothing, Unit], interrupted: Promise[Nothing, Unit]): Loader =
    new Loader:
      def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild] =
        loads
          .updateAndGet(_ + 1)
          .flatMap:
            case 1 => entered.succeed(()) *> ZIO.never.onInterrupt(interrupted.succeed(()))
            case _ => ZIO.succeed(LoadedBuild(graph, LawPlugins.none, HistosBuild.fixed(graph)))
      def loadLocked(repo: Path, apiJar: Path, lock: DependencyLock): ZIO[Scope, LoadError, LoadedBuild] =
        ZIO.dieMessage("the law never loads a locked build")
      def buildClasspath(apiJar: Path): IO[LoadError, List[Path]] =
        ZIO.dieMessage("the law never asks for the build classpath")
      def reportBuild(repo: Path, apiJar: Path): IO[LoadError, BuildNotes] =
        ZIO.dieMessage("the law never reports the build")

  /** A directory under `/tmp`, removed after the law, holds the checkout `place` names, the socket's home, an empty
    * Coursier cache that `Resolve` reads offline, the store, and the ivy root. The machine is [[LawMachine]]'s.
    */
  private def served[E](place: Path => Path, loader: ZLayer[Resolve, Nothing, Loader])(
      law: Path => ZIO[Scope & Daemon.Needs, E, TestResult]
  ): ZIO[Scope, E, TestResult] =
    for
      temp <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory(Path.of("/tmp"), "hd")).orDie)(path =>
        Workspace.delete(path).ignore
      )
      repo = place(temp)
      _      <- ZIO.attemptBlocking(Files.createDirectories(repo)).orDie
      _      <- TestSystem.putEnv("HISTOS_HOME", temp.resolve("home").toString)
      _      <- LawMachine.system(temp)
      result <- law(repo).provideSomeLayer[Scope](
        Resolve.offline(temp.resolve("coursier")) >+>
          (loader ++ ZLayer.succeed(ContentStore.in(repo.resolve("store"))) ++
            ZLayer.succeed(IvyRoot.at(temp.resolve("ivy"))))
      )
    yield result

  private def stand(
      repo: Path,
      relaunch: Path => IO[ServerError, Unit],
  ): ZIO[Scope & Daemon.Needs, ServerError, Daemon.Running] =
    for
      machine <- LawMachine.at(repo)
      api = repo.resolve("api.jar")
      _       <- ZIO.attemptBlocking(Files.write(api, Array.emptyByteArray)).orDie
      running <- Daemon.open(
        repo,
        LoadedBuild(graph, LawPlugins.none, HistosBuild.fixed(graph)),
        LawMachine.fixtureJdk,
        version,
        api,
        Nil,
        ZIO.dieMessage("the law never reports the build"),
        machine,
        relaunch,
        None,
      )
    yield running

  private def outs(heard: Chunk[Feed]): String =
    heard.collect { case Feed.Event(BuildEvent.Out(text)) => text }.mkString("\n")

  private def done(heard: Chunk[Feed]): Option[CallResult] =
    heard.collect { case Feed.Done(result) => result }.lastOption

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    ProjectGraph.trusted(List(law), Splits.aggregate, Catalog.empty, None, Nil, resolution = Resolution.Solve)
end DaemonOpenLawSpec
