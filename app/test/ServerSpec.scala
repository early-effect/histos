package histos.server

import histos.bsp.{Frame, Rpc}
import histos.cli.{Cli, CliExit, Command}
import histos.jdk.{Machine, Os, Settled}
import histos.native.NativeOrigin
import histos.load.{BuildNotes, LoadError, LoadedBuild, Loader}
import histos.resolve.Resolve
import histos.zinc.GraphCompile
import histos.{Jvm, ProjectGraph, Projects, ScalaVersion, module}

import java.net.UnixDomainSocketAddress
import java.nio.channels.{Channels, SocketChannel}
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.{Promise, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object ServerSpec extends ZIOSpecDefault:
  private val protocol = suite("record")(
    test("server is a command"):
      Cli
        .parse(List("server"))
        .map(parsed => assertTrue(parsed == Command.Server))
    ,
    test("a record attaches only when the version and the fingerprint match"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(root, "histos.scala", "object build\n")
            write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
          }
          fingerprint <- Fingerprint.read(root)
          socket = root.resolve("missing.sock").toAbsolutePath.normalize
          _ <- Record.write(root, Cli.VersionText, fingerprint, socket)
          matched <- Attach.decide(root, Cli.VersionText)
          version <- Attach.decide(root, "0.0.0")
          _ <- ZIO.attempt(
            write(
              root,
              ".histos/dependencies.lock",
              "cell demo 3.10.0 jvm\nchanged\n"
            )
          )
          drifted <- Attach.decide(root, Cli.VersionText)
          bare <- temp
          absent <- Attach.decide(bare, Cli.VersionText)
        yield assertTrue(
          matched match
            case Decision.Open(found) => found == socket
            case _                    => false
          ,
          version == Decision.DifferentVersion,
          drifted match
            case Decision.Open(found) => found == socket
            case _                    => false
          ,
          absent == Decision.Absent
        )
    ,
    test("editing a path plugin source is fingerprint drift"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
            write(
              root,
              "histos.scala",
              "plugin(path\"tools/docs\")\nobject build\n"
            )
            write(root, "tools/docs/Echo.scala", "object Echo\n")
            write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
          }
          fingerprint <- Fingerprint.read(root)
          socket = root.resolve("missing.sock").toAbsolutePath.normalize
          _ <- Record.write(root, Cli.VersionText, fingerprint, socket)
          matched <- Attach.decide(root, Cli.VersionText)
          beforeScala <- ZIO.attempt(
            Files.readString(root.resolve("histos.scala"))
          )
          beforeLock <- ZIO.attempt(
            Files.readString(
              root.resolve(".histos").resolve("dependencies.lock")
            )
          )
          _ <- ZIO.attempt(
            write(root, "tools/docs/Echo.scala", "object Echo\n// edited\n")
          )
          drifted <- Attach.decide(root, Cli.VersionText)
          afterScala <- ZIO.attempt(
            Files.readString(root.resolve("histos.scala"))
          )
          afterLock <- ZIO.attempt(
            Files.readString(
              root.resolve(".histos").resolve("dependencies.lock")
            )
          )
        yield assertTrue(
          matched match
            case Decision.Open(_) => true
            case _                => false
          ,
          drifted match
            case Decision.Open(_) => true
            case _                => false
          ,
          beforeScala == afterScala,
          beforeLock == afterLock
        )
  )

  private val attached = suite("daemon")(
    test("a matching server compiles, and a changed lock is refused"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(
                Projects(module("demo", Jvm)(ScalaVersion("3.10.0")))
              )
              name <- built.projects.collect { case item: histos.Module =>
                item.name
              } match
                case found :: Nil => ZIO.succeed(found)
                case _            => ZIO.fail(Fixture.Shape)
              _ <- ZIO.attempt {
                write(root, "histos.scala", "object build\n")
                write(
                  root,
                  "demo/src/Hello.scala",
                  "object Hello {\n  def n: Int = 1\n}\n"
                )
                write(
                  root,
                  ".histos/dependencies.lock",
                  "cell demo 3.10.0 jvm\n"
                )
              }
              jdk <- GraphCompile.jdkHere
              running <- Daemon.open(
                root,
                built,
                jdk,
                Cli.VersionText,
                root.resolve("missing-api.jar"),
                Nil,
                ZIO.succeed(histos.load.BuildNotes(false, Nil))
              )
              feeds <- Client.talk(running.record.socket, Call.Compile(Nil)).runCollect
              classFile <- ZIO.attempt(
                Files.isRegularFile(
                  GraphCompile
                    .classesDir(root, name, ScalaVersion("3.10.0"))
                    .resolve("Hello.class")
                )
              )
              _ <- ZIO.attempt(
                write(
                  root,
                  ".histos/dependencies.lock",
                  "cell demo 3.10.0 jvm\nchanged\n"
                )
              )
              refused <- Client
                .talk(running.record.socket, Call.Compile(Nil))
                .runCollect
            yield assertTrue(
              feeds.exists {
                case Feed.Out(text) =>
                  text.contains("demo") && text.contains("jvm")
                case _ => false
              },
              feeds.exists {
                case Feed.Exit(code, err) => code == 0 && err.isEmpty
                case _                    => false
              },
              classFile,
              refused.exists {
                case Feed.Exit(code, err) =>
                  code == histos.cli.CliExit.LoadFailed.code && err.nonEmpty
                case _ => false
              }
            )
          program.provideSome[Scope](
            liveLayers(root)
          )
    ,
    test("a path plugin edit stays stale on a second call"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(
                Projects(module("demo", Jvm)(ScalaVersion("3.10.0")))
              )
              _ <- ZIO.attempt {
                write(
                  root,
                  "histos.scala",
                  "plugin(path\"tools/docs\")\nobject build\n"
                )
                write(root, "tools/docs/Echo.scala", "object Echo\n")
                write(
                  root,
                  ".histos/dependencies.lock",
                  "cell demo 3.10.0 jvm\n"
                )
              }
              jdk <- GraphCompile.jdkHere
              running <- Daemon.open(
                root,
                built,
                jdk,
                Cli.VersionText,
                root.resolve("missing-api.jar"),
                Nil,
                ZIO.succeed(histos.load.BuildNotes(false, Nil))
              )
              _ <- ZIO.attempt(
                write(root, "tools/docs/Echo.scala", "object Echo\n// edited\n")
              )
              first <- Client.talk(running.record.socket, Call.Compile(Nil)).runCollect
              second <- Client.talk(running.record.socket, Call.Compile(Nil)).runCollect
            yield assertTrue(
              first.exists {
                case Feed.Exit(code, _) =>
                  code == histos.cli.CliExit.LoadFailed.code
                case _ => false
              },
              second.exists {
                case Feed.Exit(code, _) =>
                  code == histos.cli.CliExit.LoadFailed.code
                case _ => false
              }
            )
          program.provideSome[Scope](
            liveLayers(root)
          )
    ,
    test(
      "the daemon answers bsp initialize, and exit before shutdown is status 1"
    ):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(
                Projects(module("demo", Jvm)(ScalaVersion("3.10.0")))
              )
              _ <- ZIO.attempt(write(root, "histos.scala", "object build\n"))
              jdk <- GraphCompile.jdkHere
              running <- Daemon.open(
                root,
                built,
                jdk,
                Cli.VersionText,
                root.resolve("missing-api.jar"),
                Nil,
                ZIO.succeed(histos.load.BuildNotes(false, Nil))
              )
              heard <- bsp(running.record.socket)
            yield assertTrue(
              heard.result.displayName == "histos",
              heard.marker == Message.Marker(1)
            )
          program.provideSome[Scope](
            liveLayers(root)
          )
    ,
    test("graph is served, and cancel of an unknown run fails"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(
                Projects(module("demo", Jvm)(ScalaVersion("3.10.0")))
              )
              _ <- ZIO.attempt(write(root, "histos.scala", "object build\n"))
              jdk <- GraphCompile.jdkHere
              running <- Daemon.open(
                root,
                built,
                jdk,
                Cli.VersionText,
                root.resolve("missing-api.jar"),
                Nil,
                ZIO.succeed(histos.load.BuildNotes(false, Nil))
              )
              shown <- Client.talk(running.record.socket, Call.Graph).runCollect
              missing <- Client
                .talk(running.record.socket, Call.Cancel("missing"))
                .runCollect
            yield assertTrue(
              shown.exists {
                case Feed.Out(text) => text.contains("demo")
                case _              => false
              },
              shown.exists {
                case Feed.Exit(code, err) => code == 0 && err.isEmpty
                case _                    => false
              },
              missing.exists {
                case Feed.Exit(code, err) =>
                  code == CliExit.TaskFailed.code && err.contains("no run")
                case _ => false
              }
            )
          program.provideSome[Scope](
            liveLayers(root)
          )
  )

  private val lifecycle = suite("lifecycle")(
    test("compile attaches to a running server without loading the build"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- demo
              _ <- ZIO.attempt {
                write(root, "histos.scala", "object build\n")
                write(root, "demo/src/Hello.scala", "object Hello {\n  def n: Int = 1\n}\n")
                write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
              }
              _ <- daemon(root, built)
              done <- Cli.execute(List("compile"), root, root.resolve("missing-api.jar"))
              classFile <- ZIO.attempt(
                Files.isRegularFile(root.resolve(".histos/classes/demo/3.10.0/jvm/Hello.class"))
              )
            yield assertTrue(done.exit == CliExit.Success, classFile)
          program.provideSome[Scope](liveLayers(root) ++ ZLayer.succeed(machine(root)))
    ,
    test("a cell that fails to compile fails a compile sent to the server"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- demo
              _ <- ZIO.attempt {
                write(root, "histos.scala", "object build\n")
                write(root, "demo/src/Bad.scala", "object Bad {\n  val n: Int = \"text\"\n}\n")
                write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
              }
              running <- daemon(root, built)
              feeds <- Client.talk(running.record.socket, Call.Compile(Nil)).runCollect
            yield assertTrue(feeds.exists {
              case Feed.Exit(code, _) => code == CliExit.CompileFailed.code
              case _                  => false
            })
          program.provideSome[Scope](liveLayers(root))
    ,
    test("a server that cannot write its record fails to open, and leaves no socket"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- demo
              _ <- ZIO.attempt {
                write(root, "histos.scala", "object build\n")
                write(root, ".histos/server.json/keep", "a directory where the record goes\n")
              }
              opened <- ZIO.scoped(daemon(root, built).either).timeout(30.seconds)
              socket <- ZIO.attempt(Files.exists(Record.socket(root)))
            yield assertTrue(opened == Some(Left(ServerError.Io(ServerIo.Save))), !socket)
          program.provideSome[Scope](liveLayers(root))
    ,
    test("reboot starts a successor, and the old server stops with an idle connection open"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- demo
              _ <- ZIO.attempt(write(root, "histos.scala", "object build\n"))
              launched <- Promise.make[Nothing, Unit]
              running <- daemon(root, built, _ => launched.succeed(()).unit)
              _ <- connected(running.record.socket)
              feeds <- Client.talk(running.record.socket, Call.Reboot).runCollect
              stopped <- running.done.timeout(30.seconds)
              relaunched <- launched.isDone
              record <- ZIO.attempt(Files.exists(Record.file(root)))
            yield assertTrue(
              feeds.contains(Feed.Exit(CliExit.Success.code, "")),
              stopped == Some(()),
              relaunched,
              !record,
            )
          program.provideSome[Scope](liveLayers(root))
    ,
    test("a bsp session reloads a changed build and announces the new targets"):
      ZIO.scoped:
        temp.zip(demoAndExtra).flatMap: (root, grown) =>
          val program =
            for
              built <- demo
              _ <- ZIO.attempt(write(root, "histos.scala", "object build\n"))
              running <- daemon(root, built)
              session <- bspSession(running.record.socket)
              _ <- ZIO.attempt(write(root, "histos.scala", "object build\n// edited\n"))
              heard <- session.ask(2, "workspace/buildTargets")
              targets <- decoded[Rpc.RpcOk[Rpc.BuildTargetsResult]](Some(heard.reply))
            yield assertTrue(
              heard.notes.contains("buildTarget/didChange"),
              targets.result.targets.exists(_.id.uri.contains("extra")),
            )
          program.provideSome[Scope](layers(root, ZLayer.succeed(reloading(grown))))
    ,
    test("a reload announces the change to every spliced bsp session"):
      ZIO.scoped:
        temp.zip(demoAndExtra).flatMap: (root, grown) =>
          val program =
            for
              built <- demo
              _ <- ZIO.attempt(write(root, "histos.scala", "object build\n"))
              running <- daemon(root, built)
              first <- bspSession(running.record.socket)
              second <- bspSession(running.record.socket)
              reloaded <- Client.talk(running.record.socket, Call.Reload).runCollect
              firstNote <- first.next.timeout(30.seconds)
              secondNote <- second.next.timeout(30.seconds)
            yield assertTrue(
              reloaded.contains(Feed.Exit(CliExit.Success.code, "")),
              firstNote == Some("buildTarget/didChange"),
              secondNote == Some("buildTarget/didChange"),
            )
          program.provideSome[Scope](layers(root, ZLayer.succeed(reloading(grown))))
  ) @@ TestAspect.withLiveClock

  private val demo = ZIO.fromEither(Projects(module("demo", Jvm)(ScalaVersion("3.10.0"))))

  private val demoAndExtra =
    ZIO.fromEither(Projects(module("demo", Jvm)(ScalaVersion("3.10.0")), module("extra", Jvm)(ScalaVersion("3.10.0"))))

  private def daemon(
      root: Path,
      graph: ProjectGraph,
      relaunch: Path => zio.IO[ServerError, Unit] = _ => ZIO.unit
  ) =
    GraphCompile.jdkHere.flatMap: jdk =>
      Daemon.open(
        root,
        graph,
        jdk,
        Cli.VersionText,
        root.resolve("missing-api.jar"),
        Nil,
        ZIO.succeed(BuildNotes(false, Nil)),
        relaunch = relaunch
      )

  /** A loader whose every load is the next graph, as if `histos.scala` changed. */
  private def reloading(next: ProjectGraph): Loader =
    new Loader:
      def scan(repo: Path) = ZIO.fail(LoadError.NoEntry)
      def load(repo: Path, apiJar: Path) = ZIO.succeed(LoadedBuild(next, Calls.quiet))
      def buildClasspath(apiJar: Path) = ZIO.succeed(Nil)
      def reportBuild(repo: Path, apiJar: Path) = ZIO.succeed(BuildNotes(false, Nil))

  private def deps(root: Path) =
    Resolve.live(root.resolve("coursier")) ++
      ZLayer.succeed(histos.cache.ContentStore.in(root.resolve("cache")))

  private def liveLayers(root: Path) =
    deps(root) >+> histos.load.Loader.live

  private def layers(root: Path, loader: zio.ULayer[Loader]) =
    deps(root) ++ loader

  private def machine(root: Path): Machine =
    Machine(Map.empty, root.toString, Os.Linux, root.resolve("cache"), root, None, Settled.No, NativeOrigin.Central)

  private final case class Answered(reply: String, notes: List[String])

  /** An initialized BSP splice. `ask` collects notification methods until the reply with `id`. */
  private final case class BspSession(input: java.io.InputStream, output: java.io.OutputStream):
    def ask(id: Int, method: String): ZIO[Any, Throwable | Frame.FrameError | Fixture, Answered] =
      Frame.write(output, Rpc.Incoming(method = Some(method), id = Some(Rpc.RpcId.Num(id))).toJson) *>
        collect(id, Nil)

    def next: ZIO[Any, Frame.FrameError | Fixture, String] =
      Frame.read(input).flatMap(decoded[Rpc.Incoming]).flatMap(note => ZIO.fromOption(note.method).orElseFail(Fixture.Reply))

    private def collect(id: Int, notes: List[String]): ZIO[Any, Frame.FrameError | Fixture, Answered] =
      Frame.read(input).flatMap:
        case None => ZIO.fail(Fixture.Reply)
        case Some(text) =>
          decoded[Rpc.Incoming](Some(text)).flatMap: frame =>
            frame.method match
              case Some(note) => collect(id, notes :+ note)
              case None if frame.id.contains(Rpc.RpcId.Num(id)) => ZIO.succeed(Answered(text, notes))
              case None => collect(id, notes)

  private def connected(socket: Path) =
    ZIO.acquireRelease(ZIO.attempt(SocketChannel.open(UnixDomainSocketAddress.of(socket))))(channel => ZIO.attempt(channel.close()).ignore)

  private def bspSession(socket: Path) =
    for
      channel <- connected(socket)
      session = BspSession(Channels.newInputStream(channel), Channels.newOutputStream(channel))
      _ <- Frame.write(session.output, Call.encode(Call.Bsp))
      _ <- Frame.read(session.input)
      _ <- session.ask(1, "build/initialize")
      _ <- Frame.write(session.output, Rpc.Incoming(method = Some("build/initialized")).toJson)
    yield session

  private enum Fixture:
    case Shape
    case Reply

  private final case class Heard(result: Rpc.InitializeResult, marker: Message)

  private def bsp(socket: Path) =
    ZIO
      .acquireRelease(
        ZIO.attempt(SocketChannel.open(UnixDomainSocketAddress.of(socket)))
      )(channel => ZIO.attempt(channel.close()).ignore)
      .flatMap { channel =>
        val input = Channels.newInputStream(channel)
        val output = Channels.newOutputStream(channel)
        for
          _ <- Frame.write(output, Call.encode(Call.Bsp))
          _ <- Frame.read(input)
          _ <- Frame.write(
            output,
            Rpc
              .Incoming(
                method = Some("build/initialize"),
                id = Some(Rpc.RpcId.Num(1))
              )
              .toJson
          )
          init <- Frame
            .read(input)
            .flatMap(decoded[Rpc.RpcOk[Rpc.InitializeResult]])
          _ <- Frame.write(
            output,
            Rpc.Incoming(method = Some("build/exit")).toJson
          )
          marker <- Frame.read(input).flatMap(decoded[Message])
        yield Heard(init.result, marker)
      }

  private def decoded[A: JsonDecoder](
      frame: Option[String]
  ): ZIO[Any, Fixture, A] =
    frame match
      case None       => ZIO.fail(Fixture.Reply)
      case Some(text) =>
        ZIO.fromEither(text.fromJson[A]).mapError(_ => Fixture.Reply)

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-server"))):
      dir =>
        ZIO.attempt {
          if Files.exists(dir) then
            val walk = Files.walk(dir)
            val paths =
              try walk.iterator.asScala.toList
              finally walk.close()
            paths
              .sortBy(_.getNameCount)
              .reverse
              .foreach(path => Files.deleteIfExists(path))
        }.ignore

  def spec =
    suite("server")(protocol, attached, lifecycle) @@
      TestAspect.withLiveSystem @@
      TestAspect.timeout(4.minutes)
