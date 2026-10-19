package histos.server

import histos.bsp.{Frame, Rpc}
import histos.cli.{Cli, Command}
import histos.resolve.Resolve
import histos.zinc.GraphCompile
import histos.{Jvm, Projects, ScalaVersion, module}

import java.net.UnixDomainSocketAddress
import java.nio.channels.{Channels, SocketChannel}
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.{Scope, ZIO, durationInt}
import zio.test.*

object ServerSpec extends ZIOSpecDefault:
  private val protocol = suite("record")(
    test("server is a command"):
      Cli.parse(List("server")).map(parsed => assertTrue(parsed == Command.Server)),
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
          _ <- ZIO.attempt(write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\nchanged\n"))
          drifted <- Attach.decide(root, Cli.VersionText)
          bare <- temp
          absent <- Attach.decide(bare, Cli.VersionText)
        yield assertTrue(
          matched match
            case Decision.Open(found) => found == socket
            case _                    => false
          ,
          version == Decision.Stale(Drift.Version),
          drifted == Decision.Stale(Drift.Fingerprint),
          absent == Decision.Absent,
        ),
    test("editing a path plugin source is fingerprint drift"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
                 write(root, "histos.scala", "plugin(path\"tools/docs\")\nobject build\n")
                 write(root, "tools/docs/Echo.scala", "object Echo\n")
                 write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
               }
          fingerprint <- Fingerprint.read(root)
          socket = root.resolve("missing.sock").toAbsolutePath.normalize
          _ <- Record.write(root, Cli.VersionText, fingerprint, socket)
          matched <- Attach.decide(root, Cli.VersionText)
          beforeScala <- ZIO.attempt(Files.readString(root.resolve("histos.scala")))
          beforeLock <- ZIO.attempt(Files.readString(root.resolve(".histos").resolve("dependencies.lock")))
          _ <- ZIO.attempt(write(root, "tools/docs/Echo.scala", "object Echo\n// edited\n"))
          drifted <- Attach.decide(root, Cli.VersionText)
          afterScala <- ZIO.attempt(Files.readString(root.resolve("histos.scala")))
          afterLock <- ZIO.attempt(Files.readString(root.resolve(".histos").resolve("dependencies.lock")))
        yield assertTrue(
          matched match
            case Decision.Open(_) => true
            case _                => false
          ,
          drifted == Decision.Stale(Drift.Fingerprint),
          beforeScala == afterScala,
          beforeLock == afterLock,
        ),
  )

  private val attached = suite("daemon")(
    test("a matching server compiles, and a changed lock is refused"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(Projects(module("demo", Jvm)(ScalaVersion("3.10.0"))))
              name <- built.projects.collect { case item: histos.Module => item.name } match
                        case found :: Nil => ZIO.succeed(found)
                        case _            => ZIO.fail(Fixture.Shape)
              _ <- ZIO.attempt {
                     write(root, "histos.scala", "object build\n")
                     write(root, "demo/src/Hello.scala", "object Hello {\n  def n: Int = 1\n}\n")
                     write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
                   }
              jdk <- GraphCompile.jdkHere
              record <- Daemon.open(root, built, jdk, Cli.VersionText)
              feeds <- Client.talk(record.socket, Call.Compile(Nil)).runCollect
              classFile <- ZIO.attempt(Files.isRegularFile(GraphCompile.classesDir(root, name, ScalaVersion("3.10.0")).resolve("Hello.class")))
              _ <- ZIO.attempt(write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\nchanged\n"))
              refused <- Client.talk(record.socket, Call.Compile(Nil)).runDrain.either
            yield assertTrue(
              feeds.exists {
                case Feed.Out(text) => text.contains("demo") && text.contains("jvm")
                case _              => false
              },
              feeds.exists {
                case Feed.Exit(code, err) => code == 0 && err.isEmpty
                case _                    => false
              },
              classFile,
              refused == Left(ServerError.Stale),
            )
          program.provideSome[Scope](Resolve.live(root.resolve("coursier")))
    ,
    test("a path plugin edit stays stale on a second call"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(Projects(module("demo", Jvm)(ScalaVersion("3.10.0"))))
              _ <- ZIO.attempt {
                     write(root, "histos.scala", "plugin(path\"tools/docs\")\nobject build\n")
                     write(root, "tools/docs/Echo.scala", "object Echo\n")
                     write(root, ".histos/dependencies.lock", "cell demo 3.10.0 jvm\n")
                   }
              jdk <- GraphCompile.jdkHere
              record <- Daemon.open(root, built, jdk, Cli.VersionText)
              _ <- ZIO.attempt(write(root, "tools/docs/Echo.scala", "object Echo\n// edited\n"))
              first <- Client.talk(record.socket, Call.Compile(Nil)).runDrain.either
              second <- Client.talk(record.socket, Call.Compile(Nil)).runDrain.either
            yield assertTrue(first == Left(ServerError.Stale), second == Left(ServerError.Stale))
          program.provideSome[Scope](Resolve.live(root.resolve("coursier")))
    ,
    test("the daemon answers bsp initialize, and exit before shutdown is status 1"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              built <- ZIO.fromEither(Projects(module("demo", Jvm)(ScalaVersion("3.10.0"))))
              _ <- ZIO.attempt(write(root, "histos.scala", "object build\n"))
              jdk <- GraphCompile.jdkHere
              record <- Daemon.open(root, built, jdk, Cli.VersionText)
              heard <- bsp(record.socket)
            yield assertTrue(
              heard.result.displayName == "histos",
              heard.marker == Message.Marker(1),
            )
          program.provideSome[Scope](Resolve.live(root.resolve("coursier")))
  )

  private enum Fixture:
    case Shape
    case Reply

  private final case class Heard(result: Rpc.InitializeResult, marker: Message)

  private def bsp(socket: Path) =
    ZIO.acquireRelease(ZIO.attempt(SocketChannel.open(UnixDomainSocketAddress.of(socket))))(channel =>
      ZIO.attempt(channel.close()).ignore
    ).flatMap { channel =>
      val input = Channels.newInputStream(channel)
      val output = Channels.newOutputStream(channel)
      for
        _ <- Frame.write(output, Call.encode(Call.Bsp))
        _ <- Frame.read(input)
        _ <- Frame.write(output, Rpc.Incoming(method = Some("build/initialize"), id = Some(Rpc.RpcId.Num(1))).toJson)
        init <- Frame.read(input).flatMap(decoded[Rpc.RpcOk[Rpc.InitializeResult]])
        _ <- Frame.write(output, Rpc.Incoming(method = Some("build/exit")).toJson)
        marker <- Frame.read(input).flatMap(decoded[Message])
      yield Heard(init.result, marker)
    }

  private def decoded[A: JsonDecoder](frame: Option[String]): ZIO[Any, Fixture, A] =
    frame match
      case None => ZIO.fail(Fixture.Reply)
      case Some(text) => ZIO.fromEither(text.fromJson[A]).mapError(_ => Fixture.Reply)

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-server"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("server")(protocol, attached) @@ TestAspect.sequential @@ TestAspect.timeout(4.minutes)
