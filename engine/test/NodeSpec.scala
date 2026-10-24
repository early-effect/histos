package histos.node

import histos.toolchain.{RuntimeLockFixture, ToolLockError}
import histos.archive.{
  ArchiveSource,
  ArchiveKind,
  CommonsArchiveDecoder,
  ExtractFailure,
  HttpsUrl,
  ArchiveFixture,
  ArchiveStage
}
import histos.jdk.{Machine, Os, Settled, Sha256}
import histos.native.NativeOrigin
import histos.process.{ChildEvent, ChildProcess, ProcessExit}
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos
}

import histos.toolchain.RuntimeIdentity
import heddle.client.{Client, ClientError}
import heddle.http.{Body, Request, Response}

import histos.workspace.{IoFailure, Workspace}
import histos.toolchain.RuntimeFiles

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.{Chunk, IO, Scope, UIO, ZIO, durationInt}
import zio.test.*

object NodeSpec extends ZIOSpecDefault:
  private val major22 = NodeMajor.trusted(22)
  private val major20 = NodeMajor.trusted(20)
  private val v22 = NodeVersion.trusted("22.14.0", major22)
  private val v20 = NodeVersion.trusted("20.11.1", major20)

  private def home(path: String, version: NodeVersion): NodeHome =
    NodeHome(Path.of(path), hasBinary = true, version = Right(version))

  private val bare =
    NodeView(
      NodeLocated.Unset,
      NodeLocated.Unset,
      NodeManagerSlot.Absent,
      NodeManagerSlot.Absent,
      NodeManagerSlot.Absent,
      Nil
    )

  private val policy = suite("resolution")(
    test("PATH and managers are diagnostic, not authority"):
      val found = Node.resolve(
        major22,
        bare.copy(
          onPath = NodeLocated.Found(home("/path/22", v22)),
          nvm = NodeManagerSlot.Present(List(home("/nvm/22", v22)))
        )
      )
      assertTrue(
        found.is(_.subtype[NodeResolution.Provision]).major == major22,
        found
          .is(_.subtype[NodeResolution.Provision])
          .trace
          .contains(
            NodeCandidate.Match(NodeOrigin.Path, Path.of("/path/22"), v22)
          ),
        found
          .is(_.subtype[NodeResolution.Provision])
          .trace
          .contains(
            NodeCandidate.Match(NodeOrigin.Nvm, Path.of("/nvm/22"), v22)
          )
      )
    ,
    test("an arbitrary cached Node does not replace the archive pin"):
      assertTrue(
        Node
          .resolve(major22, bare.copy(cached = List(home("/cache/22", v22))))
          .is(_.subtype[NodeResolution.Provision])
          .major == major22
      )
    ,
    test("a matching explicit override is selected"):
      assertTrue(
        Node
          .resolve(
            major22,
            bare.copy(explicit = NodeLocated.Found(home("/local/22", v22)))
          )
          .is(_.subtype[NodeResolution.Selected])
          .chosen == NodeCandidate
          .Match(NodeOrigin.Override, Path.of("/local/22"), v22)
      )
    ,
    test("a bare machine provisions"):
      assertTrue(
        Node
          .resolve(major22, bare)
          .is(_.subtype[NodeResolution.Provision])
          .major == major22
      )
    ,
    test("a wrong explicit override stops"):
      val found = Node.resolve(
        major22,
        bare.copy(
          explicit = NodeLocated.Found(home("/override/20", v20)),
          onPath = NodeLocated.Found(home("/opt/node", v22))
        )
      )
      assertTrue(
        found
          .is(_.subtype[NodeResolution.OverrideRejected])
          .reason == NodeReject.WrongMajor(major20)
      )
  )

  private val versions = suite("versions")(
    test("node --version drops the leading v, and a major is not a pin"):
      assertTrue(
        NodeVersion.from("v22.14.0") == Right(v22),
        NodeVersion.from("22.14.0") == Right(v22),
        NodeVersion.from("22") == Left(NodeVersionError.Bad("22")),
        NodeVersion.from("0.1.0") == Left(NodeVersionError.Bad("0.1.0")),
        NodeMajor.from(0) == Left(NodeError.BadMajor(0))
      )
  )

  private val locks = suite("node lock")(
    test("host rows round-trip and mismatched majors retain their pin error"):
      for lock <- ZIO.fromEither(
          RuntimeLockFixture.node(
            v22,
            ArchiveSource.Local(Path.of("/opt/node.zip")),
            Sha256.of(Chunk.empty)
          )
        )
      yield assertTrue(
        NodeLock.parse(NodeLock.render(lock)).map(NodeLock.render) == Right(
          NodeLock.render(lock)
        ),
        NodeLock.parse(
          NodeLock.render(lock).replace("version 22.14.0", "version 20.11.1")
        ) == Left(
          NodeLockError.Schema(
            ToolLockError.Pin(NodePinError.MajorDisagrees(major22, major20))
          )
        ),
        NodeLock.parse("") == Left(NodeLockError.Schema(ToolLockError.Empty)),
        NodeLock.parse("nope") == Left(
          NodeLockError.Schema(ToolLockError.UnknownField("nope"))
        )
      )
  )

  private val asks = suite("when node is required")(
    test("only a js module asks for node"):
      val named = ProjectName
        .from("core")
        .map: name =>
          def one(platform: Platform) =
            ProjectGraph.trusted(
              List(
                Module(
                  name,
                  Platforms(platform, Nil),
                  ScalaVersions(ScalaVersion.trusted("3.9.0"), Nil),
                  Nil,
                  Nil,
                  SourcePos("histos.scala", 1, 1)
                )
              )
            )
          (Node.required(one(Platform.Jvm)), Node.required(one(Platform.Js)))
      assertTrue(named == Right((false, true)))
  )

  private val layouts = suite("fixture node")(
    test("a locked survey does not execute or report an unrelated nvm Node"):
      ZIO.scoped:
        for
          root <- temp
          nvm = root
            .resolve(".nvm")
            .resolve("versions")
            .resolve("node")
            .resolve("v22.14.0")
          _ <- ZIO.attempt(script(nvm, "v22.14.0"))
          machine = Machine(
            Map("NVM_DIR" -> root.resolve(".nvm").toString),
            root.toString,
            Os.Linux,
            histos.toolchain.Architecture.X86_64,
            root.resolve("cache"),
            root,
            None,
            Settled.No,
            NativeOrigin.Central
          )
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          _ <- writeLock(
            root,
            ArchiveSource.Remote(url),
            Sha256.of(Chunk.fromArray("x".getBytes))
          )
          found <- NodeToolchain.explain(machine)
          text = NodeToolchain.text(found)
        yield assertTrue(
          !text.contains("match nvm 22.14.0"),
          found.resolution
            .is(_.subtype[NodeResolution.Provision])
            .major == major22,
          !text.contains(nvm.toString),
          !text.contains("java")
        )
    ,
    test(
      "a probe leaves a node's mode alone, and a node that cannot run is not chosen"
    ):
      ZIO.scoped:
        for
          root <- temp
          nvm = root
            .resolve(".nvm")
            .resolve("versions")
            .resolve("node")
            .resolve("v22.14.0")
          node = nvm.resolve("bin").resolve("node")
          _ <- ZIO.attempt(script(nvm, "v22.14.0"))
          _ <- ZIO.attempt(
            Files.setPosixFilePermissions(
              node,
              PosixFilePermissions.fromString("rw-r--r--")
            )
          )
          before <- ZIO.attempt(
            Files.getPosixFilePermissions(node).asScala.toSet
          )
          machine = Machine(
            Map("NVM_DIR" -> root.resolve(".nvm").toString),
            root.toString,
            Os.Linux,
            histos.toolchain.Architecture.X86_64,
            root.resolve("cache"),
            root,
            None,
            Settled.No,
            NativeOrigin.Central
          )
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          _ <- writeLock(
            root,
            ArchiveSource.Remote(url),
            Sha256.of(Chunk.fromArray("x".getBytes))
          )
          found <- NodeToolchain.explain(machine)
          after <- ZIO.attempt(
            Files.getPosixFilePermissions(node).asScala.toSet
          )
        yield assertTrue(
          after == before,
          found.resolution
            .is(_.subtype[NodeResolution.Provision])
            .major == major22,
          found.resolution.is(_.subtype[NodeResolution.Provision]).trace == Nil
        )
    ,
    test("a malformed node.local fails without falling back"):
      ZIO.scoped:
        for
          root <- temp
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          _ <- writeLock(
            root,
            ArchiveSource.Remote(url),
            Sha256.of(Chunk.empty)
          )
          _ <- ZIO.attempt {
            Files.writeString(root.resolve(".histos/node.local"), "/one\n/two")
          }
          result <- NodeToolchain.resolve(at(root)).either
        yield assertTrue(
          result == Left(
            NodeMiss.Unresolved(NodeError.Override(NodeOverrideError.Multiple))
          )
        )
    ,
    test("a local Node reports a content identity and never downloads"):
      ZIO.scoped:
        for
          root <- temp
          local = root.resolve("local")
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          _ <- writeLock(
            root,
            ArchiveSource.Remote(url),
            Sha256.of(Chunk.empty)
          )
          _ <- ZIO.attempt {
            script(local, "v22.14.0")
            Files.writeString(
              root.resolve(".histos/node.local"),
              local.toString
            )
          }
          first <- NodeToolchain.resolve(at(root))
          _ <- ZIO.attempt(Files.writeString(local.resolve("extra"), "changed"))
          second <- NodeToolchain.resolve(at(root))
        yield assertTrue(
          first.binary.path == local.resolve("bin/node"),
          first.identity
            .is(_.subtype[RuntimeIdentity.Local])
            .home
            .endsWith(Path.of("local")),
          first.identity != second.identity
        )
    ,
    test("the locked full version is validated before installation"):
      ZIO.scoped:
        for
          root <- temp
          zip <- zipOf("node/bin/node" -> scriptBytes("v22.15.0"))
          file = root.resolve("different.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          sha = Sha256.of(Chunk.fromArray(zip))
          result <- ZIO
            .fromEither(
              RuntimeLockFixture.node(v22, ArchiveSource.Local(file), sha)
            )
            .flatMap(lock => NodeStage.stage(lock, at(root)))
            .either
          left <- names(root.resolve("cache/nodes").resolve(sha.value))
        yield assertTrue(
          result == Left(NodeStageError.NoHome),
          left == List("archive")
        )
    ,
    test("a malformed Node version retains its typed version failure"):
      ZIO.scoped:
        for
          root <- temp
          zip <- zipOf("node/bin/node" -> scriptBytes("bad"))
          file = root.resolve("bad-version.zip")
          _ <- ZIO.attemptBlocking(Files.write(file, zip))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.node(
              v22,
              ArchiveSource.Local(file),
              Sha256.of(Chunk.fromArray(zip))
            )
          )
          result <- NodeStage.stage(lock, at(root)).either
          left <- names(
            root
              .resolve("cache/nodes")
              .resolve(Sha256.of(Chunk.fromArray(zip)).value)
          )
        yield assertTrue(
          result
            .is(_.left)
            .is(_.subtype[NodeStageError.Version])
            .error == NodeVersionError.Bad("bad"),
          left == List("archive")
        )
    ,
    test("a malformed zip reports an extraction failure"):
      ZIO.scoped:
        for
          root <- temp
          raw <- zipOf("node/bin/node" -> Array[Byte](1, 2, 3))
          zip = raw.take(24)
          file = root.resolve("malformed.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          sha = Sha256.of(Chunk.fromArray(zip))
          out <- ZIO
            .fromEither(
              RuntimeLockFixture.node(v22, ArchiveSource.Local(file), sha)
            )
            .flatMap(lock => NodeStage.stage(lock, at(root)))
            .either
          archive = root
            .resolve("cache")
            .resolve("nodes")
            .resolve(sha.value)
            .resolve("archive")
        yield assertTrue(
          out == Left(
            NodeStageError.Extract(
              archive,
              ExtractFailure.Malformed(ArchiveKind.Zip)
            )
          )
        )
    ,
    test("a failed node unpack leaves no partial directory and no temp file"):
      ZIO.scoped:
        for
          root <- temp
          bad <- zipOf(
            "node/bin/node" -> scriptBytes("v22.14.0"),
            "../outside" -> scriptBytes("v22.14.0")
          )
          file = root.resolve("bad.zip")
          _ <- ZIO.attempt(Files.write(file, bad))
          sha = Sha256.of(Chunk.fromArray(bad))
          out <- ZIO
            .fromEither(
              RuntimeLockFixture.node(v22, ArchiveSource.Local(file), sha)
            )
            .flatMap(lock => NodeStage.stage(lock, at(root)))
            .either
          left <- names(
            root.resolve("cache").resolve("nodes").resolve(sha.value)
          )
        yield assertTrue(
          out == Left(NodeStageError.UnsafePath("../outside")),
          left == List("archive")
        )
    ,
    test("a local tar.gz or tar.xz node stages with a bin/node that runs"):
      ZIO.scoped:
        for
          root <- temp
          env <- zio.System.envs
          tree = root.resolve("tree")
          _ <- ZIO.attempt {
            script(tree.resolve("node-v22.14.0-darwin-arm64"), "v22.14.0")
            Files.createDirectories(root.resolve("pack"))
          }
          gz = root.resolve("node.tar.gz")
          xz = root.resolve("node.tar.xz")
          gzPacked <- pack(
            List(
              "tar",
              "-czf",
              gz.toString,
              "-C",
              tree.toString,
              "node-v22.14.0-darwin-arm64"
            ),
            root,
            env
          )
          xzPacked <- pack(
            List(
              "tar",
              "-cJf",
              xz.toString,
              "-C",
              tree.toString,
              "node-v22.14.0-darwin-arm64"
            ),
            root,
            env
          )
          machine = at(root).copy(env = env)
          gzHome <- stageFile(gz, machine)
          xzHome <- ZIO.when(xzPacked)(stageFile(xz, machine))
          runs <- ZIO.attempt(
            Files.isExecutable(gzHome.path.resolve("bin").resolve("node"))
          )
        yield assertTrue(
          gzPacked,
          gzHome.version == Right(v22),
          runs,
          xzHome.forall(_.version == Right(v22))
        )
    @@ TestAspect.withLiveSystem,
    test("interrupting a version probe stops the node and what it started"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt(
            slowNode(root.resolve("homes").resolve("v22.14.0"), root)
          )
          fiber <- NodeProbe.homes(root.resolve("homes")).fork
          leader <- awaitPid(root.resolve("leader.pid"))
          child <- awaitPid(root.resolve("child.pid"))
          done <- fiber.interrupt.timeout(5.seconds)
          dead <- ZIO.foreach(leader.toList ++ child.toList)(awaitDead)
        yield assertTrue(
          done.exists(_.isInterrupted),
          leader.nonEmpty,
          child.nonEmpty,
          dead == List(true, true)
        )
    @@ TestAspect.withLiveClock,
    test("file and HTTPS zips share staging and traversal checks"):
      ZIO.scoped:
        for
          root <- temp
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          body = scriptBytes("v22.14.0")
          zip <- zipOf("node/bin/node" -> body)
          file = root.resolve("node.zip")
          _ <- ZIO.attempt(Files.write(file, zip))
          sha = Sha256.of(Chunk.fromArray(zip))
          bad <- zipOf("../outside" -> body)
          _ <- ZIO.attempt(Files.write(root.resolve("bad.zip"), bad))
          staged <- ZIO
            .fromEither(
              RuntimeLockFixture.node(v22, ArchiveSource.Local(file), sha)
            )
            .flatMap(lock => NodeStage.stage(lock, at(root)))
            .either
          remoteLock <- ZIO.fromEither(
            RuntimeLockFixture.node(v22, ArchiveSource.Remote(url), sha)
          )
          remote <- NodeStage(
            ArchiveStage(archiveClient(zip), CommonsArchiveDecoder())
          )
            .stage(
              remoteLock,
              at(root.resolve("remote"))
            )
            .either
          escaped <- ZIO
            .fromEither(
              RuntimeLockFixture.node(
                v22,
                ArchiveSource.Local(root.resolve("bad.zip")),
                Sha256.of(Chunk.fromArray(bad))
              )
            )
            .flatMap(lock => NodeStage.stage(lock, at(root.resolve("escape"))))
            .either
          leaked <- escapedFile(root.resolve("escape"))
        yield assertTrue(
          staged.map(_.version) == Right(Right(v22)),
          remote.map(_.version) == Right(Right(v22)),
          escaped == Left(NodeStageError.UnsafePath("../outside")),
          !leaked
        )
  )

  private def archiveClient(bytes: Array[Byte]): Client =
    new Client:
      def batched(request: Request): IO[ClientError, Response] =
        ZIO.succeed(Response(body = Body.fromBytes(Chunk.fromArray(bytes))))
      def streaming(request: Request): ZIO[Scope, ClientError, Response] =
        batched(request)

  private def at(root: Path): Machine =
    Machine(
      Map.empty,
      root.toString,
      Os.Linux,
      histos.toolchain.Architecture.X86_64,
      root.resolve("cache"),
      root,
      None,
      Settled.No,
      NativeOrigin.Central
    )

  private def pack(argv: List[String], root: Path, env: Map[String, String]) =
    ChildProcess
      .command(argv, root.resolve("pack"), env)
      .runCollect
      .map(_.lastOption.contains(ChildEvent.Exited(ProcessExit(0))))

  private def stageFile(file: Path, machine: Machine) =
    ZIO
      .attempt(Files.readAllBytes(file))
      .flatMap: bytes =>
        ZIO
          .fromEither(
            RuntimeLockFixture.node(
              v22,
              ArchiveSource.Local(file),
              Sha256.of(Chunk.fromArray(bytes))
            )
          )
          .flatMap(lock => NodeStage.stage(lock, machine))

  /** Answers `--version` only after the `sleep` it starts, which holds its
    * stdout, exits.
    */
  private def slowNode(home: Path, root: Path): Unit =
    val bin = home.resolve("bin")
    Files.createDirectories(bin)
    val node = bin.resolve("node")
    Files.writeString(
      node,
      s"""#!/bin/sh
         |echo $$$$ > '$root/leader.pid'
         |sleep 30 &
         |echo $$! > '$root/child.pid'
         |wait
         |printf '%s\n' 'v22.14.0'
         |""".stripMargin
    )
    Files.setPosixFilePermissions(
      node,
      PosixFilePermissions.fromString("rwxr-xr-x")
    )

  private def awaitPid(path: Path): IO[IoFailure, Option[Long]] =
    readPid(path)
      .flatMap:
        case Some(pid) => ZIO.succeed(Some(pid))
        case None      => ZIO.sleep(20.millis) *> awaitPid(path)
      .timeout(5.seconds)
      .map(_.flatten)

  private def readPid(path: Path): IO[IoFailure, Option[Long]] =
    RuntimeFiles
      .readIfPresent(path)
      .map(_.flatMap(_.trim.toLongOption.filter(_ > 0)))

  private def awaitDead(pid: Long): UIO[Boolean] =
    ZIO
      .suspendSucceed {
        if ProcessHandle.of(pid).toScala.exists(_.isAlive) then
          ZIO.sleep(20.millis) *> awaitDead(pid)
        else ZIO.succeed(true)
      }
      .timeout(1.second)
      .map(_.getOrElse(false))

  private def writeLock(
      root: Path,
      source: ArchiveSource,
      sha: Sha256
  ): IO[IoFailure | ToolLockError[Nothing], Unit] =
    for
      lock <- ZIO.fromEither(RuntimeLockFixture.node(v22, source, sha))
      _ <- Workspace.writeString(
        root.resolve(".histos/node.lock"),
        NodeLock.render(lock)
      )
    yield ()

  private def script(home: Path, version: String): Path =
    val bin = home.resolve("bin")
    Files.createDirectories(bin)
    val node = bin.resolve("node")
    Files.write(node, scriptBytes(version))
    node.toFile.setExecutable(true)
    home

  private def scriptBytes(version: String): Array[Byte] =
    s"#!/bin/sh\nprintf '%s\\n' '$version'\n".getBytes(StandardCharsets.UTF_8)

  private def names(dir: Path): IO[IoFailure, List[String]] =
    RuntimeFiles
      .paths(dir, 1)
      .map(
        _.filterNot(_ == dir)
          .flatMap(path => Option(path.getFileName).map(_.toString))
          .sorted
      )

  private def escapedFile(root: Path): IO[IoFailure, Boolean] =
    RuntimeFiles
      .paths(root, Int.MaxValue)
      .map(
        _.exists(path =>
          Option(path.getFileName).exists(_.toString == "outside")
        )
      )

  private def zipOf(
      entries: (String, Array[Byte])*
  ): IO[IoFailure, Array[Byte]] =
    ArchiveFixture
      .zip(entries.map((name, bytes) => name -> Chunk.fromArray(bytes))*)
      .map(_.toArray)

  private def temp: ZIO[Scope, IoFailure, Path] = Workspace.work("histos-node")

  def spec =
    suite("node")(
      policy,
      versions,
      locks,
      asks,
      layouts
    ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(
      2.minutes
    )
