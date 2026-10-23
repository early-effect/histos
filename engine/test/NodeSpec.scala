package histos.node

import histos.jdk.{ArchiveSource, HttpsUrl, Machine, Os, Settled, Sha256}
import histos.testrun.{ProcessExit, TestEvent, TestProcess}
import histos.{Module, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.{Chunk, Scope, UIO, ZIO, durationInt}
import zio.test.*

object NodeSpec extends ZIOSpecDefault:
  private val major22 = NodeMajor.trusted(22)
  private val major20 = NodeMajor.trusted(20)
  private val v22     = NodeVersion.trusted("22.14.0", major22)
  private val v20     = NodeVersion.trusted("20.11.1", major20)

  private def home(path: String, version: NodeVersion): NodeHome =
    NodeHome(Path.of(path), hasBinary = true, version = Right(version))

  private val bare =
    NodeView(NodeLocated.Unset, NodeLocated.Unset, NodeManagerSlot.Absent, NodeManagerSlot.Absent, NodeManagerSlot.Absent, Nil)

  private val policy = suite("resolution")(
    test("a node on PATH of the pinned major is chosen before nvm"):
      val view = bare.copy(
        onPath = NodeLocated.Found(home("/opt/node", v22)),
        nvm = NodeManagerSlot.Present(List(home("/nvm/22", v22))),
      )
      assertTrue:
        Node.resolve(major22, view) match
          case NodeResolution.Selected(chosen, _) =>
            chosen.origin == NodeOrigin.Path && chosen.home == Path.of("/opt/node")
          case _ => false
    ,
    test("the wrong node on PATH falls through to fnm when nvm is absent"):
      val view = bare.copy(
        onPath = NodeLocated.Found(home("/opt/node", v20)),
        fnm = NodeManagerSlot.Present(List(home("/fnm/22", v22))),
      )
      assertTrue:
        Node.resolve(major22, view) match
          case NodeResolution.Selected(chosen, _) => chosen.origin == NodeOrigin.Fnm
          case _                                  => false
    ,
    test("a manager without the pin does not use the cache"):
      val view = bare.copy(
        nvm = NodeManagerSlot.Present(List(home("/nvm/20", v20))),
        cached = List(home("/cache/22", v22)),
      )
      assertTrue:
        Node.resolve(major22, view) match
          case NodeResolution.Install(NodeInstall.Nvm(major), _) => major == major22
          case _                                                 => false
    ,
    test("nvm is asked before fnm when no manager has the pin"):
      val view = bare.copy(
        nvm = NodeManagerSlot.Present(List(home("/nvm/20", v20))),
        fnm = NodeManagerSlot.Present(Nil),
        volta = NodeManagerSlot.Present(List(home("/volta/20", v20))),
        cached = List(home("/cache/22", v22)),
      )
      assertTrue:
        Node.resolve(major22, view) match
          case NodeResolution.Install(NodeInstall.Nvm(major), _) => major == major22
          case _                                                 => false
    ,
    test("volta is used when it is the only manager with the pin"):
      val view = bare.copy(
        volta = NodeManagerSlot.Present(List(home("/volta/22", v22))),
      )
      assertTrue:
        Node.resolve(major22, view) match
          case NodeResolution.Selected(chosen, _) =>
            chosen.origin == NodeOrigin.Volta && chosen.version == v22
          case _ => false
    ,
    test("a bare machine provisions and does not invent a manager"):
      assertTrue:
        Node.resolve(major22, bare.copy(cached = Nil)) match
          case NodeResolution.Provision(major, _) => major == major22
          case _                                  => false
    ,
    test("the cache is used only when no manager is installed"):
      val view = bare.copy(cached = List(home("/cache/22", v22)))
      assertTrue:
        Node.resolve(major22, view) match
          case NodeResolution.Selected(chosen, _) => chosen.origin == NodeOrigin.Cache
          case _                                  => false
    ,
    test("a wrong explicit override stops"):
      val view = bare.copy(
        explicit = NodeLocated.Found(home("/override/20", v20)),
        onPath = NodeLocated.Found(home("/opt/node", v22)),
      )
      assertTrue:
        Node.resolve(major22, view) match
          case NodeResolution.OverrideRejected(NodeReject.WrongMajor(found), _) => found == major20
          case _                                                                => false
    ,
  )

  private val versions = suite("versions")(
    test("node --version drops the leading v, and a major is not a pin"):
      assertTrue(
        NodeVersion.from("v22.14.0") == Right(v22),
        NodeVersion.from("22.14.0") == Right(v22),
        NodeVersion.from("22") == Left(NodeVersionError.Bad("22")),
        NodeVersion.from("0.1.0") == Left(NodeVersionError.Bad("0.1.0")),
        NodeMajor.from(0) == Left(NodeError.BadMajor(0)),
      )
  )

  private val locks = suite("node lock")(
    test("a file url round-trips and the major has to agree"):
      val sha  = Sha256.of(Chunk.fromArray("node".getBytes(StandardCharsets.UTF_8)))
      val lock = NodeLock(major22, v22, ArchiveSource.Local(Path.of("/opt/node.zip")), sha)
      val again = NodeLock.parse(NodeLock.render(lock))
      val wrong =
        """major 22
          |version 20.11.1
          |url file:///opt/node.zip
          |sha256 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
          |""".stripMargin
      assertTrue(
        again == Right(lock),
        NodeLock.parse(wrong) == Left(NodeLockError.MajorDisagrees(major22, major20)),
        NodeLock.parse("") == Left(NodeLockError.Empty),
        NodeLock.parse("nope") == Left(NodeLockError.BadLine("nope")),
        NodeLock.parse(
          """major 22
            |version 22.14.0
            |url http://example.test/node.zip
            |sha256 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
            |""".stripMargin,
        ) == Left(NodeLockError.BadUrl("http://example.test/node.zip")),
      )
  )

  private val asks = suite("when node is required")(
    test("only a js module asks for node"):
      val named = ProjectName.from("core").map: name =>
        def one(platform: Platform) =
          ProjectGraph.trusted(List(Module(
            name,
            Platforms(platform, Nil),
            ScalaVersions(ScalaVersion.trusted("3.9.0"), Nil),
            Nil,
            Nil,
            SourcePos("histos.scala", 1, 1),
          )))
        (Node.required(one(Platform.Jvm)), Node.required(one(Platform.Js)))
      assertTrue(named == Right((false, true)))
  )

  private val layouts = suite("fixture node")(
    test("nvm's node is read from the script and a jvm home is not searched"):
      ZIO.scoped:
        for
          root <- temp
          nvm  = root.resolve(".nvm").resolve("versions").resolve("node").resolve("v22.14.0")
          _    <- ZIO.attempt(script(nvm, "v22.14.0"))
          machine = Machine(
                      Map("NVM_DIR" -> root.resolve(".nvm").toString),
                      root.toString,
                      Os.Linux,
                      root.resolve("cache"),
                      root,
                      None,
                      Settled.No,
                    )
          url  <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          _    <- ZIO.attempt(writeLock(root, ArchiveSource.Remote(url), Sha256.of(Chunk.fromArray("x".getBytes))))
          found <- NodeToolchain.explain(machine)
          text = NodeToolchain.text(found)
        yield assertTrue(text.contains("chosen nvm 22.14.0"), text.contains(nvm.toString), !text.contains("java"))
    ,
    test("a probe leaves a node's mode alone, and a node that cannot run is not chosen"):
      ZIO.scoped:
        for
          root   <- temp
          nvm    = root.resolve(".nvm").resolve("versions").resolve("node").resolve("v22.14.0")
          node   = nvm.resolve("bin").resolve("node")
          _      <- ZIO.attempt(script(nvm, "v22.14.0"))
          _      <- ZIO.attempt(Files.setPosixFilePermissions(node, PosixFilePermissions.fromString("rw-r--r--")))
          before <- ZIO.attempt(Files.getPosixFilePermissions(node).asScala.toSet)
          machine = Machine(Map("NVM_DIR" -> root.resolve(".nvm").toString), root.toString, Os.Linux, root.resolve("cache"), root, None, Settled.No)
          url    <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          _      <- ZIO.attempt(writeLock(root, ArchiveSource.Remote(url), Sha256.of(Chunk.fromArray("x".getBytes))))
          found  <- NodeToolchain.explain(machine)
          after  <- ZIO.attempt(Files.getPosixFilePermissions(node).asScala.toSet)
        yield assertTrue(
          after == before,
          found.resolution == NodeResolution.Install(
            NodeInstall.Nvm(major22),
            List(
              NodeCandidate.NotPresent(NodeOrigin.Override),
              NodeCandidate.NotPresent(NodeOrigin.Path),
              NodeCandidate.Rejected(NodeOrigin.Nvm, NodeReject.UnreadableVersion(NodeVersionError.NotExecutable(node))),
              NodeCandidate.NotPresent(NodeOrigin.Fnm),
              NodeCandidate.NotPresent(NodeOrigin.Volta),
            ),
          ),
        )
    ,
    test("a failed node unpack leaves no partial directory and no temp file"):
      ZIO.scoped:
        for
          root <- temp
          bad  = zipOf("node/bin/node" -> scriptBytes("v22.14.0"), "../outside" -> scriptBytes("v22.14.0"))
          file = root.resolve("bad.zip")
          _    <- ZIO.attempt(Files.write(file, bad))
          sha  = Sha256.of(Chunk.fromArray(bad))
          out  <- NodeStage.stage(NodeLock(major22, v22, ArchiveSource.Local(file), sha), at(root)).either
          left <- ZIO.attempt(names(root.resolve("cache").resolve("nodes").resolve(sha.value)))
        yield assertTrue(out == Left(NodeStageError.UnsafePath("../outside")), left == List("archive"))
    ,
    test("a local tar.gz or tar.xz node stages with a bin/node that runs"):
      ZIO.scoped:
        for
          root  <- temp
          env   <- zio.System.envs
          tree  = root.resolve("tree")
          _     <- ZIO.attempt {
                     script(tree.resolve("node-v22.14.0-darwin-arm64"), "v22.14.0")
                     Files.createDirectories(root.resolve("pack"))
                   }
          gz    = root.resolve("node.tar.gz")
          xz    = root.resolve("node.tar.xz")
          gzPacked <- pack(List("tar", "-czf", gz.toString, "-C", tree.toString, "node-v22.14.0-darwin-arm64"), root, env)
          xzPacked <- pack(List("tar", "-cJf", xz.toString, "-C", tree.toString, "node-v22.14.0-darwin-arm64"), root, env)
          machine = at(root).copy(env = env)
          gzHome <- stageFile(gz, machine)
          xzHome <- ZIO.when(xzPacked)(stageFile(xz, machine))
          runs   <- ZIO.attempt(Files.isExecutable(gzHome.path.resolve("bin").resolve("node")))
        yield assertTrue(
          gzPacked,
          gzHome.version == Right(v22),
          runs,
          xzHome.forall(_.version == Right(v22)),
        )
    @@ TestAspect.withLiveSystem
    ,
    test("interrupting a version probe stops the node and what it started"):
      ZIO.scoped:
        for
          root   <- temp
          _      <- ZIO.attempt(slowNode(root.resolve("homes").resolve("v22.14.0"), root))
          fiber  <- NodeProbe.homes(root.resolve("homes")).fork
          leader <- awaitPid(root.resolve("leader.pid"))
          child  <- awaitPid(root.resolve("child.pid"))
          done   <- fiber.interrupt.timeout(5.seconds)
          dead   <- ZIO.foreach(leader.toList ++ child.toList)(awaitDead)
        yield assertTrue(done.exists(_.isInterrupted), leader.nonEmpty, child.nonEmpty, dead == List(true, true))
    @@ TestAspect.withLiveClock
    ,
    test("a file zip is staged and an https lock is not fetched"):
      ZIO.scoped:
        for
          root <- temp
          url  <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          body = scriptBytes("v22.14.0")
          zip  = zipOf("node/bin/node" -> body)
          file = root.resolve("node.zip")
          _    <- ZIO.attempt(Files.write(file, zip))
          sha  = Sha256.of(Chunk.fromArray(zip))
          bad  = zipOf("../outside" -> body)
          _    <- ZIO.attempt(Files.write(root.resolve("bad.zip"), bad))
          staged <- NodeStage.stage(NodeLock(major22, v22, ArchiveSource.Local(file), sha), at(root)).either
          remote <- NodeStage.stage(NodeLock(major22, v22, ArchiveSource.Remote(url), sha), at(root.resolve("remote"))).either
          escaped <- NodeStage.stage(
                       NodeLock(major22, v22, ArchiveSource.Local(root.resolve("bad.zip")), Sha256.of(Chunk.fromArray(bad))),
                       at(root.resolve("escape")),
                     ).either
          leaked = escapedFile(root.resolve("escape"))
        yield assertTrue(
          staged.map(_.version) == Right(Right(v22)),
          remote == Left(NodeStageError.Remote(url)),
          !Files.exists(root.resolve("remote").resolve("cache")),
          escaped == Left(NodeStageError.UnsafePath("../outside")),
          !leaked,
        )
  )

  private def at(root: Path): Machine =
    Machine(Map.empty, root.toString, Os.Linux, root.resolve("cache"), root, None, Settled.No)

  private def pack(argv: List[String], root: Path, env: Map[String, String]) =
    TestProcess.command(argv, root.resolve("pack"), env).runCollect.map(_.lastOption.contains(TestEvent.Exited(ProcessExit(0))))

  private def stageFile(file: Path, machine: Machine) =
    ZIO.attempt(Files.readAllBytes(file)).flatMap: bytes =>
      NodeStage.stage(NodeLock(major22, v22, ArchiveSource.Local(file), Sha256.of(Chunk.fromArray(bytes))), machine)

  /** Answers `--version` only after the `sleep` it starts, which holds its stdout, exits. */
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
         |""".stripMargin,
    )
    Files.setPosixFilePermissions(node, PosixFilePermissions.fromString("rwxr-xr-x"))

  private def awaitPid(path: Path): UIO[Option[Long]] =
    ZIO.suspendSucceed {
      readPid(path) match
        case Some(pid) => ZIO.succeed(Some(pid))
        case None      => ZIO.sleep(20.millis) *> awaitPid(path)
    }.timeout(5.seconds).map(_.flatten)

  private def readPid(path: Path): Option[Long] =
    if !Files.isRegularFile(path) then None
    else
      try Files.readString(path).trim.toLongOption.filter(_ > 0)
      catch case _: java.io.IOException => None

  private def awaitDead(pid: Long): UIO[Boolean] =
    ZIO.suspendSucceed {
      if ProcessHandle.of(pid).toScala.exists(_.isAlive) then ZIO.sleep(20.millis) *> awaitDead(pid) else ZIO.succeed(true)
    }.timeout(1.second).map(_.getOrElse(false))

  private def writeLock(root: Path, source: ArchiveSource, sha: Sha256): Unit =
    Files.createDirectories(root.resolve(".histos"))
    Files.writeString(root.resolve(".histos").resolve("node.lock"), NodeLock.render(NodeLock(major22, v22, source, sha)))

  private def script(home: Path, version: String): Path =
    val bin = home.resolve("bin")
    Files.createDirectories(bin)
    val node = bin.resolve("node")
    Files.write(node, scriptBytes(version))
    node.toFile.setExecutable(true)
    home

  private def scriptBytes(version: String): Array[Byte] =
    s"#!/bin/sh\nprintf '%s\\n' '$version'\n".getBytes(StandardCharsets.UTF_8)

  private def names(dir: Path): List[String] =
    if !Files.isDirectory(dir) then Nil
    else
      val listed = Files.list(dir)
      try listed.iterator.asScala.flatMap(path => Option(path.getFileName).map(_.toString)).toList.sorted
      finally listed.close()

  private def escapedFile(root: Path): Boolean =
    if !Files.exists(root) then false
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.exists(path => Option(path.getFileName).exists(_.toString == "outside"))
      finally walk.close()

  private def zipOf(entries: (String, Array[Byte])*): Array[Byte] =
    val buffer = new java.io.ByteArrayOutputStream
    val output = new java.util.zip.ZipOutputStream(buffer)
    val crc    = new java.util.zip.CRC32
    entries.foreach: (name, body) =>
      crc.reset()
      crc.update(body)
      val entry = new java.util.zip.ZipEntry(name)
      entry.setMethod(java.util.zip.ZipEntry.STORED)
      entry.setSize(body.length.toLong)
      entry.setCrc(crc.getValue)
      output.putNextEntry(entry)
      output.write(body)
      output.closeEntry()
    output.close()
    buffer.toByteArray

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-node"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("node")(policy, versions, locks, asks, layouts) @@ TestAspect.timeout(2.minutes)
