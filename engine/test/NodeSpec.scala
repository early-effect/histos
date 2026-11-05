package histos.node

import histos.jdk.{Machine, Os}
import histos.native.NativeOrigin
import histos.toolchain.{Architecture, Runtimes, RuntimeIdentity}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{NodePath, NodePathError}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import zio.{IO, ZIO}
import zio.test.*

object NodeSpec extends ZIOSpecDefault:
  def spec = suite("Node floor")(
    test("25 satisfies the floor and 22 does not"):
      val older = NodeVersion.from("22.14.0")
      val newer = NodeVersion.from("25.2.0")
      assertTrue(
        NodeToolchain.Floor.value == "25",
        newer.exists(found =>
          histos.toolchain.VersionOrder.atLeast(found.value, NodeToolchain.Floor.value, stripJavaOne = false)
        ),
        older.exists(found =>
          !histos.toolchain.VersionOrder.atLeast(found.value, NodeToolchain.Floor.value, stripJavaOne = false)
        ),
      )
    ,
    test("a present Node 25 is accepted and nothing is fetched"):
      ZIO.scoped:
        for
          root      <- Workspace.work("histos-node-floor")
          home      <- node(root.resolve("home"), "v25.2.0")
          canonical <- ZIO.attemptBlocking(home.toRealPath().normalize)
          found     <- NodeToolchain.resolve(onPath(root, home), None)
          fetched   <- ZIO.attemptBlocking(
            Files.exists(root.resolve("cache").resolve("nodes"))
          )
        yield assertTrue(
          found.version.value == "25.2.0",
          (found.identity match
            case RuntimeIdentity.Local(path, _) => path == canonical
          ),
          !fetched,
        )
    ,
    test("Node 22 on PATH fails with the floor, the found version, and the source"):
      ZIO.scoped:
        for
          root      <- Workspace.work("histos-node-old")
          home      <- node(root.resolve("home"), "v22.14.0")
          canonical <- ZIO.attemptBlocking(home.toRealPath().normalize)
          result    <- NodeToolchain.resolve(onPath(root, home), None).either
          version   <- ZIO.fromEither(NodeVersion.from("22.14.0"))
        yield assertTrue(
          result == Left(NodeMiss.TooOld(NodeToolchain.Floor, version, canonical, NodeSource.Path)),
          result.left.exists(_.message == s"Node 22.14.0 from PATH at $canonical is older than 25"),
        )
    ,
    test("a missing binary fails naming PATH and the floor"):
      ZIO.scoped:
        for
          root   <- Workspace.work("histos-node-absent")
          result <- NodeToolchain.resolve(machine(root, Map.empty), None).either
        yield assertTrue(
          result == Left(NodeMiss.Absent(NodeToolchain.Floor, NodeSource.Path)),
          result.left.exists(_.message == "no Node on PATH; Histos needs 25 or newer"),
        )
    ,
    test("node.local below the floor does not fall through"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-node-local")
          good <- node(root.resolve("good"), "v25.2.0")
          bad  <- node(root.resolve("bad"), "v22.14.0")
          _    <- Workspace.writeString(
            root.resolve(".histos/node.local"),
            bad.toString + "\n",
          )
          result <- NodeToolchain
            .resolve(onPath(root, good), None)
            .either
          version <- ZIO.fromEither(NodeVersion.from("22.14.0"))
        yield assertTrue(
          result == Left(NodeMiss.TooOld(NodeToolchain.Floor, version, bad, NodeSource.Local)),
          result.left.exists(_.message == s"Node 22.14.0 from node.local at $bad is older than 25"),
        )
    ,
    test("HISTOS_NODE wins over the build, node.local, and PATH"):
      ZIO.scoped:
        for
          root      <- Workspace.work("histos-node-env")
          chosen    <- node(root.resolve("chosen"), "v25.2.0")
          other     <- node(root.resolve("other"), "v22.14.0")
          path      <- ZIO.fromEither(NodePath.from(binary(other).toString))
          canonical <- ZIO.attemptBlocking(binary(chosen).toRealPath().normalize)
          _         <- Workspace.writeString(root.resolve(".histos/node.local"), other.toString + "\n")
          found     <- NodeToolchain.resolve(
            machine(
              root,
              Map(
                "PATH"            -> other.resolve("bin").toString,
                NodeToolchain.Key -> binary(chosen).toString,
              ),
            ),
            Some(path),
          )
          runtime <- Runtimes.read(
            machine(root, Map(NodeToolchain.Key -> binary(chosen).toString, "PATH" -> other.resolve("bin").toString)),
            Some(path),
          )
          kept <- runtime.nodeRuntime
        yield assertTrue(
          found.version.value == "25.2.0",
          found.binary.path == canonical,
          kept.version.value == "25.2.0",
        )
    ,
    test("the build path wins over node.local and PATH when HISTOS_NODE is unset"):
      ZIO.scoped:
        for
          root   <- Workspace.work("histos-node-config")
          chosen <- node(root.resolve("chosen"), "v25.2.0")
          other  <- node(root.resolve("other"), "v22.14.0")
          path   <- ZIO.fromEither(NodePath.from(binary(chosen).toString))
          _      <- Workspace.writeString(root.resolve(".histos/node.local"), other.toString + "\n")
          found  <- NodeToolchain.resolve(onPath(root, other), Some(path))
        yield assertTrue(found.version.value == "25.2.0", found.binary.path.endsWith("node"))
    ,
    test("a Node 22 from HISTOS_NODE fails and does not fall through to PATH"):
      ZIO.scoped:
        for
          root      <- Workspace.work("histos-node-env-old")
          good      <- node(root.resolve("good"), "v25.2.0")
          bad       <- node(root.resolve("bad"), "v22.14.0")
          canonical <- ZIO.attemptBlocking(bad.toRealPath().normalize)
          result    <- NodeToolchain
            .resolve(
              machine(root, Map("PATH" -> good.resolve("bin").toString, NodeToolchain.Key -> binary(bad).toString)),
              None,
            )
            .either
          version <- ZIO.fromEither(NodeVersion.from("22.14.0"))
        yield assertTrue(
          result == Left(NodeMiss.TooOld(NodeToolchain.Floor, version, canonical, NodeSource.Environment)),
          result.left.exists(_.message == s"Node 22.14.0 from HISTOS_NODE at $canonical is older than 25"),
        )
    ,
    test("a Node 22 from config fails and does not fall through to PATH"):
      ZIO.scoped:
        for
          root      <- Workspace.work("histos-node-config-old")
          good      <- node(root.resolve("good"), "v25.2.0")
          bad       <- node(root.resolve("bad"), "v22.14.0")
          path      <- ZIO.fromEither(NodePath.from(binary(bad).toString))
          canonical <- ZIO.attemptBlocking(bad.toRealPath().normalize)
          result    <- NodeToolchain.resolve(onPath(root, good), Some(path)).either
          version   <- ZIO.fromEither(NodeVersion.from("22.14.0"))
        yield assertTrue(
          result == Left(NodeMiss.TooOld(NodeToolchain.Floor, version, canonical, NodeSource.Config)),
          result.left.exists(_.message == s"Node 22.14.0 from config at $canonical is older than 25"),
        )
    ,
    test("a missing or unusable HISTOS_NODE does not fall through"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-node-env-bad")
          good <- node(root.resolve("good"), "v25.2.0")
          missing = root.resolve("gone").resolve("node")
          quiet   = root.resolve("quiet").resolve("node")
          folder  = root.resolve("node")
          _      <- plain(quiet)
          _      <- Workspace.createDirectories(folder)
          absent <- NodeToolchain
            .resolve(
              machine(root, Map("PATH" -> good.resolve("bin").toString, NodeToolchain.Key -> missing.toString)),
              None,
            )
            .either
          blocked <- NodeToolchain
            .resolve(
              machine(root, Map("PATH" -> good.resolve("bin").toString, NodeToolchain.Key -> quiet.toString)),
              None,
            )
            .either
          dir <- NodeToolchain
            .resolve(
              machine(root, Map("PATH" -> good.resolve("bin").toString, NodeToolchain.Key -> folder.toString)),
              None,
            )
            .either
          relative <- NodeToolchain
            .resolve(machine(root, Map("PATH" -> good.resolve("bin").toString, NodeToolchain.Key -> "bin/node")), None)
            .either
          blank <- NodeToolchain
            .resolve(machine(root, Map("PATH" -> good.resolve("bin").toString, NodeToolchain.Key -> "   ")), None)
            .either
          named <- NodeToolchain
            .resolve(
              machine(
                root,
                Map("PATH" -> good.resolve("bin").toString, NodeToolchain.Key -> root.resolve("nodejs").toString),
              ),
              None,
            )
            .either
        yield assertTrue(
          absent == Left(NodeMiss.Unusable(missing, NodeSource.Environment, NodeFileFault.Missing)),
          absent.left.exists(_.message == s"HISTOS_NODE names $missing, which is missing"),
          blocked == Left(NodeMiss.Unusable(quiet, NodeSource.Environment, NodeFileFault.NotExecutable)),
          blocked.left.exists(_.message == s"HISTOS_NODE names $quiet, which is not executable"),
          dir == Left(NodeMiss.Unusable(folder, NodeSource.Environment, NodeFileFault.NotAFile)),
          relative == Left(NodeMiss.Refused(NodeSource.Environment, NodePathError.Relative("bin/node"))),
          relative.left.exists(_.message == "HISTOS_NODE is not absolute: bin/node"),
          blank == Left(NodeMiss.Refused(NodeSource.Environment, NodePathError.Empty)),
          blank.left.exists(_.message == "HISTOS_NODE is empty"),
          named == Left(
            NodeMiss.Refused(NodeSource.Environment, NodePathError.NotBinary(root.resolve("nodejs").toString))
          ),
        )
    ,
    test("a missing or non-executable config path does not fall through"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-node-config-bad")
          good <- node(root.resolve("good"), "v25.2.0")
          missing = root.resolve("gone").resolve("node")
          quiet   = root.resolve("quiet").resolve("node")
          gone    <- ZIO.fromEither(NodePath.from(missing.toString))
          still   <- ZIO.fromEither(NodePath.from(quiet.toString))
          _       <- plain(quiet)
          absent  <- NodeToolchain.resolve(onPath(root, good), Some(gone)).either
          blocked <- NodeToolchain.resolve(onPath(root, good), Some(still)).either
        yield assertTrue(
          absent == Left(NodeMiss.Unusable(missing, NodeSource.Config, NodeFileFault.Missing)),
          absent.left.exists(_.message == s"config names $missing, which is missing"),
          blocked == Left(NodeMiss.Unusable(quiet, NodeSource.Config, NodeFileFault.NotExecutable)),
          blocked.left.exists(_.message == s"config names $quiet, which is not executable"),
        )
    ,
    test("whitespace around HISTOS_NODE is the same binary"):
      ZIO.scoped:
        for
          root  <- Workspace.work("histos-node-env-space")
          home  <- node(root.resolve("home"), "v25.2.0")
          found <- NodeToolchain.resolve(
            machine(root, Map(NodeToolchain.Key -> s"  ${binary(home)}  ")),
            None,
          )
        yield assertTrue(found.version.value == "25.2.0"),
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem

  private def machine(root: Path, env: Map[String, String]): Machine =
    Machine(
      env,
      root.toString,
      Os.Linux,
      Architecture.X86_64,
      root.resolve("cache"),
      root,
      None,
      NativeOrigin.Central,
    )

  private def onPath(root: Path, home: Path): Machine =
    machine(root, Map("PATH" -> home.resolve("bin").toString))

  private def binary(home: Path): Path =
    home.resolve("bin").resolve("node")

  private def node(home: Path, version: String): IO[IoFailure, Path] =
    val file = binary(home)
    for
      _ <- Workspace.createDirectories(file.getParent)
      _ <- Workspace.writeString(file, s"#!/bin/sh\nprintf '%s\\n' '$version'\n")
      _ <- executable(file)
    yield home

  private def plain(file: Path): IO[IoFailure, Unit] =
    for
      _ <- Workspace.createDirectories(file.getParent)
      _ <- Workspace.writeString(file, "#!/bin/sh\nprintf '%s\\n' 'v25.2.0'\n")
      _ <- ZIO
        .attemptBlocking(Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rw-r--r--")))
        .mapError(_ => IoFailure(file, IoFault.Write))
    yield ()

  private def executable(file: Path): IO[IoFailure, Unit] =
    ZIO
      .attemptBlocking(Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rwxr-xr-x")))
      .mapError(_ => IoFailure(file, IoFault.Write))
      .unit
end NodeSpec
