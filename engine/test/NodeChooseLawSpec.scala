package histos.node

import histos.jdk.{Machine, Os}
import histos.native.NativeOrigin
import histos.toolchain.Architecture
import histos.workspace.Workspace
import histos.NodePath

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import zio.{Exit, ZIO}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, test}

/** A chosen Node is the binary that answered, and a named file that cannot run is that miss. */
object NodeChooseLawSpec extends ZIOSpecDefault:
  def spec = suite("node choose laws")(
    test("a binary on PATH is that Node, and a named file that cannot run is that miss"):
      ZIO.scoped:
        Workspace
          .work("histos-node-choose")
          .flatMap: repo =>
            val oldHome  = repo.resolve("old")
            val goodHome = repo.resolve("good")
            val badHome  = repo.resolve("bad")
            val sight    = repo.resolve("sight")
            val quiet    = repo.resolve("quiet")
            val plain    = repo.resolve("plain").resolve("node")
            val folder   = repo.resolve("notfile").resolve("node")
            val local    = repo.resolve(".histos").resolve("node.local")
            val oldNode  = oldHome.resolve("bin").resolve("node")
            val badNode  = badHome.resolve("bin").resolve("node")
            val path     = goodHome.resolve("bin").toString
            for
              _    <- script(oldNode, "v20.0.0")
              _    <- script(goodHome.resolve("bin").resolve("node"), "v26.0.0")
              _    <- script(badNode, "nope")
              _    <- script(sight.resolve("node"), "v26.0.0")
              _    <- Workspace.createDirectories(quiet)
              _    <- write(plain, "#!/bin/sh\necho v26.0.0\n", executable = false)
              _    <- Workspace.createDirectories(folder)
              live <- NodeToolchain.choose(machine(repo, Map("PATH" -> path)), None)
              told <- NodeToolchain.explain(machine(repo, Map("PATH" -> path)), None)
              seen <- NodeToolchain.sights(
                machine(repo, Map("PATH" -> s"${sight}:${sight}:${quiet}:${plain.getParent}"))
              )
              sightNode <- ZIO.attempt(sight.resolve("node").toRealPath().normalize).orDie
              named     <- NodeToolchain.choose(
                machine(repo, Map("HISTOS_NODE" -> live.runtime.binary.path.toString)),
                None,
              )
              old        <- NodeToolchain.choose(machine(repo, Map("HISTOS_NODE" -> oldNode.toString)), None).exit
              oldShown   <- shown(old)
              oldReal    <- ZIO.attempt(oldHome.toRealPath().normalize).orDie
              garbled    <- NodeToolchain.choose(machine(repo, Map("HISTOS_NODE" -> badNode.toString)), None).exit
              garbledAt  <- ZIO.attempt(badNode.toRealPath().normalize).orDie
              locked     <- NodeToolchain.choose(machine(repo, Map("HISTOS_NODE" -> plain.toString)), None).exit
              nested     <- NodeToolchain.choose(machine(repo, Map("HISTOS_NODE" -> folder.toString)), None).exit
              _          <- Workspace.writeString(local, s"${goodHome.toAbsolutePath.normalize}\n")
              kept       <- NodeToolchain.choose(machine(repo, Map("PATH" -> quiet.toString)), None)
              _          <- Workspace.writeString(local, "node\n")
              relative   <- NodeToolchain.choose(machine(repo, Map.empty), None).exit
              _          <- Workspace.writeString(local, "/usr\n/bin\n")
              several    <- NodeToolchain.choose(machine(repo, Map.empty), None).exit
              _          <- Workspace.writeString(local, "\u0000\n")
              invalid    <- NodeToolchain.choose(machine(repo, Map.empty), None).exit
              node       <- ZIO.fromEither(NodePath.from(live.runtime.binary.path.toString))
              configured <- NodeToolchain.choose(machine(repo, Map("PATH" -> quiet.toString)), Some(node))
              goodNode   <- ZIO.attempt(goodHome.resolve("bin").resolve("node").toRealPath().normalize).orDie
            yield
              val absent = NodeMiss.Absent(NodeToolchain.Floor, NodeSource.Path)
              assertTrue(
                live.source == NodeSource.Path,
                live.runtime.version == NodeVersion.trusted("26.0.0", NodeMajor.trusted(26)),
                live.runtime.binary.path == goodNode,
                told.binary == live.runtime.binary,
                told.version == live.runtime.version,
                NodeToolchain.text(told) == s"node ${told.version.value} ${told.binary.path}",
                NodeToolchain.origin(NodeSource.Environment) == "HISTOS_NODE",
                NodeToolchain.origin(NodeSource.Config) == "runnerTools.nodePath",
                NodeToolchain.origin(NodeSource.Path) == "PATH",
                NodeToolchain.origin(NodeSource.Local) == ".histos/node.local",
                NodeToolchain.failure(absent) == s"${absent.message}. ${NodeToolchain.guidance}",
                named.source == NodeSource.Environment,
                named.runtime.version == live.runtime.version,
                configured.source == NodeSource.Config,
                configured.runtime.binary.path == live.runtime.binary.path,
                oldShown == s"old:${NodeToolchain.Floor.value}:20.0.0:$oldReal:${NodeSource.Environment}",
                garbled == Exit.fail(
                  NodeMiss.Rejected(
                    garbledAt,
                    NodeReject.UnreadableVersion(NodeVersionError.Bad("nope")),
                    NodeSource.Environment,
                  )
                ),
                locked == Exit.fail(NodeMiss.Unusable(plain, NodeSource.Environment, NodeFileFault.NotExecutable)),
                nested == Exit.fail(NodeMiss.Unusable(folder, NodeSource.Environment, NodeFileFault.NotAFile)),
                kept.source == NodeSource.Local,
                kept.runtime.version == NodeVersion.trusted("26.0.0", NodeMajor.trusted(26)),
                relative == Exit.fail(NodeMiss.Unresolved(NodeError.Override(NodeOverrideError.Relative("node")))),
                several == Exit.fail(NodeMiss.Unresolved(NodeError.Override(NodeOverrideError.Multiple))),
                invalid == Exit.fail(NodeMiss.Unresolved(NodeError.Override(NodeOverrideError.Invalid("\u0000")))),
                seen.map(_.binary) == List(sightNode),
                seen.map(_.version) == List(NodeVersion.trusted("26.0.0", NodeMajor.trusted(26))),
              )
  )
    // The `node --version` child is launched with the running JVM's java.home, and a PATH sight bounds it with a
    // timeout. Every PATH the law reads is one it wrote.
    @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock

  private def machine(repo: Path, env: Map[String, String]): Machine =
    Machine(env, "/home", Os.Mac, Architecture.Aarch64, repo, repo, None, NativeOrigin.Central)

  private def shown(exit: Exit[NodeMiss, NodeChoice]): ZIO[Any, Nothing, String] =
    exit match
      case Exit.Success(_)     => ZIO.succeed("ok")
      case Exit.Failure(cause) =>
        cause.failureOption match
          case Some(NodeMiss.TooOld(required, found, home, source)) =>
            ZIO
              .attempt(home.toRealPath().normalize)
              .orDie
              .map(real => s"old:${required.value}:${found.value}:$real:$source")
          case Some(other) => ZIO.succeed(other.toString)
          case None        => ZIO.succeed("empty")

  private def script(path: Path, text: String): ZIO[Any, Nothing, Unit] =
    write(path, s"#!/bin/sh\necho $text\n", executable = true)

  private def write(path: Path, text: String, executable: Boolean): ZIO[Any, Nothing, Unit] =
    ZIO
      .attempt:
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, text)
        val mode = if executable then "rwxr-xr-x" else "rw-r--r--"
        Files.setPosixFilePermissions(path, PosixFilePermissions.fromString(mode))
        ()
      .orDie
end NodeChooseLawSpec
