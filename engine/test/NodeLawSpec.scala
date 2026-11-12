package histos.node

import histos.jdk.{Machine, Os}
import histos.native.NativeOrigin
import histos.toolchain.Architecture
import histos.workspace.Workspace
import histos.{
  Module,
  NodePathError,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import java.nio.file.Path

import zio.{Exit, ZIO}
import zio.test.*

object NodeLawSpec extends ZIOSpecDefault:
  def spec = suite("node laws")(
    test("a full version keeps its major, a leading v is stripped, and zero or a fragment is bad"):
      assertTrue(
        NodeVersion.from("v25.1.2") == Right(NodeVersion.trusted("25.1.2", NodeMajor.trusted(25))),
        NodeVersion.from("V1.2.3") == Right(NodeVersion.trusted("1.2.3", NodeMajor.trusted(1))),
        NodeVersion.from("0.1.0") == Left(NodeVersionError.Bad("0.1.0")),
        NodeVersion.from("25") == Left(NodeVersionError.Bad("25")),
        NodeVersion.from("v25.0.0-rc.1") == Right(NodeVersion.trusted("25.0.0-rc.1", NodeMajor.trusted(25))),
        NodeVersion.from("v26.0.0-nightly20260101abc+7") ==
          Right(NodeVersion.trusted("26.0.0-nightly20260101abc+7", NodeMajor.trusted(26))),
        NodeVersion.from("25.0.0-") == Left(NodeVersionError.Bad("25.0.0-")),
        NodeVersion.from("25.0.0 rc") == Left(NodeVersionError.Bad("25.0.0 rc")),
        NodeMajor.from(0) == Left(NodeError.BadMajor(0)),
        Node.required(ProjectGraph.trusted(List(module(Platform.Jvm)))) == false,
        Node.required(ProjectGraph.trusted(List(module(Platform.Js)))) == true,
      )
    ,
    test("a shadow is a later newer binary, and a floor that is already met is none"):
      val old    = sight("/old/bin/node", "20.0.0", 20)
      val middle = sight("/mid/bin/node", "24.0.0", 24)
      val next   = sight("/new/bin/node", "26.0.0", 26)
      val floor  = sight("/floor/bin/node", "25.0.0", 25)
      assertTrue(
        NodeToolchain.shadow(List(old, middle, next)) == Some(NodeShadow(old, next)),
        NodeToolchain.shadow(List(floor, next)) == None,
        NodeToolchain.shadow(Nil) == None,
        NodeToolchain.shadow(List(old)) == None,
        NodeToolchain.Floor.major.value == 25,
      )
    ,
    test("HISTOS_NODE wins, a relative or non-node path is refused, and a missing binary is unusable"):
      ZIO.scoped:
        Workspace
          .work("histos-node")
          .flatMap: repo =>
            for
              relative <- NodeToolchain.choose(machine(repo, Map("HISTOS_NODE" -> "node")), None).exit
              binary   <- NodeToolchain.choose(machine(repo, Map("HISTOS_NODE" -> "/usr/bin/java")), None).exit
              missing  <- NodeToolchain.choose(machine(repo, Map("HISTOS_NODE" -> "/no/such/node")), None).exit
            yield assertTrue(
              relative match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(
                    NodeMiss.Refused(NodeSource.Environment, NodePathError.Relative("node"))
                  )
                case Exit.Success(_) => false
              ,
              binary match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(
                    NodeMiss.Refused(NodeSource.Environment, NodePathError.NotBinary("/usr/bin/java"))
                  )
                case Exit.Success(_) => false
              ,
              missing match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(
                    NodeMiss.Unusable(Path.of("/no/such/node"), NodeSource.Environment, NodeFileFault.Missing)
                  )
                case Exit.Success(_) => false,
            )
    ,
    test("no override and an empty path is an absent node, and a directory without a binary is missing"):
      ZIO.scoped:
        Workspace
          .work("histos-node-path")
          .flatMap: repo =>
            val empty = repo.resolve("empty-path")
            for
              _      <- Workspace.createDirectories(empty)
              absent <- NodeToolchain.choose(machine(repo, Map("PATH" -> empty.toString)), None).exit
              home   <- NodeProbe.home(repo.resolve("home"))
              binary <- NodeProbe.binary(repo.resolve("home")).exit
              _      <- Workspace.writeString(repo.resolve(".histos").resolve("node.local"), "")
              local  <- NodeToolchain.choose(machine(repo, Map.empty), None).exit
            yield assertTrue(
              absent match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(NodeMiss.Absent(NodeToolchain.Floor, NodeSource.Path))
                case Exit.Success(_) => false
              ,
              !home.hasBinary,
              home.version == Left(NodeVersionError.Missing),
              binary match
                case Exit.Failure(cause) =>
                  cause.failureOption match
                    case Some(NodeError.Version(_, NodeVersionError.Missing)) => true
                    case _                                                    => false
                case Exit.Success(_) => false
              ,
              local match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(
                    NodeMiss.Unresolved(NodeError.Override(NodeOverrideError.Empty))
                  )
                case Exit.Success(_) => false,
            ),
  )

  private def module(platform: Platform): Module =
    Module(
      ProjectName.trusted("core"),
      Platforms(platform, Nil),
      ScalaVersions(ScalaVersion.trusted("3.3.7"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )

  private def sight(path: String, version: String, major: Int): NodeSight =
    NodeSight(Path.of(path), NodeVersion.trusted(version, NodeMajor.trusted(major)))

  private def machine(repo: Path, env: Map[String, String]): Machine =
    Machine(env, "/home", Os.Mac, Architecture.Aarch64, repo, repo, None, NativeOrigin.Central)
end NodeLawSpec
