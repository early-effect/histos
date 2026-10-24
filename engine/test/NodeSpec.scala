package histos.node

import histos.jdk.{Machine, Os}
import histos.native.NativeOrigin
import histos.toolchain.{Architecture, RuntimeIdentity}
import histos.workspace.{IoFailure, IoFault, Workspace}

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
        newer.exists(_.atLeast(NodeToolchain.Floor)),
        older.exists(!_.atLeast(NodeToolchain.Floor))
      )
    ,
    test("a present Node 25 is accepted and nothing is fetched"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-node-floor")
          home <- node(root.resolve("home"), "v25.2.0")
          canonical <- ZIO.attemptBlocking(home.toRealPath().normalize)
          found <- NodeToolchain.resolve(onPath(root, home))
          fetched <- ZIO.attemptBlocking(
            Files.exists(root.resolve("cache").resolve("nodes"))
          )
        yield assertTrue(
          found.version.value == "25.2.0",
          (found.identity match
            case RuntimeIdentity.Local(path, _) => path == canonical
          ),
          !fetched
        )
    ,
    test("Node 22 fails with the floor and the found version"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-node-old")
          home <- node(root.resolve("home"), "v22.14.0")
          canonical <- ZIO.attemptBlocking(home.toRealPath().normalize)
          result <- NodeToolchain.resolve(onPath(root, home)).either
          version <- ZIO.fromEither(NodeVersion.from("22.14.0"))
        yield assertTrue(
          result == Left(
            NodeMiss.TooOld(NodeToolchain.Floor, version, canonical)
          ),
          result.left.exists(error =>
            error.message.contains("25") && error.message.contains("22.14.0")
          )
        )
    ,
    test("a missing binary fails naming the floor"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-node-absent")
          result <- NodeToolchain.resolve(machine(root, Map.empty)).either
        yield assertTrue(
          result == Left(NodeMiss.Absent(NodeToolchain.Floor)),
          result.left.exists(_.message.contains("25"))
        )
    ,
    test("node.local below the floor does not fall through"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-node-local")
          good <- node(root.resolve("good"), "v25.2.0")
          bad <- node(root.resolve("bad"), "v22.14.0")
          _ <- Workspace.writeString(
            root.resolve(".histos/node.local"),
            bad.toString + "\n"
          )
          result <- NodeToolchain
            .resolve(onPath(root, good))
            .either
          version <- ZIO.fromEither(NodeVersion.from("22.14.0"))
        yield assertTrue(
          result == Left(NodeMiss.TooOld(NodeToolchain.Floor, version, bad))
        )
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
      NativeOrigin.Central
    )

  private def onPath(root: Path, home: Path): Machine =
    machine(root, Map("PATH" -> home.resolve("bin").toString))

  private def node(home: Path, version: String): IO[IoFailure, Path] =
    val binary = home.resolve("bin").resolve("node")
    for
      _ <- Workspace.createDirectories(binary.getParent)
      _ <- Workspace.writeString(
        binary,
        s"#!/bin/sh\nprintf '%s\\n' '$version'\n"
      )
      _ <- ZIO
        .attemptBlocking(
          Files.setPosixFilePermissions(
            binary,
            PosixFilePermissions.fromString("rwxr-xr-x")
          )
        )
        .mapError(_ => IoFailure(binary, IoFault.Write))
    yield home
