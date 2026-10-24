package histos.toolchain

import histos.toolchain.RuntimeLockFixture
import histos.archive.*
import heddle.client.{Client, ClientError}
import heddle.http.{Body, Request, Response}
import histos.jdk.{
  Machine,
  Os,
  Settled,
  JdkVersion,
  JdkLock,
  Toolchain,
  ToolchainError,
  OverrideError,
  Reject,
  JdkRuntime,
  JdkStage,
  Sha256
}
import histos.native.NativeOrigin
import histos.node.*
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import zio.{Chunk, IO, Scope, ZIO, durationInt}
import zio.test.*

object RuntimePolicySpec extends ZIOSpecDefault:
  private def machine(root: Path): Machine =
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

  private def client(bytes: Chunk[Byte]): Client = new Client:
    def batched(request: Request): IO[ClientError, Response] =
      ZIO.succeed(Response(body = Body.fromBytes(bytes)))
    def streaming(request: Request): ZIO[Scope, ClientError, Response] =
      batched(request)

  private def jdk(home: Path): IO[IoFailure, Path] =
    Workspace.createDirectories(home.resolve("bin")) *>
      Workspace.writeString(home.resolve("bin/javac"), "javac") *>
      Workspace
        .writeString(home.resolve("release"), "JAVA_VERSION=\"21.0.2\"\n")
        .as(home)

  private def node(home: Path): IO[IoFailure, Path] =
    for
      _ <- Workspace.createDirectories(home.resolve("bin"))
      _ <- Workspace.writeString(
        home.resolve("bin/node"),
        "#!/bin/sh\nprintf '%s\\n' 'v22.14.0'\n"
      )
      _ <- ZIO
        .attemptBlocking(
          Files.setPosixFilePermissions(
            home.resolve("bin/node"),
            PosixFilePermissions.fromString("rwxr-xr-x")
          )
        )
        .mapError(_ => IoFailure(home.resolve("bin/node"), IoFault.Write))
    yield home

  def spec = suite("runtime lock policy")(
    test(
      "without a JDK lock the installed bootstrap runtime is retained with content identity"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          home <- jdk(root.resolve("bootstrap"))
          found <- Toolchain.resolve(machine(root).copy(running = Some(home)))
          version <- ZIO.fromEither(JdkVersion.from("21.0.2"))
          identity <- RuntimeFiles.identity(home)
        yield assertTrue(found == JdkRuntime(home, version, identity))
    ,
    test("without a Node lock PATH provides the explicit bootstrap trust seam"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          home <- node(root.resolve("bootstrap"))
          found <- NodeToolchain.resolve(
            machine(root).copy(env =
              Map("PATH" -> home.resolve("bin").toString)
            )
          )
          canonical <- ZIO.attemptBlocking(home.toRealPath())
          identity <- RuntimeFiles.identity(home)
        yield assertTrue(
          found == NodeRuntime(
            NodeBinary(canonical.resolve("bin/node")),
            identity
          )
        )
    ,
    test(
      "a JDK lock selects its archive despite JAVA_HOME and the running JVM"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          local <- jdk(root.resolve("local"))
          bytes <- ArchiveFixture.zip(
            "jdk/bin/javac" -> Chunk.empty,
            "jdk/bin/java" -> Chunk.empty,
            "jdk/release" -> Chunk.fromArray(
              "JAVA_VERSION=\"21.0.2\"\n".getBytes(
                java.nio.charset.StandardCharsets.UTF_8
              )
            )
          )
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/jdk.zip"))
          version <- ZIO.fromEither(JdkVersion.from("21.0.2"))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.jdk(
              version,
              ArchiveSource.Remote(url),
              Sha256.of(bytes)
            )
          )
          _ <- Workspace.writeString(
            root.resolve(".histos/jdk.lock"),
            JdkLock.render(lock)
          )
          found <- Toolchain(
            JdkStage(ArchiveStage(client(bytes), CommonsArchiveDecoder()))
          )
            .resolve(
              machine(root).copy(
                env = Map("JAVA_HOME" -> local.toString),
                running = Some(local)
              )
            )
        yield assertTrue(
          found.home == root
            .resolve("cache/jdks")
            .resolve(Sha256.of(bytes).value)
            .resolve("unpack/jdk"),
          found.version == version,
          found.identity == RuntimeIdentity.Locked(Sha256.of(bytes))
        )
    ,
    test("a Node lock selects the exact archive instead of PATH"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          local <- node(root.resolve("local"))
          bytes <- ArchiveFixture.zip(
            "node/bin/node" -> Chunk.fromArray(
              "#!/bin/sh\nprintf '%s\\n' 'v22.14.0'\n".getBytes(
                java.nio.charset.StandardCharsets.UTF_8
              )
            )
          )
          url <- ZIO.fromEither(HttpsUrl.from("https://example.test/node.zip"))
          version <- ZIO.fromEither(NodeVersion.from("22.14.0"))
          lock <- ZIO.fromEither(
            RuntimeLockFixture.node(
              version,
              ArchiveSource.Remote(url),
              Sha256.of(bytes)
            )
          )
          _ <- Workspace.writeString(
            root.resolve(".histos/node.lock"),
            NodeLock.render(lock)
          )
          found <- NodeToolchain(
            NodeStage(ArchiveStage(client(bytes), CommonsArchiveDecoder()))
          )
            .resolve(
              machine(root).copy(env =
                Map("PATH" -> local.resolve("bin").toString)
              )
            )
        yield assertTrue(
          found.binary.path == root
            .resolve("cache/nodes")
            .resolve(Sha256.of(bytes).value)
            .resolve("unpack/node/bin/node"),
          found.identity == RuntimeIdentity.Locked(Sha256.of(bytes))
        )
    ,
    test(
      "malformed and missing explicit overrides never fall back to bootstrap runtimes"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          java <- jdk(root.resolve("bootstrap-jdk"))
          js <- node(root.resolve("bootstrap-node"))
          _ <- Workspace.writeString(
            root.resolve(".histos/toolchain.local"),
            "relative"
          )
          _ <- Workspace.writeString(root.resolve(".histos/node.local"), "")
          badJdk <- Toolchain
            .resolve(machine(root).copy(running = Some(java)))
            .either
          badNode <- NodeToolchain
            .resolve(
              machine(root)
                .copy(env = Map("PATH" -> js.resolve("bin").toString))
            )
            .either
          missing = root.resolve("absent")
          _ <- Workspace.writeString(
            root.resolve(".histos/toolchain.local"),
            missing.toString
          )
          _ <- Workspace.writeString(
            root.resolve(".histos/node.local"),
            missing.toString
          )
          missingJdk <- Toolchain
            .resolve(machine(root).copy(running = Some(java)))
            .either
          missingNode <- NodeToolchain
            .resolve(
              machine(root)
                .copy(env = Map("PATH" -> js.resolve("bin").toString))
            )
            .either
        yield assertTrue(
          badJdk == Left(
            ToolchainError.Override(OverrideError.Relative("relative"))
          ),
          badNode == Left(
            NodeMiss.Unresolved(NodeError.Override(NodeOverrideError.Empty))
          ),
          missingJdk == Left(ToolchainError.Rejected(missing, Reject.NoJavac)),
          missingNode == Left(NodeMiss.Rejected(missing, NodeReject.NoBinary))
        )
    ,
    test(
      "B7 present invalid locks fail even with valid local JDK and Node overrides"
    ):
      ZIO.scoped:
        ZIO
          .foreach(List("directory", "dangling", "malformed")): kind =>
            for
              root <- Workspace.work("histos-trust-local")
              java <- jdk(root.resolve("local-jdk"))
              js <- node(root.resolve("local-node"))
              _ <- Workspace.writeString(
                root.resolve(".histos/toolchain.local"),
                java.toString
              )
              _ <- Workspace
                .writeString(root.resolve(".histos/node.local"), js.toString)
              paths = List("jdk.lock", "node.lock")
                .map(root.resolve(".histos").resolve(_))
              _ <- ZIO.foreachDiscard(paths): path =>
                kind match
                  case "directory" => Workspace.createDirectories(path).unit
                  case "dangling"  =>
                    ZIO
                      .attemptBlocking(
                        Files.createSymbolicLink(path, root.resolve("missing"))
                      )
                      .mapError(_ => IoFailure(path, IoFault.Create))
                      .unit
                  case _ => Workspace.writeString(path, "not a lock")
              foundJdk <- Toolchain
                .resolve(machine(root).copy(running = Some(java)))
                .either
              foundNode <- NodeToolchain
                .resolve(
                  machine(root)
                    .copy(env = Map("PATH" -> js.resolve("bin").toString))
                )
                .either
            yield assertTrue(
              if kind == "malformed" then
                foundJdk == Left(
                  ToolchainError.Lock(
                    histos.jdk.LockError
                      .Schema(ToolLockError.UnknownField("not"))
                  )
                )
              else
                foundJdk == Left(
                  ToolchainError.Io(
                    IoFailure(root.resolve(".histos/jdk.lock"), IoFault.Read)
                  )
                )
              ,
              if kind == "malformed" then
                foundNode == Left(
                  NodeMiss.Unresolved(
                    NodeError.Lock(
                      NodeLockError.Schema(ToolLockError.UnknownField("not"))
                    )
                  )
                )
              else
                foundNode == Left(
                  NodeMiss.Unresolved(
                    NodeError.Io(
                      IoFailure(root.resolve(".histos/node.lock"), IoFault.Read)
                    )
                  )
                )
            )
          .map(_.foldLeft(assertTrue(true))(_ && _))
    ,
    test(
      "B7 denied configuration traversal is typed IO, never missing bootstrap trust"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-trust-stat")
          java <- jdk(root.resolve("bootstrap-jdk"))
          js <- node(root.resolve("bootstrap-node"))
          marker = root.resolve("node-executed")
          _ <- Workspace.writeString(
            js.resolve("bin/node"),
            s"#!/bin/sh\nprintf marker > '$marker'\nprintf '%s\\n' 'v22.14.0'\n"
          )
          config <- Workspace.createDirectories(root.resolve(".histos"))
          _ <- Workspace.writeString(config.resolve("jdk.lock"), "present")
          _ <- Workspace.writeString(config.resolve("node.lock"), "present")
          results <- ZIO.scoped:
            for
              mode <- ZIO
                .attemptBlocking(Files.getPosixFilePermissions(config))
                .mapError(_ => IoFailure(config, IoFault.Stat))
              _ <- ZIO.acquireRelease(
                ZIO
                  .attemptBlocking(
                    Files.setPosixFilePermissions(
                      config,
                      PosixFilePermissions.fromString("---------")
                    )
                  )
                  .mapError(_ => IoFailure(config, IoFault.Write))
              )(_ =>
                ZIO
                  .attemptBlocking(Files.setPosixFilePermissions(config, mode))
                  .ignore
              )
              raw <- RuntimeFiles
                .readIfPresent(config.resolve("jdk.lock"))
                .either
              foundJdk <- Toolchain
                .resolve(machine(root).copy(running = Some(java)))
                .either
              foundNode <- NodeToolchain
                .resolve(
                  machine(root)
                    .copy(env = Map("PATH" -> js.resolve("bin").toString))
                )
                .either
            yield (raw, foundJdk, foundNode)
          executed <- ZIO
            .attemptBlocking(Files.exists(marker))
            .mapError(_ => IoFailure(marker, IoFault.Stat))
          absent <- RuntimeFiles.readIfPresent(
            root.resolve("absent/config.lock")
          )
        yield assertTrue(
          results._1 == Left(
            IoFailure(config.resolve("jdk.lock"), IoFault.Stat)
          ),
          results._2 == Left(
            ToolchainError.Io(
              IoFailure(config.resolve("jdk.lock"), IoFault.Stat)
            )
          ),
          results._3 == Left(
            NodeMiss.Unresolved(
              NodeError.Io(
                IoFailure(config.resolve("node.local"), IoFault.Stat)
              )
            )
          ),
          !executed,
          absent.isEmpty
        )
    ,
    test(
      "manifest exclusions use the canonical home even through a path alias"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          home = root.resolve("content")
          alias = root.resolve("alias")
          _ <- Workspace.writeString(home.resolve("tool"), "content")
          _ <- Workspace.writeString(home.resolve("manifest"), "excluded")
          _ <- ZIO
            .attemptBlocking(Files.createSymbolicLink(alias, home))
            .mapError(_ => IoFailure(alias, IoFault.Create))
          direct <- RuntimeFiles.content(home, Set(home.resolve("manifest")))
          indirect <- RuntimeFiles.content(
            alias,
            Set(alias.resolve("manifest"))
          )
        yield assertTrue(direct == indirect)
    ,
    test("changing a linked local file changes the recorded content identity"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          home <- jdk(root.resolve("local"))
          linked = root.resolve("linked")
          _ <- Workspace.writeString(linked, "first")
          _ <- ZIO
            .attemptBlocking(
              Files.createSymbolicLink(home.resolve("library"), linked)
            )
            .mapError(_ => IoFailure(home.resolve("library"), IoFault.Create))
          first <- RuntimeFiles.identity(home)
          _ <- Workspace.writeString(linked, "second")
          second <- RuntimeFiles.identity(home)
          canonical <- ZIO.attemptBlocking(home.toRealPath())
        yield assertTrue(
          first.is(_.subtype[RuntimeIdentity.Local]).home == canonical,
          second.is(_.subtype[RuntimeIdentity.Local]).home == canonical,
          first != second
        )
  ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(2.minutes)
