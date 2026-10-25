package histos.jdk

import histos.native.NativeOrigin
import histos.toolchain.{Architecture, RuntimeIdentity}
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}
import zio.test.*

object JdkSpec extends ZIOSpecDefault:
  def spec = suite("JDK floor")(
    test("numeric order treats a missing component as zero"):
      val floor = Toolchain.Floor
      val required = JdkVersion.from("21.0.2")
      val older = JdkVersion.from("21.0.1")
      val same = JdkVersion.from("21.0.2+7")
      val newer = JdkVersion.from("22")
      val ancient = JdkVersion.from("17.0.9")
      assertTrue(
        floor.value == "21",
        newer.exists(_.atLeast(floor)),
        ancient.exists(!_.atLeast(floor)),
        required.exists(need => older.exists(!_.atLeast(need))),
        required.exists(need => same.exists(_.atLeast(need)))
      )
    ,
    test("a JVM at or above 21 is accepted and nothing is fetched"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-jdk-floor")
          home <- jdk(root.resolve("home"), "21.0.4")
          canonical <- ZIO.attemptBlocking(home.toRealPath().normalize)
          found <- Toolchain.resolve(machine(root, Some(home)))
          fetched <- ZIO.attemptBlocking(
            Files.exists(root.resolve("cache").resolve("jdks"))
          )
        yield assertTrue(
          found.home == home,
          found.version.value == "21.0.4",
          (found.identity match
            case RuntimeIdentity.Local(path, _) => path == canonical
          ),
          !fetched
        )
    ,
    test("the published runtime build is the jdk salt"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-jdk-build")
          home <- jdk(root.resolve("home"), "21.0.4", Some("21.0.4+7-LTS"))
          first <- Toolchain.resolve(machine(root, Some(home)))
          _ <- Workspace.writeString(home.resolve("bin/javac"), "rewritten")
          same <- Toolchain.resolve(machine(root, Some(home)))
          _ <- Workspace.writeString(
            home.resolve("release"),
            "JAVA_VERSION=\"21.0.4\"\nJAVA_RUNTIME_VERSION=\"21.0.4+8-LTS\"\n"
          )
          next <- Toolchain.resolve(machine(root, Some(home)))
        yield assertTrue(
          first.version.value == "21.0.4",
          next.version.value == "21.0.4",
          first.identity == same.identity,
          first.identity != next.identity
        )
    ,
    test("an older JVM fails with the floor and the found version"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-jdk-old")
          home <- jdk(root.resolve("home"), "17.0.9")
          result <- Toolchain.resolve(machine(root, Some(home))).either
          version <- ZIO.fromEither(JdkVersion.from("17.0.9"))
        yield assertTrue(
          result == Left(ToolchainError.TooOld(Toolchain.Floor, version, home)),
          result.left.exists(error =>
            error.message.contains("21") && error.message.contains("17.0.9")
          )
        )
    ,
    test("a missing JVM fails naming the floor"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-jdk-absent")
          result <- Toolchain.resolve(machine(root, None)).either
        yield assertTrue(
          result == Left(ToolchainError.Absent(Toolchain.Floor)),
          result.left.exists(_.message.contains("21"))
        )
    ,
    test("toolchain.local below the floor does not fall through"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-jdk-local")
          running <- jdk(root.resolve("running"), "21.0.4")
          chosen <- jdk(root.resolve("chosen"), "17.0.2")
          _ <- Workspace.writeString(
            root.resolve(".histos/toolchain.local"),
            chosen.toString + "\n"
          )
          result <- Toolchain.resolve(machine(root, Some(running))).either
          version <- ZIO.fromEither(JdkVersion.from("17.0.2"))
        yield assertTrue(
          result == Left(
            ToolchainError.TooOld(Toolchain.Floor, version, chosen)
          )
        )
  )

  private def machine(root: Path, running: Option[Path]): Machine =
    Machine(
      Map.empty,
      root.toString,
      Os.Linux,
      Architecture.X86_64,
      root.resolve("cache"),
      root,
      running,
      NativeOrigin.Central
    )

  private def jdk(
      home: Path,
      version: String,
      build: Option[String] = None
  ): IO[IoFailure, Path] =
    val runtime =
      build.fold("")(value => s"JAVA_RUNTIME_VERSION=\"$value\"\n")
    Workspace.createDirectories(home.resolve("bin")) *>
      Workspace.writeString(home.resolve("bin/javac"), "javac") *>
      Workspace
        .writeString(
          home.resolve("release"),
          s"JAVA_VERSION=\"$version\"\n$runtime"
        )
        .as(home)
