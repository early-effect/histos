package histos.publish

import histos.jdk.{Sha256, Sha256Digest}
import histos.workspace.Workspace

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.TimeUnit

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object ShimSpec extends ZIOSpecDefault:
  def spec = suite("the install shim")(
    test("a coordinate manifest does not need a dist jar") {
      ZIO.scoped {
        for
          root       <- Workspace.work("histos-shim-spec").orDieWith(error => new java.io.IOException(error.message))
          coordinate <- kind(root, coordinateManifest)
          dist       <- kind(root, distManifest)
          missing    <- kind(root, "{\n  \"version\": \"0.5.0\"\n}\n")
        yield assertTrue(coordinate == "coordinate", dist == "dist", missing == "missing")
      }
    },
    test("the checkout script replaces a shim whose hash differs") {
      val body = LauncherScript.posix(Sha256.trusted("ab" * 32))
      assertTrue(
        body.contains("""[ ! -x "$shim" ] || [ "$(hash_of "$shim")" != "$shim_sha" ]""")
      )
    },
    test("installing coursier does not move the download onto itself") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-shim-mv").orDieWith(error => new java.io.IOException(error.message))
          file <- ZIO.fromEither(script).mapError(message => new java.io.IOException(message))
          text <- ZIO.attemptBlocking(Files.readString(file)).orDie
          sha  <- ZIO
            .fromOption(coursierSha(text))
            .orElseFail(new java.io.IOException("coursier_sha is not in the shim"))
          bin  = root.resolve("bin")
          home = root.resolve("home")
          log  = root.resolve("mv.log")
          _     <- ZIO.attemptBlocking(Files.createDirectories(bin)).orDie
          _     <- executable(bin.resolve("curl"), curlStub)
          _     <- executable(bin.resolve("sha256sum"), shaStub)
          _     <- executable(bin.resolve("mv"), mvStub)
          out   <- runInstall(file, bin, home, log, sha)
          moved <- ZIO.attemptBlocking {
            if Files.isRegularFile(log) then Files.readString(log).split('\n').toList.filter(_.nonEmpty)
            else Nil
          }.orDie
        yield
          val from = moved.headOption.getOrElse("")
          val to   = moved.lift(1).getOrElse("")
          assertTrue(
            moved.length == 2,
            from != to,
            to.endsWith("coursier.jar"),
            !out.contains("are the same file"),
            !out.contains("manifest version is not"),
          )
      }
    },
    test("a runner links only when install asks for the client") {
      ZIO.scoped {
        for
          asked <- runnerInstall(client = true)
          plain <- runnerInstall(client = false)
        yield assertTrue(
          asked.log.contains("toolchain client"),
          asked.kit,
          !plain.log.contains("toolchain client"),
          !plain.kit,
        )
      }
    },
    test("an explicit client link failure fails the install") {
      ZIO.scoped {
        for
          failed <- runnerInstall(client = true, linkFails = true).exit
          plain  <- runnerInstall(client = false, linkFails = true)
        yield assertTrue(failed.isFailure, !plain.kit, !plain.log.contains("toolchain client"))
      }
    },
    test("a local asset manifest copies the staged ivy jar") {
      ZIO.scoped {
        for copied <- ivyAssetInstall
        yield assertTrue(copied.installed == copied.staged, !copied.log.contains("coursier"))
      }
    },
    test("a commit pin does not offer an older release") {
      val pin   = "0.5.0-dc7382bc347b-SNAPSHOT"
      val names =
        """0.4.4-manifest.json
          |0.5.0-manifest.json
          |channel-0.4.4-SNAPSHOT
          |channel-0.5.0-SNAPSHOT
          |channel-0.6.0-SNAPSHOT
          |""".stripMargin
      script match
        case Left(message) => ZIO.fail(new java.io.IOException(message))
        case Right(file)   =>
          for
            line     <- invoke(file, Chunk("--snapshot-line", pin), "")
            stamp    <- invoke(file, Chunk("--snapshot-line", "0.5.0-20261119.025603-1"), "")
            pointer  <- invoke(file, Chunk("--snapshot-line", "0.5.0-SNAPSHOT"), "")
            release  <- invoke(file, Chunk("--snapshot-line", "0.4.4"), "")
            older    <- invoke(file, Chunk("--newest", "release", line.out), "0.4.4-manifest.json\n")
            ahead    <- invoke(file, Chunk("--newest", "release", line.out), names)
            followed <- invoke(file, Chunk("--newest", "snapshot", line.out), names)
          yield assertTrue(
            line.code == 0 && line.out == "0.5.0",
            stamp.code == 0 && stamp.out == "0.5.0",
            pointer.code == 0 && pointer.out == "0.5.0",
            release.code != 0,
            older.code != 0,
            ahead.code == 0 && ahead.out == "0.5.0",
            followed.code == 0 && followed.out == "0.6.0-SNAPSHOT",
          )
    },
    test("a one-argument install does not read an unset client flag") {
      ZIO.scoped {
        for
          runner <- oneArg(ci = true)
          local  <- oneArg(ci = false)
        yield assertTrue(
          runner.ran,
          !runner.kit,
          local.ran,
          local.kit,
        )
      }
    },
    test("HISTOS_PIN selects its jar and a bad id fails") {
      val pin = "0.5.0-abcdefabcdef-SNAPSHOT"
      ZIO.scoped {
        script match
          case Left(message) => ZIO.fail(new java.io.IOException(message))
          case Right(file)   =>
            for
              root <- Workspace.work("histos-shim-pin").orDieWith(error => new java.io.IOException(error.message))
              bin  = root.resolve("bin")
              home = root.resolve("home")
              log  = root.resolve("java.log")
              jar  = home.resolve("toolchains").resolve(pin).resolve("histos.jar")
              _        <- write(jar, "pin-jar")
              _        <- write(bin.resolve("java"), javaStub)
              _        <- executable(bin.resolve("java"))
              _        <- write(root.resolve("build.properties"), "histos.version=0.5.0-other-SNAPSHOT\n")
              selected <- pinned(file, root, bin, home, log, pin, List("version"))
              slash    <- pinned(file, root, bin, home, log, "a/b", List("version"))
              dot      <- pinned(file, root, bin, home, log, "..", List("version"))
              space    <- pinned(file, root, bin, home, log, "a b", List("version"))
              back     <- pinned(file, root, bin, home, log, "a\\b", List("version"))
              missing  <- pinned(file, root, bin, home, log, "0.5.0-missing-SNAPSHOT", List("version"))
              recorded <- ZIO.attemptBlocking(Files.readString(log)).orDie
            yield assertTrue(
              selected.code == 0,
              recorded.contains(jar.toString),
              !recorded.contains("0.5.0-other-SNAPSHOT"),
              slash.code != 0 && slash.out.contains("not one path segment"),
              dot.code != 0 && dot.out.contains("not one path segment"),
              space.code != 0 && space.out.contains("not one path segment"),
              back.code != 0 && back.out.contains("not one path segment"),
              missing.code != 0 && missing.out.contains("is not installed"),
            )
      }
    },
  )

  private final case class ShimOut(code: Int, out: String)

  private def invoke(file: Path, args: Chunk[String], input: String): ZIO[Any, java.io.IOException, ShimOut] =
    ZIO
      .attemptBlocking {
        val command = file.toString :: args.toList
        val process = new ProcessBuilder(command*)
          .redirectErrorStream(true)
          .start()
        val stdin = process.getOutputStream
        stdin.write(input.getBytes(StandardCharsets.UTF_8))
        stdin.close()
        val out = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
        ShimOut(process.waitFor(), out)
      }
      .mapError(error => new java.io.IOException(Option(error.getMessage).getOrElse(error.toString)))

  private def kind(root: Path, text: String): ZIO[Any, java.io.IOException, String] =
    script match
      case Left(message) => ZIO.fail(new java.io.IOException(message))
      case Right(file)   =>
        ZIO
          .attemptBlocking {
            val manifest = root.resolve("manifest.json")
            Files.writeString(manifest, text)
            val process = new ProcessBuilder(file.toString, "--manifest-kind", manifest.toString)
              .redirectErrorStream(true)
              .start()
            val out  = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
            val code = process.waitFor()
            (code, out)
          }
          .mapError(error => new java.io.IOException(Option(error.getMessage).getOrElse(error.toString)))
          .flatMap:
            case (0, out)    => ZIO.succeed(out)
            case (code, out) => ZIO.fail(new java.io.IOException(s"shim exited $code: $out"))

  private def script: Either[String, Path] =
    val start = Path.of("").toAbsolutePath.normalize
    walk(start) match
      case Some(root) => Right(root.resolve("install").resolve("histos"))
      case None       => Left(s"install/histos is not above $start")

  private def walk(current: Path): Option[Path] =
    if Files.isRegularFile(current.resolve("install").resolve("histos")) then Some(current)
    else
      Option(current.getParent) match
        case Some(parent) => walk(parent)
        case None         => None

  private val installVersion = "0.5.0-test-SNAPSHOT"

  private final case class RunnerInstall(log: String, kit: Boolean)

  private final case class IvyCopy(installed: Chunk[Byte], staged: Chunk[Byte], log: String)

  private final case class OneArg(ran: Boolean, kit: Boolean)

  /** A normal command installs a missing toolchain with one argument. `CI` decides the kit. */
  private def oneArg(ci: Boolean): ZIO[Scope, java.io.IOException, OneArg] =
    val jarBytes = "staged-ivy-jar".getBytes(StandardCharsets.UTF_8)
    val kitBytes = "kit".getBytes(StandardCharsets.UTF_8)
    val org      = "rocks.earlyeffect"
    val module   = "histos"
    script match
      case Left(message) => ZIO.fail(new java.io.IOException(message))
      case Right(file)   =>
        for
          root <- Workspace.work("histos-shim-one-arg").orDieWith(error => new java.io.IOException(error.message))
          bin    = root.resolve("bin")
          home   = root.resolve("home")
          assets = root.resolve("assets")
          ivy    = root.resolve("ivy")
          staged = ivy.resolve(org).resolve(module).resolve(installVersion).resolve("jars").resolve(s"$module.jar")
          tool   = home.resolve("toolchains").resolve(installVersion)
          log    = root.resolve("java.log")
          _      <- write(root.resolve("build.properties"), s"histos.version=$installVersion\n")
          _      <- write(staged, jarBytes)
          _      <- write(assets.resolve("client-kit.zip"), kitBytes)
          _      <- write(assets.resolve("manifest.json"), oneArgManifest(org, module, digest(kitBytes)))
          _      <- write(bin.resolve("java"), javaStub)
          _      <- executable(bin.resolve("java"))
          _      <- runOneArg(file, root, bin, home, assets, ivy, log, ci)
          result <- ZIO.attemptBlocking {
            val recorded = if Files.isRegularFile(log) then Files.readString(log) else ""
            val placed   = Files.isRegularFile(tool.resolve("client-kit.zip"))
            OneArg(recorded.contains("version"), placed)
          }.orDie
        yield result

  private def oneArgManifest(org: String, module: String, kitSha: String): String =
    s"""{
       |  "version": "$installVersion",
       |  "group": "$org",
       |  "artifact": "$module",
       |  "revision": "$installVersion",
       |  "ivyOrganisation": "$org",
       |  "ivyModule": "$module",
       |  "ivyRevision": "$installVersion",
       |  "clientKit": "client-kit.zip",
       |  "clientKitSha256": "$kitSha"
       |}
       |""".stripMargin

  private def runOneArg(
      shim: Path,
      root: Path,
      bin: Path,
      home: Path,
      assets: Path,
      ivy: Path,
      log: Path,
      ci: Boolean,
  ): ZIO[Any, java.io.IOException, Unit] =
    ZIO
      .attemptBlocking {
        val builder = new ProcessBuilder(shim.toString, "version")
        builder.directory(root.toFile)
        builder.redirectErrorStream(true)
        val env = builder.environment()
        env.put("CI", if ci then "true" else "false")
        env.put("HISTOS_HOME", home.toString)
        env.put("HISTOS_ASSETS", assets.toString)
        env.put("HISTOS_IVY", ivy.toString)
        env.put("JAVA_LOG", log.toString)
        val inherited = Option(env.get("PATH")).getOrElse("")
        env.put("PATH", bin.toString + java.io.File.pathSeparator + inherited)
        val process  = builder.start()
        val finished = process.waitFor(30, TimeUnit.SECONDS)
        val out      = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
        if !finished then
          process.destroyForcibly()
          throw new java.io.IOException(s"shim timed out: $out")
        val code = process.exitValue()
        if code != 0 then throw new java.io.IOException(s"shim exited $code: $out")
      }
      .mapError {
        case error: java.io.IOException => error
        case error                      => new java.io.IOException(Option(error.getMessage).getOrElse(error.toString))
      }
      .unit

  /** Seeds a dist toolchain and a kit under `HISTOS_ASSETS`. `CI` is set. `HISTOS_CLIENT=on` must not link. */
  private def runnerInstall(
      client: Boolean,
      linkFails: Boolean = false,
  ): ZIO[Scope, java.io.IOException, RunnerInstall] =
    val jarBytes = "jar".getBytes(StandardCharsets.UTF_8)
    val kitBytes = "kit".getBytes(StandardCharsets.UTF_8)
    val manifest = distInstallManifest(digest(jarBytes), digest(kitBytes))
    script match
      case Left(message) => ZIO.fail(new java.io.IOException(message))
      case Right(file)   =>
        for
          root <- Workspace.work("histos-shim-client").orDieWith(error => new java.io.IOException(error.message))
          bin    = root.resolve("bin")
          home   = root.resolve("home")
          assets = root.resolve("assets")
          tool   = home.resolve("toolchains").resolve(installVersion)
          log    = root.resolve("java.log")
          _ <- write(bin.resolve("java"), if linkFails then javaLinkFailStub else javaStub)
          _ <- write(tool.resolve("histos.jar"), jarBytes)
          _ <- write(tool.resolve("manifest.json"), manifest.getBytes(StandardCharsets.UTF_8))
          _ <- write(assets.resolve("client-kit.zip"), kitBytes)
          _ <- write(assets.resolve("manifest.json"), manifest.getBytes(StandardCharsets.UTF_8))
          _ <- executable(bin.resolve("java"))
          args =
            if client then List("toolchain", "install", "--client", installVersion)
            else List("toolchain", "install", installVersion)
          _    <- runShim(file, bin, home, assets, log, args, None)
          text <- ZIO.attemptBlocking {
            val recorded = if Files.isRegularFile(log) then Files.readString(log) else ""
            val placed   = Files.isRegularFile(tool.resolve("client-kit.zip"))
            RunnerInstall(recorded, placed)
          }.orDie
        yield text

  /** Seeds the packed jar only on the ivy path. The asset manifest names that coordinate. */
  private def ivyAssetInstall: ZIO[Scope, java.io.IOException, IvyCopy] =
    val jarBytes = "staged-ivy-jar".getBytes(StandardCharsets.UTF_8)
    val org      = "rocks.earlyeffect"
    val module   = "histos"
    script match
      case Left(message) => ZIO.fail(new java.io.IOException(message))
      case Right(file)   =>
        for
          root <- Workspace.work("histos-shim-ivy").orDieWith(error => new java.io.IOException(error.message))
          bin    = root.resolve("bin")
          home   = root.resolve("home")
          assets = root.resolve("assets")
          ivy    = root.resolve("ivy")
          staged = ivy.resolve(org).resolve(module).resolve(installVersion).resolve("jars").resolve(s"$module.jar")
          tool   = home.resolve("toolchains").resolve(installVersion).resolve("histos.jar")
          log    = root.resolve("java.log")
          _      <- write(staged, jarBytes)
          _      <- write(assets.resolve("manifest.json"), ivyAssetManifest(org, module, installVersion))
          _      <- write(bin.resolve("java"), javaStub)
          _      <- executable(bin.resolve("java"))
          _      <- runShim(file, bin, home, assets, log, List("toolchain", "install", installVersion), Some(ivy))
          copied <- ZIO.attemptBlocking {
            val recorded = if Files.isRegularFile(log) then Files.readString(log) else ""
            val got      = if Files.isRegularFile(tool) then Chunk.fromArray(Files.readAllBytes(tool)) else Chunk.empty
            IvyCopy(got, Chunk.fromArray(jarBytes), recorded)
          }.orDie
        yield copied

  private def ivyAssetManifest(org: String, module: String, revision: String): String =
    s"""{
       |  "version": "$revision",
       |  "group": "$org",
       |  "artifact": "$module",
       |  "revision": "$revision",
       |  "ivyOrganisation": "$org",
       |  "ivyModule": "$module",
       |  "ivyRevision": "$revision"
       |}
       |""".stripMargin

  private def distInstallManifest(jarSha: String, kitSha: String): String =
    s"""{
       |  "version": "$installVersion",
       |  "jar": "histos.jar",
       |  "jarSha256": "$jarSha",
       |  "clientKit": "client-kit.zip",
       |  "clientKitSha256": "$kitSha"
       |}
       |""".stripMargin

  private def digest(bytes: Array[Byte]): String =
    Sha256Digest.of(Chunk.fromArray(bytes)).value

  private def write(path: Path, bytes: Array[Byte]): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      Files.write(path, bytes)
      ()
    }.orDie

  private def write(path: Path, text: String): ZIO[Any, Nothing, Unit] =
    write(path, text.getBytes(StandardCharsets.UTF_8))

  private def executable(path: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-xr-x"))
      ()
    }.orDie

  /** One shim run. A non-zero exit is a result, so a rejected pin stays an assertion. */
  private def pinned(
      shim: Path,
      root: Path,
      bin: Path,
      home: Path,
      log: Path,
      pin: String,
      args: List[String],
  ): ZIO[Any, java.io.IOException, ShimOut] =
    ZIO
      .attemptBlocking {
        val builder = new ProcessBuilder((shim.toString :: args)*)
        builder.directory(root.toFile)
        builder.redirectErrorStream(true)
        val env = builder.environment()
        env.put("HISTOS_HOME", home.toString)
        env.put("HISTOS_PIN", pin)
        env.put("JAVA_LOG", log.toString)
        env.remove("HISTOS_ASSETS")
        env.remove("HISTOS_IVY")
        env.remove("CI")
        val inherited = Option(env.get("PATH")).getOrElse("")
        env.put("PATH", bin.toString + java.io.File.pathSeparator + inherited)
        val process  = builder.start()
        val finished = process.waitFor(30, TimeUnit.SECONDS)
        val out      = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
        if !finished then
          process.destroyForcibly()
          ShimOut(1, s"timed out: $out")
        else ShimOut(process.exitValue(), out)
      }
      .mapError(error => new java.io.IOException(Option(error.getMessage).getOrElse(error.toString)))

  private def runShim(
      shim: Path,
      bin: Path,
      home: Path,
      assets: Path,
      log: Path,
      args: List[String],
      ivy: Option[Path],
  ): ZIO[Any, java.io.IOException, Unit] =
    ZIO
      .attemptBlocking {
        val builder = new ProcessBuilder((shim.toString :: args)*)
        builder.redirectErrorStream(true)
        val env = builder.environment()
        env.put("CI", "true")
        env.put("HISTOS_CLIENT", "on")
        env.put("HISTOS_HOME", home.toString)
        env.put("HISTOS_ASSETS", assets.toString)
        env.put("JAVA_LOG", log.toString)
        ivy.foreach(path => env.put("HISTOS_IVY", path.toString))
        val inherited = Option(env.get("PATH")).getOrElse("")
        env.put("PATH", bin.toString + java.io.File.pathSeparator + inherited)
        val process  = builder.start()
        val finished = process.waitFor(30, TimeUnit.SECONDS)
        val out      = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
        if !finished then
          process.destroyForcibly()
          throw new java.io.IOException(s"shim timed out: $out")
        val code = process.exitValue()
        if code != 0 then throw new java.io.IOException(s"shim exited $code: $out")
      }
      .mapError {
        case error: java.io.IOException => error
        case error                      => new java.io.IOException(Option(error.getMessage).getOrElse(error.toString))
      }
      .unit

  private val javaLinkFailStub =
    """|#!/bin/sh
       |if [ "$1" = "-version" ]; then
       |  echo 'openjdk version "25.0.1"' >&2
       |  exit 0
       |fi
       |printf '%s\n' "$*" >> "$JAVA_LOG"
       |case "$*" in
       |  *toolchain\ client*) exit 1 ;;
       |esac
       |exit 0
       |""".stripMargin

  private val javaStub =
    """|#!/bin/sh
       |if [ "$1" = "-version" ]; then
       |  echo 'openjdk version "25.0.1"' >&2
       |  exit 0
       |fi
       |printf '%s\n' "$*" >> "$JAVA_LOG"
       |exit 0
       |""".stripMargin

  private def coursierSha(text: String): Option[String] =
    text.linesIterator.collectFirst {
      case line if line.startsWith("coursier_sha=") => line.stripPrefix("coursier_sha=")
    }

  private def executable(path: Path, body: String): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      Files.writeString(path, body)
      Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-xr-x"))
      ()
    }.orDie

  private def runInstall(
      shim: Path,
      bin: Path,
      home: Path,
      log: Path,
      sha: String,
  ): ZIO[Any, java.io.IOException, String] =
    ZIO
      .attemptBlocking {
        val builder = new ProcessBuilder(shim.toString, "toolchain", "install", installVersion)
        builder.redirectErrorStream(true)
        val env = builder.environment()
        env.put("CI", "true")
        env.put("HISTOS_HOME", home.toString)
        env.put("MV_LOG", log.toString)
        env.put("EXPECTED_SHA", sha)
        val inherited = Option(env.get("PATH")).getOrElse("")
        env.put("PATH", bin.toString + java.io.File.pathSeparator + inherited)
        val process  = builder.start()
        val finished = process.waitFor(30, TimeUnit.SECONDS)
        if !finished then process.destroyForcibly()
        new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      }
      .mapError(error => new java.io.IOException(Option(error.getMessage).getOrElse(error.toString)))

  private val curlStub =
    """|#!/bin/sh
       |prev=
       |out=
       |url=
       |for arg in "$@"; do
       |  if [ "$prev" = "-o" ]; then out=$arg; fi
       |  case $arg in
       |    http:*|https:*) url=$arg ;;
       |  esac
       |  prev=$arg
       |done
       |mkdir -p "$(dirname "$out")"
       |case $url in
       |  */channel-*)
       |    printf '%s\n' '0.5.0-test-SNAPSHOT' > "$out"
       |    ;;
       |  *-manifest.json)
       |    printf '%s\n' \
       |      '{' \
       |      '  "version": "0.5.0-test-SNAPSHOT",' \
       |      '  "group": "rocks.earlyeffect",' \
       |      '  "artifact": "histos",' \
       |      '  "revision": "0.5.0-test-SNAPSHOT"' \
       |      '}' > "$out"
       |    ;;
       |  *)
       |    printf 'x' > "$out"
       |    ;;
       |esac
       |""".stripMargin

  private val shaStub =
    """|#!/bin/sh
       |printf '%s  %s\n' "$EXPECTED_SHA" "$1"
       |""".stripMargin

  private val mvStub =
    """|#!/bin/sh
       |printf '%s\n%s\n' "$1" "$2" >> "$MV_LOG"
       |if [ "$1" = "$2" ]; then
       |  printf "mv: '%s' and '%s' are the same file\n" "$1" "$2" >&2
       |  exit 1
       |fi
       |/bin/mv "$1" "$2"
       |""".stripMargin

  private val coordinateManifest =
    """{
      |  "version": "0.5.0",
      |  "group": "rocks.earlyeffect",
      |  "artifact": "histos",
      |  "revision": "0.5.0"
      |}
      |""".stripMargin

  private val distManifest =
    """{
      |  "version": "0.4.4",
      |  "jar": "0.4.4-histos.jar",
      |  "jarSha256": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      |}
      |""".stripMargin
end ShimSpec
