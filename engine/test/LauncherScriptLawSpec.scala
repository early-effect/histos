package histos.publish

import com.sun.net.httpserver.SimpleFileServer

import histos.jdk.{Sha256, Sha256Digest, Toolchain}
import histos.workspace.Workspace

import java.io.File
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, System, ZIO}
import zio.test.*

/** The checkout script refuses an old JDK, installs `shim-<sha256>` from loopback, and execs it. */
object LauncherScriptLawSpec extends ZIOSpecDefault:
  private val digest = Sha256("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

  def spec = suite("launcher script laws")(
    test("the floor, the pinned shim, and the windows refusal come from the rendering ship"):
      val script = LauncherScript.posix(digest)
      val floor  = Toolchain.Floor.value
      assertTrue(
        script.startsWith("#!/bin/sh\n"),
        script.contains(s"shim_sha=${digest.value}"),
        script.contains("\"$dist/shim-$shim_sha\""),
        !script.contains("shim.sha256"),
        !script.contains("\"$dist/shim\""),
        !script.contains("__SHIM_SHA__"),
        !script.contains("__FLOOR__"),
        script.contains("java_major"),
        script.contains(s"-ge $floor"),
        script.contains(s"older than $floor"),
        script.contains(s"A JDK $floor or newer is required. Histos does not install it."),
        script.contains(s"brew install --cask temurin@$floor"),
        script.contains(s"https://adoptium.net/temurin/releases?version=$floor"),
        !script.contains("-ge 21"),
        !script.contains("HISTOS_VERSION"),
        script.contains("Histos runs on macOS and Linux. On Windows, use WSL."),
        script.contains("exec \"$shim\" \"$@\""),
        PublishedShim.load == Left(ScriptError.Shim),
      )
    ,
    test("update writes the wrapper with no assembly, and check keeps it"):
      Workspace
        .work("histos-launcher")
        .flatMap: repo =>
          val script = repo.resolve("histos")
          for
            absent  <- ZIO.attemptBlocking(Files.exists(script))
            _       <- LauncherScript.update(repo, digest)
            body    <- ZIO.attemptBlocking(Files.readString(script))
            exec    <- ZIO.attemptBlocking(Files.isExecutable(script))
            matched <- LauncherScript.check(repo, digest).either
            _       <- ZIO.attemptBlocking(Files.writeString(script, "drift\n"))
            drifted <- LauncherScript.check(repo, digest).either
            _       <- ZIO.attemptBlocking(Files.deleteIfExists(script))
            missing <- LauncherScript.check(repo, digest).either
          yield assertTrue(
            !absent,
            body == LauncherScript.posix(digest),
            exec,
            matched == Right(()),
            drifted == Left(ScriptError.Drift(script)),
            missing == Left(ScriptError.Missing(script)),
          )
    ,
    test("the wrapper installs the pinned shim from loopback and refuses a checksum mismatch"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-wrapper-loopback")
          path <- System.env("PATH").orDie.map(_.getOrElse(""))
          bytes = "#!/bin/sh\nprintf '%s\\n' \"$*\" > \"$HISTOS_HOME/ran\"\n".getBytes(StandardCharsets.UTF_8)
          sha   = Sha256Digest.of(Chunk.fromArray(bytes))
          dist  = root.resolve("dist")
          home  = root.resolve("home")
          bin   = root.resolve("bin")
          _    <- write(dist.resolve(PublishedShim.asset(sha)), bytes)
          _    <- java(bin, "25.0.1")
          port <- serve(dist)
          script = root.resolve("histos")
          _         <- write(script, LauncherScript.posix(sha).getBytes(StandardCharsets.UTF_8))
          _         <- executable(script)
          ran       <- launch(script, home, s"http://127.0.0.1:$port", bin, path, List("compile", "api"))
          marker    <- ZIO.attemptBlocking(Files.readString(home.resolve("ran")))
          _         <- ZIO.attemptBlocking(Files.deleteIfExists(home.resolve("bin").resolve("histos")))
          _         <- write(dist.resolve(PublishedShim.asset(sha)), "not the shim\n".getBytes(StandardCharsets.UTF_8))
          bad       <- launch(script, home, s"http://127.0.0.1:$port", bin, path, List("compile"))
          kept      <- ZIO.attemptBlocking(Files.exists(home.resolve("ran")))
          installed <- ZIO.attemptBlocking(Files.exists(home.resolve("bin").resolve("histos")))
        yield assertTrue(
          ran.exit == 0,
          !ran.timedOut,
          marker == "compile api\n",
          bad.exit != 0,
          bad.err.contains(s"checksum mismatch for shim-${sha.value}"),
          !kept,
          !installed,
        )
    ,
    test("an older JDK is refused before the shim is fetched"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-wrapper-jdk")
          path <- System.env("PATH").orDie.map(_.getOrElse(""))
          home = root.resolve("home")
          bin  = root.resolve("bin")
          dist = root.resolve("dist")
          _    <- java(bin, "21.0.1")
          port <- serve(dist)
          script = root.resolve("histos")
          _       <- write(script, LauncherScript.posix(digest).getBytes(StandardCharsets.UTF_8))
          _       <- executable(script)
          ran     <- launch(script, home, s"http://127.0.0.1:$port", bin, path, Nil)
          fetched <- ZIO.attemptBlocking(Files.exists(home.resolve("bin").resolve("histos")))
        yield assertTrue(
          ran.exit != 0,
          !ran.timedOut,
          ran.err.contains(s"older than ${Toolchain.Floor.value}"),
          ran.err.contains(s"A JDK ${Toolchain.Floor.value} or newer is required."),
          !fetched,
        ),
  ) @@ TestAspect.withLiveSystem

  private def serve(dist: Path): ZIO[Scope, Nothing, Int] =
    ZIO
      .attemptBlocking(Files.createDirectories(dist))
      .orDie *>
      ZIO
        .acquireRelease(
          ZIO
            .attempt:
              val server = SimpleFileServer.createFileServer(
                new InetSocketAddress("127.0.0.1", 0),
                dist.toAbsolutePath,
                SimpleFileServer.OutputLevel.NONE,
              )
              server.start()
              server
            .orDie
        )(server => ZIO.succeed(server.stop(0)))
        .map(_.getAddress.getPort)

  private def java(bin: Path, version: String): ZIO[Any, Nothing, Unit] =
    val text =
      s"""#!/bin/sh
         |if [ "$${1:-}" = -version ]; then echo 'openjdk version "$version" 2026-10-24' >&2; exit 0; fi
         |echo unexpected >&2
         |exit 1
         |""".stripMargin
    write(bin.resolve("java"), text.getBytes(StandardCharsets.UTF_8)) *> executable(bin.resolve("java"))

  private def write(path: Path, bytes: Array[Byte]): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking:
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
        ()
      .orDie

  private def executable(path: Path): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking(Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-xr-x")))
      .unit
      .orDie

  private final case class Ran(exit: Int, err: String, timedOut: Boolean)

  private def launch(
      script: Path,
      home: Path,
      dist: String,
      javaBin: Path,
      path: String,
      args: List[String],
  ): ZIO[Any, Nothing, Ran] =
    val err = script.getParent.resolve("stderr")
    ZIO
      .attemptBlocking:
        Files.deleteIfExists(err)
        Files.deleteIfExists(home.resolve("ran"))
        val builder = new ProcessBuilder(("sh" :: script.toString :: args).asJava)
        builder.directory(script.getParent.toFile)
        val env = builder.environment()
        env.clear()
        env.put("PATH", s"$javaBin${File.pathSeparator}$path")
        env.put("HOME", home.getParent.resolve("user").toString)
        env.put("HISTOS_HOME", home.toString)
        env.put("HISTOS_DIST", dist)
        builder.redirectInput(ProcessBuilder.Redirect.from(new File("/dev/null")))
        builder.redirectOutput(ProcessBuilder.Redirect.DISCARD)
        builder.redirectError(err.toFile)
        val process = builder.start()
        val done    = process.waitFor(10, TimeUnit.SECONDS)
        if !done then process.destroyForcibly()
        val text = if Files.isRegularFile(err) then Files.readString(err) else ""
        Ran(if done then process.exitValue() else -1, text, !done)
      .orDie
end LauncherScriptLawSpec
