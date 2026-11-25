package histos.program

import histos.native.NativeLink
import histos.server.RunLaunch
import histos.suite.HistosSpec
import histos.{Module, Platform, Platforms, ProjectName, ScalaVersion, ScalaVersions, SourcePos, ThreadStack}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.ZIO
import zio.test.*

object RunLaunchSpec extends HistosSpec:
  private val server = Map(
    "PATH"        -> "/server/bin",
    "ONLY_SERVER" -> "kept",
    "SHARED"      -> "server",
  )

  def spec = suite("a program launch")(
    test("a shell replaces the server map and drops a server-only key") {
      val shell = RunLaunch.Shell(
        Map("PATH" -> "/caller/bin", "SHARED" -> "caller", "TERRET_POLICY" -> "Clamp"),
        Path.of("/work"),
      )
      val env = shell.programEnv(server)
      assertTrue(
        env.get("PATH").contains("/caller/bin"),
        env.get("SHARED").contains("caller"),
        env.get("TERRET_POLICY").contains("Clamp"),
        !env.contains("ONLY_SERVER"),
      )
    },
    test("a tool keeps a server key and overrides one") {
      val tool = RunLaunch.Tool(Map("SHARED" -> "tool", "TERRET_POLICY" -> "Clamp"), None)
      val env  = tool.programEnv(server)
      assertTrue(
        env.get("PATH").contains("/server/bin"),
        env.get("ONLY_SERVER").contains("kept"),
        env.get("SHARED").contains("tool"),
        env.get("TERRET_POLICY").contains("Clamp"),
      )
    },
    test("the module stack wins over the caller") {
      val module = Module(
        ProjectName("app"),
        Platforms(Platform.Native, Nil),
        ScalaVersions(ScalaVersion("3.10.0"), Nil),
        Nil,
        Nil,
        SourcePos("histos.scala", 1, 1),
        threadStack = ThreadStack("16m"),
      )
      val caller = Map("SCALANATIVE_THREAD_STACK_SIZE" -> "1m", "TERRET_POLICY" -> "Clamp")
      val env    = Program.nativeEnvironment(caller, module)
      assertTrue(
        env.get("SCALANATIVE_THREAD_STACK_SIZE").contains("16m"),
        env.get("TERRET_POLICY").contains("Clamp"),
      )
    },
    test("the linker child keeps the server PATH and drops a caller variable") {
      val shell   = RunLaunch.Shell(Map("PATH" -> "/caller/bin", "HOME" -> "/home"), Path.of("/work"))
      val program = shell.programEnv(server)
      assertTrue(
        program.get("PATH").contains("/caller/bin"),
        !program.contains("ONLY_SERVER"),
        NativeLink.linkerEnv(server) == Map("PATH" -> "/server/bin"),
      )
    },
    test("a missing directory, a file, a path outside, and a symlink that leaves are refused") {
      sandbox: root =>
        val repo     = root.resolve("repo")
        val outside  = root.resolve("outside")
        val missing  = repo.resolve("missing")
        val file     = repo.resolve("notes.txt")
        val link     = repo.resolve("leave")
        val relative = Path.of("..")
        for
          _ <- ZIO.attemptBlocking {
            Files.createDirectories(repo)
            Files.createDirectories(outside)
            Files.writeString(file, "x")
            Files.createSymbolicLink(link, outside)
            ()
          }.orDie
          gone    <- RunDirectory.accept(repo, missing).either
          noted   <- RunDirectory.accept(repo, file).either
          left    <- RunDirectory.accept(repo, outside).either
          escaped <- RunDirectory.accept(repo, link).either
          parent  <- RunDirectory.accept(repo, relative).either
        yield assertTrue(
          gone == Left(RunDirectoryFault.Missing(missing)),
          noted == Left(RunDirectoryFault.NotDirectory(file)),
          left == Left(RunDirectoryFault.Outside(outside)),
          escaped == Left(RunDirectoryFault.Outside(link)),
          parent == Left(RunDirectoryFault.Outside(relative)),
        )
    },
    test("a directory inside the repo is accepted and a relative path is repo-relative") {
      sandbox: root =>
        val repo   = root.resolve("repo")
        val nested = repo.resolve("nested")
        for
          _ <- ZIO.attemptBlocking {
            Files.createDirectories(nested)
            ()
          }.orDie
          real     <- ZIO.attemptBlocking(nested.toRealPath()).orDie
          absolute <- RunDirectory.accept(repo, nested).either
          relative <- RunDirectory.accept(repo, Path.of("nested")).either
        yield assertTrue(absolute == Right(real), relative == Right(real))
    },
    test("a shell directory may sit outside the repo") {
      sandbox: root =>
        val outside = root.resolve("outside")
        for
          _     <- ZIO.attemptBlocking(Files.createDirectories(outside)).orDie
          real  <- ZIO.attemptBlocking(outside.toRealPath()).orDie
          found <- RunDirectory.existing(outside).either
        yield assertTrue(found == Right(real))
    },
  )

  private def sandbox[A](use: Path => ZIO[Any, Any, A]): ZIO[Any, Any, A] =
    ZIO.scoped:
      ZIO
        .acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-run")).orDie)(root =>
          ZIO.attemptBlocking(remove(root)).orDie
        )
        .flatMap(use)

  private def remove(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
end RunLaunchSpec
