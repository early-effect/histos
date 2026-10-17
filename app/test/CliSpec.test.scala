package histos.cli

import histos.jdk.{Machine, Os, Settled}
import histos.load.{LoadError, LoadIo}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object CliSpec extends ZIOSpecDefault:
  private val source =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.9.0")
      |    Projects(core)
      |""".stripMargin

  private val apiJar: IO[LoadError, Path] =
    val candidates = List(Path.of("dist/histos-api.jar"), Path.of("../dist/histos-api.jar"))
    ZIO.attempt(candidates.find(Files.isRegularFile(_))).mapError(t => LoadError.Io(LoadIo.Temp)).flatMap:
      case Some(jar) => ZIO.succeed(jar)
      case None      => ZIO.fail(LoadError.MissingFile(Path.of("dist/histos-api.jar")))

  private def repo: ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO.attempt(Files.createTempDirectory("histos-cli")).mapError(t => LoadError.Io(LoadIo.Temp))
      _   <- ZIO.addFinalizer(delete(dir))
      _   <- ZIO.attempt(Files.writeString(dir.resolve("histos.scala"), source)).mapError(t => LoadError.Io(LoadIo.Temp))
    yield dir

  private def delete(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def invoke(args: List[String]): ZIO[Scope, LoadError, Completed] =
    for
      jar  <- apiJar
      dir  <- repo
      done <- Cli.execute(args, dir, jar).provideSome[Scope](layers(dir))
    yield done

  private val cli = suite("cli")(
    test("version does not touch the build"):
      for parsed <- Cli.parse(List("version"))
      yield assertTrue(parsed == Command.Version && Cli.VersionText.startsWith("histos ")),
    test("cache clean takes --dry-run"):
      for
        preview <- Cli.parse(List("cache", "clean", "--dry-run"))
        clean   <- Cli.parse(List("cache", "clean"))
      yield assertTrue(preview == Command.CacheClean(true) && clean == Command.CacheClean(false)),
    test("toolchain is a command"):
      for parsed <- Cli.parse(List("toolchain"))
      yield assertTrue(parsed == Command.Toolchain)
    ,
    test("resolve takes update or check, not both"):
      for
        plain  <- Cli.parse(List("resolve"))
        update <- Cli.parse(List("resolve", "--update"))
        check  <- Cli.parse(List("resolve", "--check"))
        both   <- Cli.parse(List("resolve", "--update", "--check")).either
      yield assertTrue(
        plain == Command.Resolve(ResolveAction.Verify),
        update == Command.Resolve(ResolveAction.Update),
        check == Command.Resolve(ResolveAction.Check),
        both == Left(CliParseError.Rejected),
      )
    ,
    test("resolve without a lock fails"):
      ZIO.scoped:
        for done <- invoke(List("resolve"))
        yield assertTrue(done.exit == CliExit.ResolveFailed && done.exit.code == 5 && done.err.contains("dependencies.lock"))
    ,
    test("toolchain reports a missing lock"):
      ZIO.scoped:
        for done <- invoke(List("toolchain"))
        yield assertTrue(done.exit == CliExit.ToolchainFailed && done.exit.code == 4 && done.err.contains("jdk.lock"))
    ,
    test("toolchain prints the chosen home"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          jdk = dir.resolve("jdk")
          _ <- ZIO.attempt {
                 Files.createDirectories(jdk.resolve("bin"))
                 Files.createDirectories(dir.resolve(".histos"))
                 Files.writeString(jdk.resolve("bin").resolve("javac"), "")
                 Files.writeString(jdk.resolve("release"), "JAVA_VERSION=\"21.0.2\"\n")
                 Files.writeString(
                   dir.resolve(".histos").resolve("jdk.lock"),
                   """major 21
                     |version 21.0.2
                     |url https://example.test/jdk.zip
                     |sha256 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                     |""".stripMargin,
                 )
               }.mapError(t => LoadError.Io(LoadIo.Temp))
          done <- Cli.execute(List("toolchain"), dir, jar).provideSome[Scope](layers(dir, Map("JAVA_HOME" -> jdk.toString)))
        yield assertTrue(done.exit == CliExit.Success && done.out.contains("chosen JavaHome") && done.out.contains("21.0.2") && done.out.contains("lock 21.0.2"))
    ,
    test("unknown commands and missing ids are usage"):
      for
        empty <- Cli.parse(Nil).either
        run   <- Cli.parse(List("run")).either
        nope  <- Cli.parse(List("nope")).either
      yield assertTrue(empty.isLeft && run.isLeft && nope.isLeft),
    test("graph prints the module"):
      ZIO.scoped:
        for done <- invoke(List("graph"))
        yield assertTrue(done.exit == CliExit.Success && done.out == "core jvm 3.9.0" && done.err.isEmpty)
    ,
    test("run inspect prints the project name"):
      ZIO.scoped:
        for done <- invoke(List("run", "inspect"))
        yield assertTrue(done.exit == CliExit.Success && done.out == "core")
    ,
    test("run of an unknown task fails"):
      ZIO.scoped:
        for done <- invoke(List("run", "compile"))
        yield assertTrue(done.exit == CliExit.TaskFailed && done.exit.code == 1 && done.err.contains("compile"))
    ,
    test("a missing build is a load failure"):
      ZIO.scoped:
        for
          jar  <- apiJar
          dir  <- ZIO.attempt(Files.createTempDirectory("histos-none")).mapError(t => LoadError.Io(LoadIo.Temp))
          _    <- ZIO.addFinalizer(delete(dir))
          done <- Cli.execute(List("graph"), dir, jar).provideSome[Scope](layers(dir))
        yield assertTrue(done.exit == CliExit.LoadFailed && done.exit.code == 3 && done.err.contains("histos.scala"))
  )

  private def layers(dir: Path, env: Map[String, String] = Map.empty) =
    histos.load.Loader.live ++
      ZLayer.succeed(histos.cache.ContentStore.in(dir.resolve("cache"))) ++
      ZLayer.succeed(machine(dir, env)) ++
      histos.resolve.Resolve.live(dir.resolve("cache").resolve("resolve"))

  private def machine(dir: Path, env: Map[String, String]): Machine =
    Machine(env, dir.toString, Os.Linux, dir.resolve("cache"), dir, None, Settled.No)

  def spec = cli
