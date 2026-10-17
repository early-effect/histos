package histos.cli

import histos.load.LoadError

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
    ZIO.attempt(candidates.find(Files.isRegularFile(_))).mapError(t => LoadError.Failed(t.getClass.getName)).flatMap:
      case Some(jar) => ZIO.succeed(jar)
      case None      => ZIO.fail(LoadError.MissingFile("dist/histos-api.jar"))

  private def repo: ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO.attempt(Files.createTempDirectory("histos-cli")).mapError(t => LoadError.Failed(t.getClass.getName))
      _   <- ZIO.addFinalizer(delete(dir))
      _   <- ZIO.attempt(Files.writeString(dir.resolve("histos.scala"), source)).mapError(t => LoadError.Failed(t.getClass.getName))
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
      done <- Cli.execute(args, dir, jar).provideSome[Scope](
                histos.load.Loader.live,
                ZLayer.succeed(histos.cache.ContentStore.in(dir.resolve("cache"))),
              )
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
          dir  <- ZIO.attempt(Files.createTempDirectory("histos-none")).mapError(t => LoadError.Failed(t.getClass.getName))
          _    <- ZIO.addFinalizer(delete(dir))
          done <- Cli.execute(List("graph"), dir, jar).provideSome[Scope](
                    histos.load.Loader.live,
                    ZLayer.succeed(histos.cache.ContentStore.in(dir.resolve("cache"))),
                  )
        yield assertTrue(done.exit == CliExit.LoadFailed && done.exit.code == 3 && done.err.contains("histos.scala"))
  )

  def spec = cli
