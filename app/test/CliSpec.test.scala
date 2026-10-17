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
    (for
      jar  <- apiJar
      dir  <- repo
      done <- Cli.execute(args, dir, jar)
    yield done).provideSome[Scope](histos.load.Loader.live)

  private val cli = suite("cli")(
    test("version does not touch the build"):
      assertTrue(Cli.parse(List("version")) == Right(Command.Version) && Cli.VersionText.startsWith("histos ")),
    test("unknown commands and missing ids are usage"):
      assertTrue(
        Cli.parse(Nil) == Left(Cli.usageText),
        Cli.parse(List("run")) == Left("run needs one task id"),
        Cli.parse(List("nope")) == Left("unknown command nope"),
      ),
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
          done <- Cli.execute(List("graph"), dir, jar).provideSome[Scope](histos.load.Loader.live)
        yield assertTrue(done.exit == CliExit.LoadFailed && done.exit.code == 3 && done.err.contains("histos.scala"))
  )

  def spec = cli
