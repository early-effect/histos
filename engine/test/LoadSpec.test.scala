package histos.load

import histos.BuildError

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object LoadSpec extends ZIOSpecDefault:
  private val apiJar: IO[LoadError, Path] =
    val candidates = List(Path.of("dist/histos-api.jar"), Path.of("../dist/histos-api.jar"))
    ZIO.attempt(candidates.find(Files.isRegularFile(_))).mapError(t => LoadError.Io(LoadIo.Temp)).flatMap:
      case Some(jar) => ZIO.succeed(jar)
      case None      => ZIO.fail(LoadError.MissingFile(Path.of("dist/histos-api.jar")))

  private def withRepo(source: String): ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO.attempt(Files.createTempDirectory("histos-repo")).mapError(t => LoadError.Io(LoadIo.Temp))
      _   <- ZIO.addFinalizer(deleteTree(dir))
      _   <- ZIO.attempt(Files.writeString(dir.resolve("histos.scala"), source)).mapError(t => LoadError.Io(LoadIo.Temp))
    yield dir

  private def deleteTree(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def load(source: String): ZIO[Loader & Scope, LoadError, LoadedBuild] =
    for
      jar <- apiJar
      dir <- withRepo(source)
      out <- ZIO.serviceWithZIO[Loader](_.load(dir, jar))
    yield out

  private val happy =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.9.0")
      |    Projects(core)
      |""".stripMargin

  private val loads = suite("loading histos.scala")(
    test("evaluates the graph and does not compile the project"):
      ZIO.scoped:
        for loaded <- load(happy)
        yield assertTrue:
          loaded.graph.projects.map(_.name.value) == List("core")
    ,
    test("a duplicate name is a broken graph"):
      val source =
        """import histos.*
          |object build extends HistosBuild:
          |  def graph = Projects(module("core", Jvm)(scala"3.9.0"), module("core", Js)(scala"3.9.0"))
          |""".stripMargin
      ZIO.scoped:
        for loaded <- load(source).either
        yield assertTrue:
          loaded match
            case Left(LoadError.BrokenGraph(BuildError.DuplicateName(name, _, _))) => name.value == "core"
            case _                                                                  => false
    ,
    test("a type error is a compile failure"):
      val source =
        """import histos.*
          |object build extends HistosBuild:
          |  def graph = Projects(module("core", Jvm)(scala"nope"))
          |""".stripMargin
      ZIO.scoped:
        for loaded <- load(source).either
        yield assertTrue:
          loaded match
            case Left(LoadError.CompileFailed(diagnostics)) => diagnostics.exists(_.text.contains("nope"))
            case _                                          => false
    ,
    test("the build file cannot see the engine"):
      val source =
        """import histos.*
          |object build extends HistosBuild:
          |  def graph =
          |    val loader = histos.load.Loader
          |    Projects()
          |""".stripMargin
      ZIO.scoped:
        for loaded <- load(source).either
        yield assertTrue:
          loaded match
            case Left(LoadError.CompileFailed(_)) => true
            case _                                => false
    ,
    test("a missing build file is its own error"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- ZIO.attempt(Files.createTempDirectory("histos-empty")).mapError(t => LoadError.Io(LoadIo.Temp))
          _   <- ZIO.addFinalizer(deleteTree(dir))
          out <- ZIO.serviceWithZIO[Loader](_.load(dir, jar)).either
        yield assertTrue:
          out match
            case Left(LoadError.MissingFile(path)) => path.endsWith("histos.scala")
            case _                                 => false
  )

  def spec = loads.provideShared(Loader.live) @@ TestAspect.sequential
