package histos.workflow

import histos.{Axis, Module, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos, Splits}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

object WorkflowSpec extends ZIOSpecDefault:
  private val v390 = ScalaVersion.trusted("3.9.0")
  private val core = project("core")
  private val tests = project("tests")

  private val render = suite("render")(
    test("the empty split set is one aggregate test job"):
      for
        golden  <- text("aggregate.yml")
        drifted <- text("drifted.yml")
        rendered = Workflow.render(graph(Splits.aggregate))
      yield assertTrue(
        rendered == Right(golden),
        golden.contains("./histos test") && !golden.contains("setup-java"),
        Workflow.differs(golden, drifted) == Left(CiError.Drift),
        Workflow.argv(TestJob.Aggregate(TestTarget.Entire)) == List("./histos", "test"),
      )
    ,
    test("a module split is a matrix, one cell per module"):
      val rendered = Workflow.render(graph(Splits.of(Axis.Module)))
      assertTrue:
        rendered match
          case Right(yaml) =>
            yaml.contains("fail-fast: false") &&
            yaml.contains("\"./histos test core\"") &&
            yaml.contains("\"./histos test tests\"") &&
            !yaml.contains("setup-java")
          case Left(_) => false
    ,
    test("update writes the render and check rejects a drifted file"):
      ZIO.scoped:
        for
          root <- temp
          file <- Workflow.update(root, graph(Splits.aggregate))
          read <- ZIO.attempt(Files.readString(file))
          again <- Workflow.check(root, graph(Splits.aggregate))
          wrote  = Files.isRegularFile(file)
          _ <- ZIO.attempt(Files.writeString(file, read + "\n"))
          drifted <- Workflow.check(root, graph(Splits.aggregate)).either
          gone <- Workflow.check(root.resolve("absent"), graph(Splits.aggregate)).either
        yield assertTrue(
          file == Workflow.path(root) && wrote,
          again == file,
          drifted == Left(CiError.Drift),
          gone match
            case Left(CiError.Missing(path)) => path == Workflow.path(root.resolve("absent"))
            case _                           => false
          ,
        )
  )

  private val affected = suite("affected")(
    test("a pull request runs the changed module and its dependents, not its dependencies"):
      val built = graph(Splits.aggregate)
      val plan = Workflow.plan(built, CiEvent.PullRequest(Set(core)))
      val leaf = Workflow.plan(built, CiEvent.PullRequest(Set(tests)))
      val none = Workflow.plan(built, CiEvent.PullRequest(Set.empty))
      val full = Workflow.plan(built, CiEvent.Other)
      assertTrue(
        plan == Right(List(TestJob.Aggregate(TestTarget.Named(core, List(tests))))),
        leaf == Right(List(TestJob.Aggregate(TestTarget.Named(tests, Nil)))),
        none == Right(Nil),
        full == Right(List(TestJob.Aggregate(TestTarget.Entire))),
      )
    ,
    test("an unknown project is not dropped"):
      val missing = project("missing")
      assertTrue(
        Workflow.plan(graph(Splits.aggregate), CiEvent.PullRequest(Set(missing))) == Left(CiError.Unknown(missing)),
      )
    ,
    test("a module matrix drops the unchanged module"):
      val built = graph(Splits.of(Axis.Module))
      val plan = Workflow.plan(built, CiEvent.PullRequest(Set(tests)))
      assertTrue(
        plan == Right(List(TestJob.Split(ModuleRef.One(tests), PlatformRef.All, VersionRef.All))),
      )
    ,
    test("a platform cell stays only when an affected module targets it"):
      val js = module("jsapp", Platform.Js, Nil)
      val jvm = module("core", Platform.Jvm, Nil)
      val built = ProjectGraph.trusted(List(jvm, js), Splits.of(Axis.Platform))
      val plan = Workflow.plan(built, CiEvent.PullRequest(Set(project("jsapp"))))
      assertTrue(
        plan == Right(List(TestJob.Split(ModuleRef.All, PlatformRef.One(Platform.Js), VersionRef.All))),
        Workflow.argv(TestJob.Split(ModuleRef.All, PlatformRef.One(Platform.Js), VersionRef.All)) ==
          List("./histos", "test", "--platform", "js"),
      )
  )

  private def graph(splits: Splits): ProjectGraph =
    val library = module("core", Platform.Jvm, Nil)
    val suite = module("tests", Platform.Jvm, List(core))
    ProjectGraph.trusted(List(library, suite), splits)

  private def module(name: String, platform: Platform, depends: List[ProjectName]): Module =
    Module(
      project(name),
      Platforms(platform, Nil),
      ScalaVersions(v390, Nil),
      Nil,
      depends,
      SourcePos("histos.scala", 1, 1),
    )

  private def project(raw: String): ProjectName =
    ProjectName.from(raw) match
      case Right(name) => name
      case Left(_)     => project("core")

  private def text(name: String): ZIO[Any, Throwable, String] =
    val candidates = List(
      Path.of("engine/test/fixtures/ci").resolve(name),
      Path.of("test/fixtures/ci").resolve(name),
    )
    ZIO.attempt(candidates.find(Files.isRegularFile(_))).flatMap:
      case Some(file) => ZIO.attempt(Files.readString(file))
      case None       => ZIO.fail(new java.io.FileNotFoundException(name))

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-workflow"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("workflow")(render, affected)
