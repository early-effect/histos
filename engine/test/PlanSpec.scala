package histos.preview

import histos.workflow.CiError
import histos.{
  ClangMajor,
  Jvm,
  Js,
  Native,
  Module,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  Projects,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  aggregate,
  module
}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

object PlanSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val older = ScalaVersion("3.8.0")
  private val at = SourcePos("histos.scala", 1, 1)

  private val plans = suite("compile preview")(
    test(
      "a selection lists its upstream and omits the rest, and writes nothing"
    ):
      ZIO.scoped {
        for
          repo <- keep("histos-preview")
          _ <- write(repo, "core/src/main/scala/Core.scala", "object Core")
          _ <- write(repo, "app/src/main/scala/App.scala", "object App")
          _ <- write(repo, "app/src/test/scala/AppSpec.scala", "object AppSpec")
          _ <- write(repo, "idle/src/main/scala/Idle.scala", "object Idle")
          core = module("core", Jvm)(scala3)
          app = module("app", Jvm)(scala3).dependsOn(core)
          idle = module("idle", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(core, app, idle))
          plan <- Plan.compile(repo, graph, List("app"))
          text = Plan.render(repo, plan)
          shown <- Plan.show(repo, graph, List("app"))
        yield assertTrue(
          shown == text,
          plan.staleness == FileStaleness.Unknown,
          text ==
            """file-level staleness: unknown
              |core 3.10.0 jvm upstream
              |upstreams
              |sources core/src/main/scala/Core.scala
              |app 3.10.0 jvm selected
              |upstreams core
              |sources app/src/main/scala/App.scala""".stripMargin,
          plan.units.collect { case PlanUnit.NotSelected(name) =>
            name.value
          } == List("idle"),
          !Files.exists(repo.resolve(".histos"))
        )
      }
    ,
    test(
      "cells follow numeric versions and Jvm, Js, Native, with platform upstreams"
    ):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-cells")
          core = module("core", Jvm)(scala3)
          app = module("app", Native, Js, Jvm)(scala3, older)
            .dependsOn(Jvm)(core)
            .clang(ClangMajor(21))
          graph <- ZIO.fromEither(Projects(core, app))
          plan <- Plan.compile(repo, graph, List("app"))
        yield assertTrue(
          plan.staleness == FileStaleness.Unknown,
          shapes(plan) == List(
            ("core", "3.10.0", "jvm", Reason.Upstream, Nil),
            ("app", "3.8.0", "jvm", Reason.Selected, List("core")),
            ("app", "3.8.0", "js", Reason.Selected, Nil),
            ("app", "3.8.0", "native", Reason.Selected, Nil),
            ("app", "3.10.0", "jvm", Reason.Selected, List("core")),
            ("app", "3.10.0", "js", Reason.Selected, Nil),
            ("app", "3.10.0", "native", Reason.Selected, Nil)
          ),
          plan.units.forall:
            case chosen: PlanUnit.Chosen => chosen.sources.isEmpty
            case PlanUnit.NotSelected(_) => true
          ,
          !Files.exists(repo.resolve(".histos"))
        )
      }
    ,
    test("no names selects every module"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-all")
          core = module("core", Jvm)(scala3)
          app = module("app", Jvm)(scala3).dependsOn(core)
          graph <- ZIO.fromEither(Projects(core, app))
          plan <- Plan.compile(repo, graph, Nil)
        yield assertTrue(
          shapes(plan).map(_._4) == List(Reason.Selected, Reason.Selected),
          shapes(plan).map(_._5) == List(Nil, List("core"))
        )
      }
    ,
    test("an aggregate selects its modules and still lists upstreams"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-agg")
          core = module("core", Jvm)(scala3)
          app = module("app", Jvm)(scala3).dependsOn(core)
          root = aggregate("root", core, app)
          graph <- ZIO.fromEither(Projects(root, core, app))
          plan <- Plan.compile(repo, graph, List("root"))
        yield assertTrue(
          shapes(plan) == List(
            ("core", "3.10.0", "jvm", Reason.Selected, Nil),
            ("app", "3.10.0", "jvm", Reason.Selected, List("core"))
          )
        )
      }
    ,
    test(
      "one module reads repository src, and its own src wins when both exist"
    ):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-src")
          _ <- write(repo, "src/main/scala/Shared.scala", "object Shared")
          only = module("only", Jvm)(scala3)
          alone <- ZIO.fromEither(Projects(only))
          shared <- Plan.compile(repo, alone, Nil)
          _ <- write(repo, "only/src/main/scala/Own.scala", "object Own")
          named <- Plan.compile(repo, alone, Nil)
        yield assertTrue(
          relativeSources(repo, shared) == List("src/main/scala/Shared.scala"),
          relativeSources(repo, named) == List("only/src/main/scala/Own.scala")
        )
      }
    ,
    test("unknown, duplicate, and a bad name are target errors"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-names")
          core = module("core", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(core))
          missing <- ZIO.fromEither(ProjectName.from("missing"))
          coreName <- ZIO.fromEither(ProjectName.from("core"))
          unknown <- Plan.compile(repo, graph, List("missing")).either
          duplicate <- Plan.compile(repo, graph, List("core", "core")).either
          bad <- Plan.compile(repo, graph, List("1core")).either
        yield assertTrue(
          unknown == Left(PreviewError.Target(CiError.Unknown(missing))),
          duplicate == Left(PreviewError.Target(CiError.Duplicate(coreName))),
          bad == Left(PreviewError.Target(CiError.BadName("1core")))
        )
      }
    ,
    test("a cycle is a cycle error and writes nothing"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-cycle")
          left <- ZIO.fromEither(ProjectName.from("left"))
          right <- ZIO.fromEither(ProjectName.from("right"))
          graph = ProjectGraph.trusted(
            List(
              cell(left, List(right)),
              cell(right, List(left))
            )
          )
          planned <- Plan.compile(repo, graph, List("left")).either
        yield assertTrue(
          planned == Left(PreviewError.Cycle(left)),
          !Files.exists(repo.resolve(".histos"))
        )
      }
  )

  private def shapes(
      plan: CompilePlan
  ): List[(String, String, String, Reason, List[String])] =
    plan.units.collect { case chosen: PlanUnit.Chosen =>
      (
        chosen.module.value,
        chosen.version.value,
        chosen.platform.token,
        chosen.reason,
        chosen.upstreams.map(_.value)
      )
    }

  private def relativeSources(repo: Path, plan: CompilePlan): List[String] =
    plan.units.flatMap:
      case chosen: PlanUnit.Chosen =>
        chosen.sources.map: path =>
          repo.toAbsolutePath.normalize
            .relativize(path.toAbsolutePath.normalize)
            .iterator
            .asScala
            .map(_.toString)
            .mkString("/")
      case PlanUnit.NotSelected(_) => Nil

  private def cell(name: ProjectName, upstream: List[ProjectName]): Module =
    Module(
      name,
      Platforms(Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      upstream.map(item => ProjectDep(item, None)),
      at
    )

  private def keep(prefix: String): ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory(prefix)))(delete)

  private def write(
      root: Path,
      name: String,
      text: String
  ): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val path = root.resolve(name)
      Files.createDirectories(path.getParent)
      Files.writeString(path, text)
    }

  private def delete(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  def spec = plans
