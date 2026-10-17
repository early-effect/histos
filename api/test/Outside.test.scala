package outside

import histos.*

import zio.test.*

object OutsideSpec extends ZIOSpecDefault:
  private val outside = suite("a build file outside the histos package")(
    test("uses the literals and the builder"):
      val core = module("core", Jvm, Js)(scala"3.9.0", scala"2.13.16")
        .libraryDependencies(dep"dev.zio::zio:2.1.26")
      val built = Projects(core)
      assertTrue:
        built match
          case Right(ProjectGraph(List(Module(name, platforms, versions, dependencies, deps, _, _, _, _, _)), Splits.aggregate, _, _, _)) =>
            name.value == "core" &&
            platforms.all == List(Jvm, Js) &&
            versions.all.map(_.value) == List("3.9.0", "2.13.16") &&
            dependencies == List(Dep("dev.zio::zio:2.1.26")) &&
            deps.isEmpty
          case other => other.fold(_.message, _.projects.map(_.name.value).mkString(",")) == "core module"
  )

  def spec = outside
