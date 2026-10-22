package outside

import histos.*

import zio.test.*

object OutsideSpec extends ZIOSpecDefault:
  private val outside = suite("a build file outside the histos package")(
    test("uses the literals and the builder"):
      val core = module("core", Jvm, Js)(scala"3.9.0", scala"2.13.16")
        .libraryDependencies(dep"dev.zio::zio:2.1.26")
      val built = Projects(core)
      val modules = built.map(_.projects.collect:
        case item: Module =>
          (item.name.value, item.platforms.all, item.versions.all.map(_.value), item.dependencies, item.dependsOn))
      assertTrue(
        built.is(_.right).splits == Splits.aggregate,
        modules == Right(List(("core", List(Jvm, Js), List("3.9.0", "2.13.16"), List(Dep("dev.zio::zio:2.1.26")), Nil)))
      )
  )

  def spec = outside
