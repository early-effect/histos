package histos

import _root_.scala.compiletime.testing.typeCheckErrors
import zio.test.*

object JsVersionSpec extends ZIOSpecDefault:
  def spec = suite("Scala.js revision")(
    test("a build can pin its Scala.js toolchain"):
      val errors = typeCheckErrors("""import histos.*
        module("web", Platform.Js)(ScalaVersion("3.10.0")).jsVersion(JsVersion("1.22.0"))
      """)
      assertTrue(errors.isEmpty)
    ,
    test("runtime parsing retains typed errors and exact revisions"):
      assertTrue(
        JsVersion.from("") == Left(JsVersionError.Empty),
        JsVersion.from("1.22") == Left(JsVersionError.NotExact("1.22")),
        JsVersion.from("1.22.0").map(_.value) == Right("1.22.0"),
        JsVersion.from("1.22.1-SNAPSHOT").map(_.value) == Right(
          "1.22.1-SNAPSHOT"
        ),
        typeCheckErrors("histos.JsVersion(\"1.+\")").nonEmpty
      )
    ,
    test("the build catalog and module overrides remain distinct data"):
      val defaults = ToolCatalog.default.copy(js =
        ToolCatalog.default.js.copy(revision = JsVersion("1.21.0"))
      )
      val web = module("web", Platform.Js)(ScalaVersion("3.10.0"))
        .jsVersion(JsVersion("1.22.0"))
      val graph = Projects.tools(defaults)(web)
      assertTrue(
        graph.map(_.tools) == Right(defaults),
        graph.map(_.projects.collect { case module: Module =>
          module.jsVersion.map(_.value)
        }) == Right(List(Some("1.22.0")))
      )
    ,
    test("a Js revision on a JVM-only module is a typed graph error"):
      val app = module("app", Platform.Jvm)(ScalaVersion("3.10.0"))
        .jsVersion(JsVersion("1.22.0"))
      zio.ZIO
        .fromEither(ProjectName.from("app"))
        .map(name =>
          assertTrue(
            Projects(app) == Left(
              BuildError.JsVersionWithoutJs(name, app.position)
            )
          )
        )
  )
