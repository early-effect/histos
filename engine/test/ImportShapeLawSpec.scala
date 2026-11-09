package histos.scaffold

import zio.test.{ZIOSpecDefault, assertTrue, test}

/** A draft keeps a directory, a shared dependency, a cycle note, and a legal Scala ident. */
object ImportShapeLawSpec extends ZIOSpecDefault:
  def spec = suite("import shape laws")(
    test("a directory, a shared dependency, and a differing scala version stay on the draft"):
      val placed = ImportDraft.sbt(
        """lazy val core = project.in(file("modules/core"))
          |""".stripMargin,
        "",
        "app",
      )
      val escaped = ImportDraft.sbt(
        """lazy val core = project.in(file("../escape"))
          |""".stripMargin,
        "",
        "app",
      )
      val shared = ImportDraft.sbt(
        """ThisBuild / libraryDependencies += "dev.zio" %% "zio" % "2.1.27"
          |lazy val core = project
          |lazy val root = project.aggregate(core)
          |""".stripMargin,
        "",
        "app",
      )
      val pinned = ImportDraft.sbt(
        """val chosen = "3.3.7"
          |ThisBuild / scalaVersion := chosen
          |lazy val core = project
          |""".stripMargin,
        "",
        "app",
      )
      val local = ImportDraft.sbt(
        """ThisBuild / scalaVersion := "3.10.0"
          |lazy val core = project.settings(
          |  scalaVersion := "3.3.4"
          |)
          |""".stripMargin,
        "",
        "app",
      )
      assertTrue(
        placed.contains(""".in(path"modules/core")"""),
        escaped.contains("// unmapped: directory ../escape"),
        shared.contains("// ThisBuild dependencies were copied onto each module"),
        shared.contains("""dep"dev.zio::zio:2.1.27""""),
        shared.contains("""aggregate("root", core)"""),
        pinned.contains("""scala"3.3.7""""),
        local.contains("""scala"3.10.0""""),
        local.contains("// unmapped: module scala version 3.3.4"),
      )
    ,
    test("a cycle drops the closing edge, and a keyword or a repeated ident is renamed"):
      val cycle = ImportDraft.sbt(
        """lazy val a = project.dependsOn(b)
          |lazy val b = project.dependsOn(a)
          |""".stripMargin,
        "",
        "app",
      )
      val keyword  = ImportDraft.sbt("lazy val type = project\n", "", "app")
      val versions =
        """val zio = "2.1.27"
          |val artifact = "dev.zio::zio:2.1.27"
          |Lib(artifact)
          |Lib(s"dev.zio::zio:$zio")
          |""".stripMargin
      val one  = ImportDraft.zipx(versions, List("my-core"), "app")
      val both = ImportDraft.zipx("", List("my-core", "my_core"), "app")
      assertTrue(
        cycle.contains("// unmapped: a depends on b"),
        cycle.contains(".dependsOn(a)"),
        !cycle.contains(".dependsOn(b)"),
        keyword.contains("""val typeModule = module("type", Jvm)"""),
        one.contains("""val my_core = module("my-core", Jvm)"""),
        one.contains("""dep"dev.zio::zio:2.1.27""""),
        both.contains("""val my_core = module("my-core", Jvm)"""),
        both.contains("""val my_core1 = module("my_core", Jvm)"""),
      ),
  )
