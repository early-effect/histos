package histos.scaffold

import zio.test.*

object ImportDraftLawSpec extends ZIOSpecDefault:
  private val scala = Skeleton.scalaVersion
  private val zio   = Skeleton.zioVersion

  def spec = suite("import draft laws")(
    test("an sbt build becomes the module graph the draft names"):
      val build =
        s"""ThisBuild / organization := "com.example"
           |ThisBuild / scalaVersion := "$scala"
           |lazy val core = project.settings(
           |  libraryDependencies += "dev.zio" %% "zio" % "$zio"
           |)
           |lazy val app = project.dependsOn(core).settings(
           |  libraryDependencies += "dev.zio" %% "zio" % "$zio" % Test,
           |  Compile / mainClass := Some("demo.Main")
           |)
           |""".stripMargin
      val draft = ImportDraft.sbt(build, "", "fallback")
      assertTrue(
        draft.contains("""module("core", Jvm)"""),
        draft.contains("""module("app", Jvm)"""),
        draft.contains(".dependsOn(core)"),
        draft.contains("""organization"com.example""""),
        draft.contains(s"""scala"$scala""""),
        draft.contains(s"""dep"dev.zio::zio:$zio""""),
        draft.contains(".testDependencies("),
        draft.contains(""".main(MainClass("demo.Main"))"""),
        !draft.contains("// unmapped:"),
      )
    ,
    test("a setting the draft does not map stays an unmapped line"):
      val badScala = ImportDraft.sbt(
        """ThisBuild / scalaVersion := "nope"
          |lazy val core = project
          |""".stripMargin,
        "",
        "app",
      )
      val plugin = ImportDraft.sbt(
        "lazy val core = project\n",
        """addSbtPlugin("org.example" % "sbt-demo" % "1.0.0")""",
        "app",
      )
      val runtime = ImportDraft.sbt(
        """lazy val core = project.settings(
          |  libraryDependencies += "com.acme" % "widgets" % "1.0.0" % "runtime"
          |)
          |""".stripMargin,
        "",
        "app",
      )
      val excluded = ImportDraft.sbt(
        """lazy val core = project.settings(
          |  libraryDependencies += "com.acme" % "widgets" % "1.0.0" exclude("org.foo", "bar")
          |)
          |""".stripMargin,
        "",
        "app",
      )
      val named = ImportDraft.sbt("lazy val _core = project\n", "", "app")
      assertTrue(
        badScala.contains("// unmapped: scalaVersion nope"),
        badScala.contains(s"""scala"$scala""""),
        plugin.contains("""// unmapped: addSbtPlugin("org.example" % "sbt-demo" % "1.0.0")"""),
        runtime.contains("""// unmapped: libraryDependencies += "com.acme" % "widgets" % "1.0.0" % "runtime""""),
        excluded.contains(
          """// unmapped: libraryDependencies += "com.acme" % "widgets" % "1.0.0" exclude("org.foo", "bar")"""
        ),
        named.contains("// unmapped: project _core"),
        named.contains("""module("app", Jvm)"""),
      )
    ,
    test("a zipx library lands on one module, and two modules leave it unmapped"):
      val versions = s"""Lib("dev.zio::zio:$zio")"""
      val one      = ImportDraft.zipx(versions, List("app"), "fallback")
      val two      = ImportDraft.zipx(versions, List("core", "app"), "fallback")
      val pinned   = ImportDraft.zipx("""Pin("sbt" -> "1.0.0")""", List("app"), "app")
      val bad      = ImportDraft.zipx("""val scalaVersion = "nope"""", Nil, "app")
      assertTrue(
        one.contains(s"""scala"$scala""""),
        one.contains(".libraryDependencies("),
        one.contains(s"""dep"dev.zio::zio:$zio""""),
        !one.contains("does not say which module"),
        two.contains("// unmapped: ZipxVersions does not say which module owns a library"),
        two.contains(s"""// unmapped: dep"dev.zio::zio:$zio""""),
        !two.contains(".libraryDependencies"),
        pinned.contains("""// unmapped: Pin "sbt" -> "1.0.0""""),
        bad.contains("// unmapped: scalaVersion nope"),
        bad.contains("// unmapped: ZipxVersions does not name modules"),
        bad.contains(s"""scala"$scala""""),
      ),
  )
end ImportDraftLawSpec
