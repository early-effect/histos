package histos.program

import histos.workspace.Workspace
import histos.{
  MainClass,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import java.nio.file.Path

import zio.ZIO
import zio.test.*

object ProgramLawSpec extends ZIOSpecDefault:
  def spec = suite("program laws")(
    test("a client is a native module named client that has a main, and it lands in dist"):
      val client = module("client", Platform.Native, Some(MainClass.trusted("demo.Exit")))
      val noMain = module("client", Platform.Native, None)
      val jvm    = module("client", Platform.Jvm, Some(MainClass.trusted("demo.Exit")))
      val other  = module("core", Platform.Native, Some(MainClass.trusted("demo.Exit")))
      val repo   = Path.of("/repo")
      assertTrue(
        ClientBinary.present(ProjectGraph.trusted(List(client))),
        !ClientBinary.present(ProjectGraph.trusted(List(noMain))),
        !ClientBinary.present(ProjectGraph.trusted(List(jvm))),
        !ClientBinary.present(ProjectGraph.trusted(List(other))),
        ClientBinary.destination(repo) == repo.resolve("dist").resolve("histos-client"),
      )
    ,
    test("a missing directory, an empty directory, a synthetic name, and garbage bytes have no main"):
      ZIO.scoped:
        Workspace
          .work("histos-mains")
          .flatMap: root =>
            val classes = root.resolve("classes")
            val file    = root.resolve("not-a-dir")
            for
              _       <- Workspace.writeString(file, "x")
              missing <- EntryPoints.mains(root.resolve("gone"))
              plain   <- EntryPoints.mains(file)
              _       <- Workspace.createDirectories(classes)
              empty   <- EntryPoints.mains(classes)
              _       <- Workspace.writeString(classes.resolve("Foo$$anon.class"), "not a class")
              _       <- Workspace.writeString(classes.resolve("Nope.class"), "not a class")
              found   <- EntryPoints.mains(classes)
            yield assertTrue(missing == Nil, plain == Nil, empty == Nil, found == Nil),
  )

  private def module(name: String, platform: Platform, application: Option[MainClass]): Module =
    Module(
      ProjectName.trusted(name),
      Platforms(platform, Nil),
      ScalaVersions(ScalaVersion.trusted("3.3.7"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
      application = application,
    )
end ProgramLawSpec
