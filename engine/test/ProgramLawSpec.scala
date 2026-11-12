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

import java.nio.file.{Files, Path}

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.{UIO, ZIO}
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
    test(
      "a public static main is found once by its object name, and a synthetic, private, instance, or broken one is not"
    ):
      ZIO.scoped:
        Workspace
          .work("histos-mains")
          .flatMap: root =>
            val classes = root.resolve("classes")
            val file    = root.resolve("not-a-dir")
            val main    = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC
            for
              _       <- Workspace.writeString(file, "x")
              missing <- EntryPoints.mains(root.resolve("gone"))
              plain   <- EntryPoints.mains(file)
              _       <- Workspace.createDirectories(classes)
              empty   <- EntryPoints.mains(classes)
              _       <- emit(classes, "demo/Main", Some(main))
              _       <- emit(classes, "demo/Main$", Some(main))
              _       <- emit(classes, "demo/Foo$$anon", Some(main))
              _       <- emit(classes, "demo/Hidden", Some(Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC))
              _       <- emit(classes, "demo/Instance", Some(Opcodes.ACC_PUBLIC))
              _       <- emit(classes, "demo/Plain", None)
              _       <- Workspace.writeString(classes.resolve("Nope.class"), "not a class")
              found   <- EntryPoints.mains(classes)
            yield assertTrue(missing == Nil, plain == Nil, empty == Nil, found == List("demo.Main")),
  )

  /** A public class named `name`, with a `main(String[])` of that access when one is given. */
  private def emit(classes: Path, name: String, main: Option[Int]): UIO[Unit] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", Array.empty[String])
    main.foreach(access => writer.visitMethod(access, "main", "([Ljava/lang/String;)V", null, null).visitEnd())
    writer.visitEnd()
    val file = classes.resolve(s"$name.class")
    ZIO
      .attemptBlocking {
        Files.createDirectories(Option(file.getParent).getOrElse(classes))
        Files.write(file, writer.toByteArray)
      }
      .orDie
      .unit

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
