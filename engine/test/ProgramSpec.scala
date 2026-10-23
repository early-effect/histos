package histos.program

import histos.resolve.Resolve
import histos.zinc.GraphCompile
import histos.{ClangMajor, Js, Jvm, MainClass, Native, Projects, Requires, ScalaVersion, TestMachine, Tool, module}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object ProgramSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  private val ran = suite("entry")(
    test("a module without a main is not linked"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
          here <- TestMachine.runtimes(root)
          out <- Program.console(root, built, "core", jdk, here).runCollect.provide(Resolve.live(root.resolve("coursier"))).either
        yield assertTrue:
          out match
            case Left(ProgramError.NoMain(name)) => name.value == "core"
            case _                                => false
    ,
    test("a jvm main runs on the main classpath"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
                 write(root, "app/src/main/scala/demo/Check.scala", hi)
                 write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
               }
          here <- TestMachine.runtimes(root)
          feeds <- Program.console(root, built, "app", jdk, here).runCollect.provide(Resolve.live(root.resolve("coursier")))
          mainEntries <- ZIO.attempt(entries(root.resolve(".histos/classes/app/3.10.0/jvm")))
        yield assertTrue(
          feeds.collect { case ProgramFeed.Out(text) => text }.mkString("\n").contains("hi"),
          feeds.collect { case ProgramFeed.Exited(_, _, platform, code) => (platform, code) } == List((Jvm, 0)),
          mainEntries.exists(_.endsWith("Check.class")),
        )
    ,
    test("a js main links and starts in node"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Js)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
                 write(root, "app/src/main/scala/demo/Check.scala", hi)
                 write(root, ".histos/dependencies.lock", "cell app 3.10.0 js\n")
               }
          chosen <- TestMachine.chosenNode(root)
          here <- TestMachine.runtimes(root)
          feeds <- Program.console(root, built, "app", jdk, here).runCollect.provide(Resolve.live(root.resolve("coursier")))
          ran <- TestMachine.launches(chosen)
        yield assertTrue(
          feeds.collect { case ProgramFeed.Out(text) => text }.mkString("\n").contains("hi"),
          feeds.collect { case ProgramFeed.Exited(_, _, platform, code) => (platform, code) } == List((Js, 0)),
          ran.nonEmpty,
        )
    @@ Requires(Tool.Node)
    ,
    test("a multi-platform module does not start until a platform is named"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm, Js)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          here <- TestMachine.runtimes(root)
          out <- Program.console(root, built, "app", jdk, here).runCollect.provide(Resolve.live(root.resolve("coursier"))).either
          classes <- ZIO.attempt(Files.exists(root.resolve(".histos").resolve("classes")))
        yield assertTrue:
          out match
            case Left(error @ ProgramError.Choose(name, platforms)) =>
              name.value == "app" &&
              platforms == List(Jvm, Js) &&
              !classes &&
              error.message.contains("histos run app jvm")
            case _ => false
    ,
    test("a platform the module does not build is refused"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          here <- TestMachine.runtimes(root)
          out <- Program.console(root, built, "app", jdk, here, Some(Native)).runCollect.provide(Resolve.live(root.resolve("coursier"))).either
          classes <- ZIO.attempt(Files.exists(root.resolve(".histos").resolve("classes")))
        yield assertTrue:
          out match
            case Left(ProgramError.Unbuilt(name, platform, builtOn)) =>
              name.value == "app" && platform == Native && builtOn == List(Jvm) && !classes
            case _ => false
    ,
    test("an explicit platform runs only that platform"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Jvm, Js)(scala3).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
                 write(root, "app/src/main/scala/demo/Check.scala", hi)
                 write(root, ".histos/dependencies.lock", "cell app 3.10.0 jvm\n")
               }
          here <- TestMachine.runtimes(root)
          feeds <- Program.console(root, built, "app", jdk, here, Some(Jvm)).runCollect.provide(Resolve.live(root.resolve("coursier")))
          jsClasses <- ZIO.attempt(Files.exists(root.resolve(".histos/classes/app/3.10.0/js")))
        yield assertTrue(
          feeds.collect { case ProgramFeed.Out(text) => text }.mkString("\n").contains("hi"),
          feeds.collect { case ProgramFeed.Exited(_, _, platform, code) => (platform, code) } == List((Jvm, 0)),
          !jsClasses,
        )
    ,
    test("a native main links and runs the binary"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          app = module("app", Native)(ScalaVersion("3.9.0")).clang(ClangMajor(21)).main(MainClass("demo.Check"))
          built <- ZIO.fromEither(Projects(app))
          _ <- ZIO.attempt {
                 write(root, "app/src/main/scala/demo/Check.scala", hi)
                 write(root, ".histos/dependencies.lock", "cell app 3.9.0 native\n")
               }
          here <- TestMachine.runtimes(root)
          feeds <- Program.console(root, built, "app", jdk, here).runCollect.provide(Resolve.live(root.resolve("coursier")))
        yield assertTrue(
          feeds.collect { case ProgramFeed.Out(text) => text }.mkString("\n").contains("hi"),
          feeds.collect { case ProgramFeed.Exited(_, _, platform, code) => (platform, code) } == List((Native, 0)),
        )
    @@ Requires(Tool.Clang(ClangMajor(21)))
    ,
  )

  private val hi =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    println("hi")
      |""".stripMargin

  private def entries(classes: Path): List[String] =
    if !Files.isDirectory(classes) then Nil
    else
      val walk = Files.walk(classes)
      try
        walk.iterator.asScala.filter(Files.isRegularFile(_)).map(path => classes.relativize(path).toString.replace('\\', '/')).toList
      finally walk.close()

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-program"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = ran @@ TestAspect.withLiveSystem @@ TestAspect.timeout(12.minutes)
