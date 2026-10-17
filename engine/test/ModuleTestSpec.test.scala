package histos.testrun

import histos.jdk.{JdkMajor, JdkVersion}
import histos.resolve.{BuildResolvers, Resolve}
import histos.zinc.ResolvedJdk
import histos.{Dep, Jvm, Projects, ScalaVersion, TestMain, TestProvider, module}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object ModuleTestSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  private val jdk: ResolvedJdk =
    val home = Path.of(java.lang.System.getProperty("java.home"))
    val text = if Files.isRegularFile(home.resolve("release")) then Files.readString(home.resolve("release")) else ""
    JdkVersion.fromRelease(text) match
      case Right(version) => ResolvedJdk(home, version)
      case Left(_)        => ResolvedJdk(home, JdkVersion.trusted("21.0.2", JdkMajor.trusted(21)))

  private val ran = suite("subprocess")(
    test("a named main runs against main classes, and a module with nothing to run is not a pass"):
      ZIO.scoped:
        for
          root <- temp
          built <- ZIO.attempt {
                     val core = module("core", Jvm)(scala3)
                     val app = module("app", Jvm)(scala3).dependsOn(core).test(TestMain("demo.Check"))
                     write(root, "core/src/main/scala/demo/Core.scala", "package demo\n\nobject Core:\n  def n: Int = 1\n")
                     write(root, "app/src/main/scala/demo/App.scala", "package demo\n\nobject App:\n  def x: Int = Core.n\n")
                     write(root, "app/test/scala/demo/Check.scala", checkPasses)
                     write(root, "extra/src/main/scala/demo/Extra.scala", "package demo\n\nobject Extra:\n  def n: Int = 1\n")
                     write(root, "extra/test/place.txt", "not scala")
                     write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\ncell app 3.10.0 jvm\ncell extra 3.10.0 jvm\n")
                     Projects(core, module("extra", Jvm)(scala3), app)
                   }.flatMap(graph => ZIO.fromEither(graph).mapError(error => new IllegalStateException(error.message)))
          cells <- ModuleTest.run(root, built, Nil, jdk).provide(Resolve.live(root.resolve("coursier")))
          named <- ModuleTest.run(root, built, List("app"), jdk).provide(Resolve.live(root.resolve("coursier")))
          mainEntries <- ZIO.attempt(entries(root.resolve(".histos/classes/app/3.10.0/jvm")))
          testEntries <- ZIO.attempt(entries(root.resolve(".histos/test-classes/app/3.10.0/jvm")))
        yield assertTrue(
          cells.map(cell => (cell.module.value, cell.status)) == List(
            ("app", TestStatus.Passed),
            ("core", TestStatus.NoTests),
            ("extra", TestStatus.NoTests),
          ),
          named.map(cell => (cell.module.value, cell.status)) == List(("app", TestStatus.Passed)),
          !mainEntries.exists(_.endsWith("Check.class")),
          testEntries.exists(_.endsWith("Check.class")),
        )
    ,
    test("a single module uses test at the repository root"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
                 write(root, "src/main/scala/demo/Hello.scala", "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n")
                 write(root, "test/scala/demo/Check.scala", helloPasses)
                 write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
               }
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3).test(TestMain("demo.Check")))).mapError(error => new IllegalStateException(error.message))
          cells <- ModuleTest.run(root, built, Nil, jdk).provide(Resolve.live(root.resolve("coursier")))
        yield assertTrue:
          cells.map(cell => (cell.module.value, cell.version.value, cell.status)) == List(("core", "3.10.0", TestStatus.Passed))
    ,
    test("a non-zero main is a failed cell and its output is kept"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
                 write(root, "src/main/scala/demo/Hello.scala", "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n")
                 write(root, "test/scala/demo/Check.scala", helloFails)
                 write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
               }
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3).test(TestMain("demo.Check")))).mapError(error => new IllegalStateException(error.message))
          cells <- ModuleTest.run(root, built, Nil, jdk).provide(Resolve.live(root.resolve("coursier")))
        yield assertTrue:
          cells match
            case List(TestedCell(_, version, TestStatus.Failed(exit, out, _))) =>
              version.value == "3.10.0" && exit == 1 && out.contains("nope")
            case _ => false
    ,
  )

  private val providers = suite("provider")(
    test("ZIO Test is the default provider, and a failing spec is not a pass"):
      ZIO.scoped:
        for
          root <- temp
          zioDep = Dep("dev.zio::zio-test-sbt:2.1.27")
          core = module("core", Jvm)(scala3).testDependencies(zioDep)
          bad = module("bad", Jvm)(scala3).testProvider(TestProvider.Zio).testDependencies(zioDep)
          built <- ZIO.fromEither(Projects(core, bad)).mapError(error => new IllegalStateException(error.message))
          _ <- ZIO.attempt {
                 write(root, "core/src/main/scala/demo/Hello.scala", "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n")
                 write(root, "core/test/scala/demo/Check.scala", passSpec)
                 write(root, "bad/src/main/scala/demo/Hello.scala", "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n")
                 write(root, "bad/test/scala/demo/Check.scala", failSpec)
               }
          _ <- Resolve.update(root, built, BuildResolvers(built.resolvers)).provide(Resolve.live(root.resolve("coursier")))
          cells <- ModuleTest.run(root, built, Nil, jdk).provide(Resolve.live(root.resolve("coursier")))
        yield assertTrue:
          cells match
            case List(TestedCell(failed, _, TestStatus.Failed(exit, out, err)), TestedCell(passed, _, TestStatus.Passed)) =>
              failed.value == "bad" && passed.value == "core" && exit == 1 && (out + err).contains("nope")
            case _ => false
  )

  private val refused = suite("refused before the subprocess")(
    test("test sources without a provider library are refused"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
                 write(root, "src/main/scala/demo/Hello.scala", "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n")
                 write(root, "test/scala/demo/Check.scala", helloPasses)
                 write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
               }
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3))).mapError(error => new IllegalStateException(error.message))
          out <- ModuleTest.run(root, built, Nil, jdk).provide(Resolve.live(root.resolve("coursier"))).either
        yield assertTrue:
          out match
            case Left(ModuleTestError.ProviderDependency(module, provider)) =>
              module.value == "core" && provider == TestProvider.Zio
            case _ => false
    ,
    test("a named main without test sources is refused"):
      ZIO.scoped:
        for
          root <- temp
          _ <- ZIO.attempt {
                 write(root, "src/main/scala/demo/Hello.scala", "package demo\n\nobject Hello:\n  def greet: String = \"hi\"\n")
                 write(root, ".histos/dependencies.lock", "cell core 3.10.0 jvm\n")
               }
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3).test(TestMain("demo.Check")))).mapError(error => new IllegalStateException(error.message))
          out <- ModuleTest.run(root, built, Nil, jdk).provide(Resolve.live(root.resolve("coursier"))).either
        yield assertTrue:
          out match
            case Left(ModuleTestError.MissingSources(module, main)) =>
              module.value == "core" && main.value == "demo.Check"
            case _ => false
    ,
  )

  private val passSpec =
    """import zio.test.*
      |
      |object Check extends ZIOSpecDefault:
      |  def spec = suite("check")(
      |    test("adds") {
      |      assertTrue(1 + 1 == 2)
      |    }
      |  )
      |""".stripMargin

  private val failSpec =
    """import zio.test.*
      |
      |object Check extends ZIOSpecDefault:
      |  def spec = suite("check")(
      |    test("nope") {
      |      assertTrue(1 == 2)
      |    }
      |  )
      |""".stripMargin

  private val helloPasses =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    if demo.Hello.greet != "hi" then System.exit(2)
      |""".stripMargin

  private val helloFails =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    System.out.println("nope")
      |    System.exit(1)
      |""".stripMargin

  private val checkPasses =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    if App.x != 1 then System.exit(2)
      |""".stripMargin

  private def entries(classes: Path): List[String] =
    if !Files.isDirectory(classes) then Nil
    else
      val walk = Files.walk(classes)
      try
        walk.iterator.asScala
          .filter(Files.isRegularFile(_))
          .map(path => classes.relativize(path).toString.replace('\\', '/'))
          .toList
          .sorted
      finally walk.close()

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-test-run"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = suite("module test")(ran, providers, refused) @@ TestAspect.sequential @@ TestAspect.timeout(6.minutes)
