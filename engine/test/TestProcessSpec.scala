package histos.testrun

import histos.Platform
import histos.pack.{MainClass, MainClassError}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object TestProcessSpec extends ZIOSpecDefault:
  private val javaHome = Path.of(java.lang.System.getProperty("java.home"))
  private val javaBin  = javaHome.resolve("bin").resolve("java")
  private val exitMain = MainClass.trusted("demo.Exit")
  private val sleepMain = MainClass.trusted("demo.Sleeper")

  private val types = suite("request")(
    test("a main class is a binary name"):
      assertTrue(
        MainClass.from("demo.Exit") == Right(exitMain),
        MainClass.from("demo.Exit$") == Right(MainClass.trusted("demo.Exit$")),
        MainClass.from("") == Left(MainClassError.Empty),
        MainClass.from("1bad") == Left(MainClassError.Bad("1bad")),
      )
    ,
    test("a suite keeps the repo, PATH, and HOME"):
      val env = ModuleTest.suiteEnv(Path.of("/repo"))
      assertTrue(
        env.get("HISTOS_REPO").contains(Path.of("/repo").toAbsolutePath.normalize.toString),
        env.get("PATH") == sys.env.get("PATH").filter(_.nonEmpty),
        env.get("HOME") == sys.env.get("HOME").filter(_.nonEmpty),
        !env.contains("USER"),
      )
    ,
    test("js is rejected before a process starts"):
      ZIO.scoped:
        for
          work <- temp
          out  <- TestProcess.run(request(work, Platform.Js, exitMain, TestClasspath(work, Nil))).either
        yield assertTrue(out == Left(TestError.UnsupportedPlatform(Platform.Js)))
    ,
    test("a missing java executable is its own error"):
      ZIO.scoped:
        for
          work <- temp
          out  <- TestProcess.run(request(work, Platform.Jvm, exitMain, TestClasspath(work, Nil)).copy(java = work.resolve("missing-java"))).either
        yield assertTrue:
          out match
            case Left(TestError.Java(path)) => path.endsWith("missing-java")
            case _                          => false
  )

  private val process = suite("subprocess")(
    test("System.exit stays in the child, the environment is empty, and the resolved jar is on the classpath"):
      ZIO.scoped:
        for
          root    <- temp
          classes <- ZIO.attempt(Files.createDirectories(root.resolve("classes")))
          built   <- ZIO.attempt(libJar(root))
          compiled <- ZIO.attempt(compile(root, classes, "Exit.java", exitSource, List(built.jar)))
          work    <- ZIO.attempt(Files.createDirectories(root.resolve("work")).toRealPath())
          parent   = Option(java.lang.System.getenv("HOME")).filter(_.nonEmpty)
          outcome <- TestProcess.run(request(work, Platform.Jvm, exitMain, TestClasspath(classes, List(built.jar)))).either
          lines    = outcome.map(_.out.text.linesIterator.map(_.trim).filter(_.nonEmpty).toList)
          scrubbed = lines match
            case Right(found) => found.contains("home=null") && parent.forall(home => !found.contains(s"home=$home"))
            case _            => false
        yield assertTrue(
          built.tool.code == 0 && built.tool.text.isEmpty,
          compiled.code == 0 && compiled.text.isEmpty,
          outcome.map(_.exit) == Right(ProcessExit(7)),
          lines == Right(List("hi", "home=null", s"dir=${work.toString}")),
          scrubbed,
        )
    ,
    test("cancel kills the sleeping test and the child in its process group"):
      ZIO.scoped:
        for
          root    <- temp
          classes <- ZIO.attempt(Files.createDirectories(root.resolve("classes")))
          compiled <- ZIO.attempt(compile(root, classes, "Sleeper.java", sleeperSource, Nil))
          work     <- ZIO.attempt(Files.createDirectories(root.resolve("work")).toRealPath())
          sleepOk   = Files.isRegularFile(Path.of("/bin/sleep"))
          fiber    <- TestProcess.run(request(work, Platform.Jvm, sleepMain, TestClasspath(classes, Nil))).fork
          child    <- awaitPid(work.resolve("child.pid"))
          self     <- awaitPid(work.resolve("self.pid"))
          childBefore = child.exists(alive)
          selfBefore  = self.exists(alive)
          exit     <- fiber.interrupt
          childDead <- ZIO.foreach(child.toList)(awaitDead)
          selfDead  <- ZIO.foreach(self.toList)(awaitDead)
        yield assertTrue(
          compiled.code == 0 && compiled.text.isEmpty && sleepOk,
          child.nonEmpty && self.nonEmpty && childBefore && selfBefore,
          exit.isInterrupted,
          childDead == List(true) && selfDead == List(true),
        )
  )

  private def request(work: Path, platform: Platform, main: MainClass, classpath: TestClasspath): TestRequest =
    TestRequest(platform, javaBin, classpath, main, work)

  private def alive(pid: Long): Boolean =
    ProcessHandle.of(pid).toScala.exists(_.isAlive)

  private def awaitPid(path: Path): ZIO[Any, Nothing, Option[Long]] =
    ZIO.suspendSucceed {
      readPid(path) match
        case Some(pid) => ZIO.succeed(Some(pid))
        case None      => ZIO.sleep(20.millis) *> awaitPid(path)
    }.timeout(2.seconds).map(_.flatten)

  private def readPid(path: Path): Option[Long] =
    if !Files.isRegularFile(path) then None
    else
      try
        val raw = Files.readString(path).trim
        raw.toLongOption.filter(_ > 0)
      catch case _: java.io.IOException => None

  private def awaitDead(pid: Long): ZIO[Any, Nothing, Boolean] =
    ZIO.suspendSucceed {
      if alive(pid) then ZIO.sleep(20.millis) *> awaitDead(pid) else ZIO.succeed(true)
    }.timeout(1.second).map(_.getOrElse(false))

  private val exitSource =
    """package demo;
      |public final class Exit {
      |  public static void main(String[] args) {
      |    System.out.println(Lib.hi());
      |    System.out.println("home=" + System.getenv("HOME"));
      |    System.out.println("dir=" + System.getProperty("user.dir"));
      |    System.out.flush();
      |    System.exit(7);
      |  }
      |}
      |""".stripMargin

  private val libSource =
    """package demo;
      |public final class Lib {
      |  public static String hi() { return "hi"; }
      |}
      |""".stripMargin

  private val sleeperSource =
    """package demo;
      |import java.nio.file.*;
      |public final class Sleeper {
      |  public static void main(String[] args) throws Exception {
      |    Files.writeString(Path.of("self.pid"), Long.toString(ProcessHandle.current().pid()));
      |    Process child = new ProcessBuilder("/bin/sleep", "2").start();
      |    Files.writeString(Path.of("child.pid"), Long.toString(child.pid()));
      |    Thread.sleep(2_000L);
      |  }
      |}
      |""".stripMargin

  private final case class Tool(code: Int, text: String)
  private final case class Library(tool: Tool, jar: Path)

  private def libJar(root: Path): Library =
    val sources = Files.createDirectories(root.resolve("lib-src"))
    val classes = Files.createDirectories(root.resolve("lib-classes"))
    val file    = sources.resolve("Lib.java")
    Files.writeString(file, libSource)
    val tool = javacTool(List("-d", classes.toString, file.toString))
    val jar  = root.resolve("lib.jar")
    if tool.code == 0 then
      val buffer = new ByteArrayOutputStream
      val output = new JarOutputStream(buffer)
      output.putNextEntry(new JarEntry("demo/Lib.class"))
      output.write(Files.readAllBytes(classes.resolve("demo").resolve("Lib.class")))
      output.closeEntry()
      output.close()
      Files.write(jar, buffer.toByteArray)
    Library(tool, jar)

  private def compile(root: Path, classes: Path, name: String, source: String, classpath: List[Path]): Tool =
    val sources = Files.createDirectories(root.resolve("src"))
    val file    = sources.resolve(name)
    Files.writeString(file, source)
    val cp = if classpath.isEmpty then Nil else List("-cp", classpath.map(_.toString).mkString(java.io.File.pathSeparator))
    javacTool(cp ++ List("-d", classes.toString, file.toString))

  private def javacTool(args: List[String]): Tool =
    val javac   = javaHome.resolve("bin").resolve("javac")
    val process = new ProcessBuilder((javac.toString :: args).asJava).redirectErrorStream(true).start()
    val text    = new String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
    Tool(process.waitFor(), text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-test-run"))): dir =>
      delete(dir)

  private def delete(dir: Path): zio.UIO[Unit] =
    ZIO.attempt:
      if Files.exists(dir) then
        val walk = Files.walk(dir)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    .ignore

  def spec = suite("test subprocess")(types, process) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.timeout(15.seconds)
