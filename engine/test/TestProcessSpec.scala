package histos.testrun

import histos.Platform
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildProcess, ChildRequest, ProcessExit}
import histos.pack.{MainClass, MainClassError}
import histos.zinc.{GraphCompile, GraphCompileError}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.{Chunk, Ref, Scope, System, UIO, ZIO, durationInt}
import zio.test.*

object ChildProcessSpec extends ZIOSpecDefault:
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
    test("a suite keeps the repo, PATH, and HOME from the machine"):
      for
        path    <- System.env("PATH")
        home    <- System.env("HOME")
        machine <- System.envs
        env = ModuleTest.suiteEnv(Path.of("/repo"), machine + ("USER" -> "someone"))
      yield assertTrue(
        env.get("HISTOS_REPO").contains(Path.of("/repo").toAbsolutePath.normalize.toString),
        env.get("PATH") == path.filter(_.nonEmpty),
        env.get("HOME") == home.filter(_.nonEmpty),
        !env.contains("USER"),
      )
    ,
    test("js is rejected before a process starts"):
      ZIO.scoped:
        for
          work <- temp
          req  <- request(work, Platform.Js, exitMain, ChildClasspath(work, Nil))
          out  <- ChildProcess.run(req).either
        yield assertTrue(out == Left(ChildError.UnsupportedPlatform(Platform.Js)))
    ,
    test("a missing java executable is its own error"):
      ZIO.scoped:
        for
          work <- temp
          req  <- request(work, Platform.Jvm, exitMain, ChildClasspath(work, Nil))
          out  <- ChildProcess.run(req.copy(java = work.resolve("missing-java"))).either
        yield assertTrue:
          out match
            case Left(ChildError.Java(path)) => path.endsWith("missing-java")
            case _                          => false
  )

  private val process = suite("subprocess")(
    test("System.exit stays in the child, the environment is empty, and the resolved jar is on the classpath"):
      ZIO.scoped:
        for
          root    <- temp
          classes <- ZIO.attempt(Files.createDirectories(root.resolve("classes")))
          built    <- libJar(root)
          compiled <- compile(root, classes, "Exit.java", exitSource, List(built.jar))
          work     <- ZIO.attempt(Files.createDirectories(root.resolve("work")).toRealPath())
          parent    = Option(java.lang.System.getenv("HOME")).filter(_.nonEmpty)
          req      <- request(work, Platform.Jvm, exitMain, ChildClasspath(classes, List(built.jar)))
          outcome  <- ChildProcess.run(req).either
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
          compiled <- compile(root, classes, "Sleeper.java", sleeperSource, Nil)
          work     <- ZIO.attempt(Files.createDirectories(root.resolve("work")).toRealPath())
          sleepOk   = Files.isRegularFile(Path.of("/bin/sleep"))
          req      <- request(work, Platform.Jvm, sleepMain, ChildClasspath(classes, Nil))
          fiber    <- ChildProcess.run(req).fork
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
    ,
    test("the group kill reaches a child the test left running after its main returned"):
      ZIO.scoped:
        for
          root     <- temp
          classes  <- ZIO.attempt(Files.createDirectories(root.resolve("classes")))
          compiled <- compile(root, classes, "Leaver.java", leaverSource, Nil)
          work     <- ZIO.attempt(Files.createDirectories(root.resolve("work")).toRealPath())
          req      <- request(work, Platform.Jvm, leaverMain, ChildClasspath(classes, Nil))
          outcome  <- ChildProcess.run(req)
          child    <- awaitPid(work.resolve("child.pid"))
          dead     <- ZIO.foreach(child.toList)(awaitDead)
        yield assertTrue(
          compiled == Tool(0, ""),
          outcome.exit == ProcessExit(0),
          child.nonEmpty,
          dead == List(true),
        )
    ,
    test("closing a command kills the grandchild it started, and that grandchild is in the command's group"):
      ZIO.scoped:
        for
          work  <- temp
          group <- Ref.make(Option.empty[(Long, Long)])
          shown <- ChildProcess
            .command(List("/bin/sh", "-c", "sleep 300 & echo $!; wait"), work, shellPath)
            .tap:
              case ChildEvent.Out(text) =>
                text.trim.toLongOption match
                  case Some(pid) =>
                    ps(pid, "pgid").zip(ps(pid, "ppid")).flatMap((pgid, ppid) => group.set(Some((pgid, ppid))))
                  case None => ZIO.unit
              case _ => ZIO.unit
            .take(1)
            .runCollect
          pids   = shown.collect { case ChildEvent.Out(text) => text.trim.toLongOption }.flatten
          dead  <- ZIO.foreach(pids)(awaitDead)
          found <- group.get
        yield assertTrue(
          pids.size == 1,
          dead == Chunk(true),
          found match
            case Some((pgid, ppid)) => pgid == ppid && pgid > 0
            case None               => false
        )
    ,
    test("a large output arrives whole and in bounded time"):
      ZIO.scoped:
        for
          root     <- temp
          classes  <- ZIO.attempt(Files.createDirectories(root.resolve("classes")))
          compiled <- compile(root, classes, "Chatty.java", chattySource, Nil)
          work     <- ZIO.attempt(Files.createDirectories(root.resolve("work")).toRealPath())
          req      <- request(work, Platform.Jvm, chattyMain, ChildClasspath(classes, Nil))
          timed    <- ChildProcess.run(req).timed
          (took, outcome) = timed
          lines    = outcome.out.text.linesIterator.size
          whole    = outcome.out.text == chattyText
        yield assertTrue(
          compiled == Tool(0, ""),
          outcome.exit == ProcessExit(0),
          lines == ChattyLines,
          whole,
          outcome.err.text.isEmpty,
          took.compareTo(15.seconds) < 0,
        )
    ,
    test("interrupting the bootstrap compile kills javac"):
      ZIO.scoped:
        for
          root    <- temp
          bin     <- ZIO.attempt(Files.createDirectories(root.resolve("jdk").resolve("bin")))
          stub    <- executable(bin.resolve("java"), "exit 0\n")
          _       <- executable(bin.resolve("javac"), s"echo $$$$ > '$root/javac.pid'\nexec sleep 30\n")
          classes <- ZIO.attempt(Files.createDirectories(root.resolve("classes")))
          work    <- ZIO.attempt(Files.createDirectories(root.resolve("work")))
          req     <- request(work, Platform.Jvm, exitMain, ChildClasspath(classes, Nil))
          fiber   <- ChildProcess.run(req.copy(java = stub)).fork
          javac   <- awaitPid(root.resolve("javac.pid"))
          exit    <- fiber.interrupt.timeout(3.seconds)
          dead    <- ZIO.foreach(javac.toList)(awaitDead)
        yield assertTrue(javac.nonEmpty, exit.exists(_.isInterrupted), dead == List(true))
  )

  private def executable(path: Path, body: String): ZIO[Any, Throwable, Path] =
    ZIO.attempt:
      Files.writeString(path, s"#!/bin/sh\n$body")
      Files.setPosixFilePermissions(path, java.nio.file.attribute.PosixFilePermissions.fromString("rwx------"))

  private val shellPath = Map("PATH" -> "/bin:/usr/bin")

  /** `pgid` equals `ppid` when the parent called `setpgid(0, 0)` before it started this process. */
  private def ps(pid: Long, field: String): UIO[Long] =
    ZIO.attemptBlocking {
      val process = new ProcessBuilder("ps", "-o", s"$field=", "-p", pid.toString).redirectErrorStream(true).start()
      val text = new String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8).trim
      process.waitFor()
      text.toLongOption.getOrElse(0L)
    }.orElseSucceed(0L)
  private val leaverMain = MainClass.trusted("demo.Leaver")
  private val chattyMain = MainClass.trusted("demo.Chatty")
  private val ChattyLines = 200000
  private val chattyText  = (0 until ChattyLines).map(index => s"line $index\n").mkString

  private def request(
      work: Path,
      platform: Platform,
      main: MainClass,
      classpath: ChildClasspath,
  ): zio.IO[GraphCompileError, ChildRequest] =
    GraphCompile.jdkHere.map: jdk =>
      ChildRequest(platform, jdk.home.resolve("bin").resolve("java"), classpath, main, work)

  private def alive(pid: Long): Boolean =
    ProcessHandle.of(pid).toScala.exists(_.isAlive)

  /** The file appears once the JVM reaches main. A busy runner spends the wait on bootstrap compile and process start. */
  private def awaitPid(path: Path): ZIO[Any, Nothing, Option[Long]] =
    ZIO.suspendSucceed {
      readPid(path) match
        case Some(pid) => ZIO.succeed(Some(pid))
        case None      => ZIO.sleep(20.millis) *> awaitPid(path)
    }.timeout(10.seconds).map(_.flatten)

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

  private val leaverSource =
    """package demo;
      |import java.nio.file.*;
      |public final class Leaver {
      |  public static void main(String[] args) throws Exception {
      |    Process child = new ProcessBuilder("/bin/sleep", "300").start();
      |    Files.writeString(Path.of("child.pid"), Long.toString(child.pid()));
      |  }
      |}
      |""".stripMargin

  private val chattySource =
    s"""package demo;
      |public final class Chatty {
      |  public static void main(String[] args) {
      |    for (int index = 0; index < $ChattyLines; index++) System.out.println("line " + index);
      |  }
      |}
      |""".stripMargin

  private final case class Tool(code: Int, text: String)
  private final case class Library(tool: Tool, jar: Path)

  private def libJar(root: Path): zio.IO[Throwable | GraphCompileError, Library] =
    for
      pair <- ZIO.attempt:
        val sources = Files.createDirectories(root.resolve("lib-src"))
        val classes = Files.createDirectories(root.resolve("lib-classes"))
        val file = sources.resolve("Lib.java")
        Files.writeString(file, libSource)
        (classes, file)
      (classes, file) = pair
      tool <- javacTool(List("-d", classes.toString, file.toString))
      jar = root.resolve("lib.jar")
      library <- ZIO.attempt:
        if tool.code == 0 then
          val buffer = new ByteArrayOutputStream
          val output = new JarOutputStream(buffer)
          output.putNextEntry(new JarEntry("demo/Lib.class"))
          output.write(Files.readAllBytes(classes.resolve("demo").resolve("Lib.class")))
          output.closeEntry()
          output.close()
          Files.write(jar, buffer.toByteArray)
        Library(tool, jar)
    yield library

  private def compile(
      root: Path,
      classes: Path,
      name: String,
      source: String,
      classpath: List[Path],
  ): zio.IO[Throwable | GraphCompileError, Tool] =
    for
      args <- ZIO.attempt:
        val sources = Files.createDirectories(root.resolve("src"))
        val file = sources.resolve(name)
        Files.writeString(file, source)
        val cp =
          if classpath.isEmpty then Nil
          else List("-cp", classpath.map(_.toString).mkString(java.io.File.pathSeparator))
        cp ++ List("-d", classes.toString, file.toString)
      tool <- javacTool(args)
    yield tool

  private def javacTool(args: List[String]): zio.IO[Throwable | GraphCompileError, Tool] =
    GraphCompile.jdkHere.flatMap: jdk =>
      val javac = jdk.home.resolve("bin").resolve("javac")
      ZIO.attemptBlocking:
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

  def spec = suite("test subprocess")(types, process) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(45.seconds)
