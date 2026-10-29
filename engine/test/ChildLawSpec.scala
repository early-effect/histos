package histos.testrun

import histos.jdk.Toolchain
import histos.pack.{Assembly, MainClass}
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildRequest, Process, ProcessExit}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.Platform

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}
import javax.tools.ToolProvider

import _root_.scala.jdk.OptionConverters.*

import zio.{Chunk, System, UIO, ZIO, durationInt}
import zio.test.*

/** Live `Process`. The assembly jar is the one `java -jar`. Nothing else in this file starts a child.
  */
object ChildLawSpec extends histos.SuiteClock:
  private val exitMain  = MainClass.trusted("demo.Exit")
  private val sleepMain = MainClass.trusted("demo.Sleeper")
  private val hiMain    = MainClass.trusted("demo.Hi")

  def spec =
    suite("counted children")(isolated, cancelled, refused, missing, assembled)
      .provide(Process.live) @@
      TestAspect.withLiveClock @@
      TestAspect.withLiveSystem @@
      TestAspect.timeout(60.seconds)

  private def isolated =
    test("System.exit stays in the child and the resolved jar is on the classpath"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-exit")
          libDir  <- Workspace.createDirectories(root.resolve("lib-classes"))
          classes <- Workspace.createDirectories(root.resolve("classes"))
          lib     <- javac(root, libDir, "Lib.java", libSource, Nil)
          jar = root.resolve("lib.jar")
          _     <- ZIO.when(lib.code == 0)(classJar(libDir.resolve("demo").resolve("Lib.class"), "demo/Lib.class", jar))
          exit  <- javac(root, classes, "Exit.java", exitSource, List(jar))
          work  <- realDir(root.resolve("work"))
          java  <- runningJava
          found <- java match
            case Some(bin) if lib.code == 0 && exit.code == 0 =>
              val req = ChildRequest(
                Platform.Jvm,
                bin,
                ChildClasspath(classes, List(jar)),
                exitMain,
                work,
              )
              ZIO.serviceWithZIO[Process](_.run(req)).either
            case _ => ZIO.succeed(Left(ChildError.Classes(classes)))
          expected = lines("hi", "home=null", s"dir=$work")
        yield assertTrue(
          lib.code == 0 && lib.text.isEmpty,
          exit.code == 0 && exit.text.isEmpty && java.nonEmpty,
          found.fold(_.message, _.out.text) == expected,
          found.fold(_.message, _.err.text).isEmpty,
          found.exists(_.exit == ProcessExit(7)),
        )

  private def cancelled =
    test("cancel kills the sleeping test and the child in its process group"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-cancel")
          classes <- Workspace.createDirectories(root.resolve("classes"))
          built   <- javac(root, classes, "Sleeper.java", sleeperSource, Nil)
          work    <- realDir(root.resolve("work"))
          java    <- runningJava
          sleep   <- ZIO.attemptBlocking(Files.isExecutable(Path.of("/bin/sleep"))).orElseSucceed(false)
          req = java.map(bin => ChildRequest(Platform.Jvm, bin, ChildClasspath(classes, Nil), sleepMain, work))
          fiber <- req match
            case Some(asked) => ZIO.serviceWithZIO[Process](_.run(asked)).forkScoped
            case None        => ZIO.never.forkScoped
          child <- awaitPid(work.resolve("child.pid"))
          self  <- awaitPid(work.resolve("self.pid"))
          childBefore = child.exists(alive)
          selfBefore  = self.exists(alive)
          exit      <- fiber.interrupt
          childDead <- ZIO.foreach(child.toList)(awaitDead)
          selfDead  <- ZIO.foreach(self.toList)(awaitDead)
        yield assertTrue(
          built.code == 0 && built.text.isEmpty && sleep && java.nonEmpty,
          child.nonEmpty && self.nonEmpty && childBefore && selfBefore,
          exit.isInterrupted,
          childDead == List(true) && selfDead == List(true),
        )

  private def refused =
    test("js is rejected before a process starts"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-child-js")
          work <- realDir(root.resolve("work"))
          java <- runningJava
          req = ChildRequest(
            Platform.Js,
            java.getOrElse(work.resolve("java")),
            ChildClasspath(work, Nil),
            exitMain,
            work,
          )
          out <- ZIO.serviceWithZIO[Process](_.run(req)).either
        yield assertTrue(java.nonEmpty, out == Left(ChildError.UnsupportedPlatform(Platform.Js)))

  private def missing =
    test("a missing java executable is its own error"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-child-java")
          work <- realDir(root.resolve("work"))
          java = work.resolve("missing-java")
          req  = ChildRequest(Platform.Jvm, java, ChildClasspath(work, Nil), exitMain, work)
          out <- ZIO.serviceWithZIO[Process](_.run(req)).either
        yield assertTrue:
          out match
            case Left(ChildError.Java(path)) => path == java
            case _                           => false

  private def assembled =
    test("java -jar runs the assembly main"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-jar")
          classes <- Workspace.createDirectories(root.resolve("classes"))
          built   <- javac(root, classes, "Hi.java", hiSource, Nil)
          jar = root.resolve("hi.jar")
          wrote <-
            if built.code == 0 then Assembly.write(List(classes), Nil, hiMain, jar).as(true)
            else ZIO.succeed(false)
          work <- realDir(root.resolve("work"))
          home <- System.property("java.home")
          java = home.map(dir => Path.of(dir, "bin", "java"))
          events <- java match
            case Some(bin) if wrote =>
              ZIO.serviceWithZIO[Process](
                _.command(Toolchain.command(bin, jar, Nil), work, Map.empty).runCollect
              )
            case _ => ZIO.succeed(Chunk.empty)
        yield
          val said = events.collect { case ChildEvent.Out(text) => text }
          val code = events.collect { case ChildEvent.Exited(status) => status }
          val err  = events.collect { case ChildEvent.Err(text) => text }
          assertTrue(
            built.code == 0 && built.text.isEmpty && wrote && java.nonEmpty,
            said == Chunk("ran"),
            code == Chunk(ProcessExit(0)),
            err.isEmpty,
          )

  private def lines(text: String*): String =
    text.mkString("", "\n", "\n")

  /** The pid file appears only after the child reaches main. */
  private def awaitPid(path: Path): UIO[Option[Long]] =
    readPid(path)
      .flatMap:
        case found @ Some(_) => ZIO.succeed(found)
        case None            => ZIO.sleep(20.millis) *> awaitPid(path)
      .timeout(10.seconds)
      .map(_.flatten)

  private def readPid(path: Path): UIO[Option[Long]] =
    ZIO
      .attemptBlocking:
        if !Files.isRegularFile(path) then None
        else Files.readString(path).trim.toLongOption.filter(_ > 0)
      .orElseSucceed(None)

  private def awaitDead(pid: Long): UIO[Boolean] =
    ZIO
      .suspendSucceed:
        if alive(pid) then ZIO.sleep(20.millis) *> awaitDead(pid)
        else ZIO.succeed(true)
      .timeout(2.seconds)
      .map(_.contains(true))

  private def alive(pid: Long): Boolean =
    ProcessHandle.of(pid).toScala.exists(_.isAlive)

  private def realDir(path: Path): ZIO[Any, IoFailure, Path] =
    Workspace
      .createDirectories(path)
      .flatMap: dir =>
        ZIO
          .attemptBlocking(dir.toRealPath())
          .mapError(_ => IoFailure(dir, IoFault.Stat))

  private def runningJava: ZIO[Any, Nothing, Option[Path]] =
    System
      .property("java.home")
      .orElseSucceed(None)
      .map(_.filter(_.nonEmpty).map(dir => Path.of(dir, "bin", "java")))

  private final case class Compiled(code: Int, text: String)

  private def javac(
      root: Path,
      classes: Path,
      name: String,
      source: String,
      classpath: List[Path],
  ): ZIO[Any, IoFailure, Compiled] =
    val file = root.resolve("src").resolve(name)
    for
      _        <- Workspace.writeString(file, source)
      compiled <- ZIO
        .attemptBlocking:
          val sink = new ByteArrayOutputStream
          val args =
            (if classpath.isEmpty then Nil
             else List("-cp", classpath.mkString(java.io.File.pathSeparator))) ++
              List("-d", classes.toString, file.toString)
          Option(ToolProvider.getSystemJavaCompiler) match
            case None       => Compiled(-1, "no java compiler")
            case Some(tool) =>
              val code = tool.run(new ByteArrayInputStream(Array.emptyByteArray), sink, sink, args*)
              Compiled(code, sink.toString(java.nio.charset.StandardCharsets.UTF_8))
        .mapError(_ => IoFailure(file, IoFault.Write))
    yield compiled

  private def classJar(file: Path, entry: String, dest: Path): ZIO[Any, IoFailure, Unit] =
    ZIO
      .attemptBlocking:
        val buffer = new ByteArrayOutputStream
        val output = new JarOutputStream(buffer)
        output.putNextEntry(new JarEntry(entry))
        output.write(Files.readAllBytes(file))
        output.closeEntry()
        output.close()
        Files.write(dest, buffer.toByteArray)
      .mapError(_ => IoFailure(dest, IoFault.Write))
      .unit

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
      |    Process child = Runtime.getRuntime().exec(new String[] {"/bin/sleep", "300"});
      |    Files.writeString(Path.of("child.pid"), Long.toString(child.pid()));
      |    Thread.sleep(300_000L);
      |  }
      |}
      |""".stripMargin

  private val hiSource =
    """package demo;
      |public final class Hi {
      |  public static void main(String[] args) {
      |    System.out.println("ran");
      |  }
      |}
      |""".stripMargin
end ChildLawSpec
