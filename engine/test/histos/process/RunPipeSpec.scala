package histos.process

import histos.Platform
import histos.pack.MainClass

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Queue, System, ZIO}
import zio.test.*

object RunPipeSpec extends ZIOSpecDefault:
  private val Echo: String =
    """package demo;
      |
      |public final class Echo {
      |  public static void main(String[] args) throws Exception {
      |    System.out.println("env=" + System.getenv("TERRET_POLICY"));
      |    System.out.println("path=" + System.getenv("PATH"));
      |    System.out.println("dir=" + System.getProperty("user.dir"));
      |    System.out.println("argc=" + args.length);
      |    for (String arg : args) {
      |      if (!arg.equals("read")) System.out.println("arg=" + arg);
      |    }
      |    if (args.length > 0 && args[0].equals("read")) {
      |      byte[] all = System.in.readAllBytes();
      |      System.out.println("in=" + new String(all, java.nio.charset.StandardCharsets.UTF_8));
      |    }
      |  }
      |}
      |""".stripMargin

  def spec = suite("a program child")(
    test("the child sees the requested variable, the launch directory, and the arguments") {
      prepared: fixture =>
        val Fixture(java, classes, scratch, launch) = fixture
        for
          main   <- echo
          events <- ChildProcess
            .events(
              ChildRequest(
                Platform.Jvm,
                java,
                ChildClasspath(classes, Nil),
                main,
                scratch,
                environment = Map("TERRET_POLICY" -> "Clamp"),
                directory = Some(launch),
                arguments = List("hello", "world"),
              )
            )
            .runCollect
          lines  = events.collect { case ChildEvent.Out(text) => text }.toList
          exited = events.collect { case ChildEvent.Exited(code) => code.value }.toList
          dirOk  <- matches(lines.collectFirst { case line if line.startsWith("dir=") => line.drop(4) }, launch)
          group  <- ZIO.attemptBlocking(Files.isRegularFile(scratch.resolve("group"))).orDie
          leaked <- ZIO.attemptBlocking(Files.exists(launch.resolve("group"))).orDie
        yield assertTrue(
          exited == List(0),
          lines.contains("env=Clamp"),
          lines.contains("path=null"),
          lines.contains("argc=2"),
          lines.contains("arg=hello"),
          lines.contains("arg=world"),
          dirOk,
          group,
          !leaked,
        )
    },
    test("stdin bytes come back and end of input is the end of the pipe") {
      prepared: fixture =>
        val Fixture(java, classes, scratch, launch) = fixture
        for
          main   <- echo
          queue  <- Queue.unbounded[RelayCommand]
          _      <- queue.offer(RelayCommand.Stdin(Chunk.fromArray("hello".getBytes(StandardCharsets.UTF_8))))
          _      <- queue.offer(RelayCommand.End)
          events <- ChildProcess
            .events(
              ChildRequest(
                Platform.Jvm,
                java,
                ChildClasspath(classes, Nil),
                main,
                scratch,
                directory = Some(launch),
                arguments = List("read"),
              ),
              RunRelay.Open(queue.take.map(Some(_))),
            )
            .runCollect
          lines  = events.collect { case ChildEvent.Out(text) => text }.toList
          exited = events.collect { case ChildEvent.Exited(code) => code.value }.toList
        yield assertTrue(exited == List(0), lines.contains("in=hello"))
    },
    test("a native command uses the launch directory") {
      prepared: fixture =>
        val Fixture(_, _, _, launch) = fixture
        ZIO.withSystem(System.SystemLive):
          for
            events <- ChildProcess
              .command(List("/bin/pwd"), launch, Map("PATH" -> "/usr/bin:/bin"))
              .runCollect
            lines  = events.collect { case ChildEvent.Out(text) => text }.toList
            exited = events.collect { case ChildEvent.Exited(code) => code.value }.toList
            dirOk <- matches(lines.find(_.nonEmpty), launch)
          yield assertTrue(exited == List(0), dirOk)
    },
  )

  private final case class Fixture(java: Path, classes: Path, scratch: Path, launch: Path)

  private def prepared[A](use: Fixture => ZIO[Any, Any, A]): ZIO[Any, Any, A] =
    ZIO.scoped:
      ZIO
        .acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-child")).orDie)(root =>
          ZIO.attemptBlocking(remove(root)).orDie
        )
        .flatMap: root =>
          val classes = root.resolve("classes")
          val scratch = root.resolve("scratch")
          val launch  = root.resolve("launch")
          val source  = classes.resolve("demo").resolve("Echo.java")
          val home    = _root_.java.lang.System.getProperty("java.home")
          val java    = Path.of(home, "bin", "java")
          val javac   = Path.of(home, "bin", "javac")
          for
            _ <- ZIO.attemptBlocking {
              Files.createDirectories(source.getParent)
              Files.createDirectories(scratch)
              Files.createDirectories(launch)
              Files.writeString(source, Echo)
              ()
            }.orDie
            compiled <- ZIO.attemptBlocking {
              val process = new ProcessBuilder(javac.toString, "-d", classes.toString, source.toString).start()
              val err     = new String(process.getErrorStream.readAllBytes(), StandardCharsets.UTF_8)
              val code    = process.waitFor()
              (code, err)
            }.orDie
            (code, err) = compiled
            _   <- ZIO.unless(code == 0)(ZIO.fail(err))
            ran <- use(Fixture(java, classes, scratch, launch))
          yield ran

  private def echo: ZIO[Any, Nothing, MainClass] =
    ZIO.fromEither(MainClass.from("demo.Echo")).orDieWith(error => new IllegalArgumentException(error.message))

  private def matches(printed: Option[String], launch: Path): ZIO[Any, Nothing, Boolean] =
    ZIO
      .attemptBlocking:
        printed match
          case Some(text) => Path.of(text).toRealPath() == launch.toRealPath()
          case None       => false
      .orDie

  private def remove(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
end RunPipeSpec
