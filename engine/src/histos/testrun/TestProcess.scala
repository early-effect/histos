package histos.testrun

import histos.Platform
import histos.jdk.Toolchain
import histos.pack.MainClass

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** The status the operating system reported. Zero and non-zero are both statuses. */
opaque type ProcessExit = Int

object ProcessExit:
  given CanEqual[ProcessExit, ProcessExit] = CanEqual.derived

  def apply(code: Int): ProcessExit = code

  extension (exit: ProcessExit) def value: Int = exit

/** Text a process wrote. It is text because the process speaks text. */
opaque type ProcessOutput = String

object ProcessOutput:
  def from(raw: String): ProcessOutput = raw

  extension (output: ProcessOutput) def text: String = output

/** Compiled classes, then resolved artifacts. The classes directory is not an optional jar. */
final case class TestClasspath(classes: Path, artifacts: List[Path]):
  def entries: List[Path] = classes :: artifacts

final case class TestRequest(
    platform: Platform,
    java: Path,
    classpath: TestClasspath,
    main: MainClass,
    work: Path,
)

final case class TestOutcome(exit: ProcessExit, out: ProcessOutput, err: ProcessOutput)

enum GroupOp:
  case Lookup, Call, Set, Pid

enum TestIo:
  case Work, Bootstrap, Spawn, Wait, Read

enum TestError:
  case UnsupportedPlatform(platform: Platform)
  case Java(path: Path)
  case Classes(path: Path)
  case Artifact(path: Path)
  case Work(path: Path)
  case Group(op: GroupOp)
  case Bootstrap(output: ProcessOutput)
  case Io(op: TestIo)

  def message: String = this match
    case UnsupportedPlatform(platform) => s"test does not run on $platform"
    case Java(path)                    => s"no java executable at $path"
    case Classes(path)                 => s"no class directory at $path"
    case Artifact(path)                => s"no classpath artifact at $path"
    case Work(path)                    => s"test work directory is not an empty directory: $path"
    case Group(op)                     => s"test process group failed while ${op.toString.toLowerCase}"
    case Bootstrap(output)             => s"test bootstrap failed: ${output.text}"
    case Io(op)                        => s"test failed while ${op.toString.toLowerCase}"

object TestProcess:
  private val EntryClass: MainClass = MainClass.trusted("subprocess.Entry")

  /**
   * Calls `setpgid(0, 0)` before the user main, so children join that group.
   * A parent cannot change the group after `java` has exec'd. This class is the only bootstrap on the test classpath.
   */
  private val EntrySource: String =
    """package subprocess;
      |
      |import java.lang.foreign.FunctionDescriptor;
      |import java.lang.foreign.Linker;
      |import java.lang.foreign.ValueLayout;
      |import java.nio.file.Files;
      |import java.nio.file.Path;
      |
      |public final class Entry {
      |  private Entry() {}
      |
      |  public static void main(String[] args) throws Throwable {
      |    Linker linker = Linker.nativeLinker();
      |    int group = (int) linker.downcallHandle(
      |      linker.defaultLookup().find("setpgid").orElseThrow(),
      |      FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT)
      |    ).invokeExact(0, 0);
      |    Files.writeString(Path.of("group"), Integer.toString(group));
      |    if (group != 0) System.exit(3);
      |    if (args.length != 1) System.exit(2);
      |    Class.forName(args[0]).getMethod("main", String[].class).invoke(null, (Object) new String[0]);
      |  }
      |}
      |""".stripMargin

  /** One subprocess for one main. The environment is empty. Interrupt kills the process group. */
  def run(request: TestRequest): IO[TestError, TestOutcome] =
    if request.platform != Platform.Jvm then ZIO.fail(TestError.UnsupportedPlatform(request.platform))
    else
      for
        _       <- require(request)
        compiler <- ZIO.fromEither(javac(request.java))
        entry    <- bootstrap(request.work, compiler)
        process <- start(request, entry)
        outcome <- body(request, process).ensuring(stop(process))
      yield outcome

  private def body(request: TestRequest, process: Process): IO[TestError, TestOutcome] =
    for
      code <- wait(process)
      _    <- grouped(request.work)
      out  <- read(request.work.resolve("stdout"))
      err  <- read(request.work.resolve("stderr"))
    yield TestOutcome(code, out, err)

  private def require(request: TestRequest): IO[TestError, Unit] =
    for
      compiler <- ZIO.fromEither(javac(request.java))
      _ <- file(request.java, TestError.Java(request.java))
      _ <- file(compiler, TestError.Java(compiler))
      _ <- directory(request.classpath.classes, TestError.Classes(request.classpath.classes))
      _ <- ZIO.foreachDiscard(request.classpath.artifacts)(path => present(path, TestError.Artifact(path)))
      _ <- empty(request.work)
    yield ()

  /** A classpath entry is a jar or a classes directory. Main classes are a directory. */
  private def present(path: Path, error: TestError): IO[TestError, Unit] =
    ZIO.attemptBlocking(Files.isRegularFile(path) || Files.isDirectory(path)).mapError(_ => TestError.Io(TestIo.Read)).flatMap: found =>
      if found then ZIO.unit else ZIO.fail(error)

  private def file(path: Path, error: TestError): IO[TestError, Unit] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).mapError(_ => TestError.Io(TestIo.Read)).flatMap: present =>
      if present then ZIO.unit else ZIO.fail(error)

  private def directory(path: Path, error: TestError): IO[TestError, Unit] =
    ZIO.attemptBlocking(Files.isDirectory(path)).mapError(_ => TestError.Io(TestIo.Read)).flatMap: present =>
      if present then ZIO.unit else ZIO.fail(error)

  private def empty(work: Path): IO[TestError, Unit] =
    ZIO.attemptBlocking {
      if !Files.isDirectory(work) then false
      else
        val listed = Files.list(work)
        try !listed.iterator.hasNext
        finally listed.close()
    }.mapError(_ => TestError.Io(TestIo.Work)).flatMap: vacant =>
      if vacant then ZIO.unit else ZIO.fail(TestError.Work(work))

  private def javac(java: Path): Either[TestError, Path] =
    Option(java.getParent) match
      case Some(bin) => Right(bin.resolve("javac"))
      case None      => Left(TestError.Java(java))

  private def bootstrap(work: Path, javac: Path): IO[TestError, Path] =
    ZIO.attemptBlocking {
      val root    = work.resolve("bootstrap")
      val sources = root.resolve("subprocess")
      Files.createDirectories(sources)
      val source  = sources.resolve("Entry.java")
      Files.writeString(source, EntrySource)
      val classes = root.resolve("classes")
      Files.createDirectories(classes)
      val process = new ProcessBuilder(javac.toString, "-d", classes.toString, source.toString)
        .redirectErrorStream(true)
        .start()
      val text = new String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
      val code = process.waitFor()
      (code, text, classes)
    }.mapError(_ => TestError.Io(TestIo.Bootstrap)).flatMap { (code, text, classes) =>
      if code == 0 then ZIO.succeed(classes)
      else ZIO.fail(TestError.Bootstrap(ProcessOutput.from(text)))
    }

  private def start(request: TestRequest, entry: Path): IO[TestError, Process] =
    ZIO.attemptBlocking {
      val entries = entry :: request.classpath.entries
      val command = List(
        request.java.toString,
        Toolchain.NativeAccess,
        "-cp",
        entries.map(_.toString).mkString(java.io.File.pathSeparator),
        EntryClass.binary,
        request.main.binary,
      )
      val builder = new ProcessBuilder(command.asJava)
      builder.directory(request.work.toFile)
      builder.environment().clear()
      builder.redirectOutput(request.work.resolve("stdout").toFile)
      builder.redirectError(request.work.resolve("stderr").toFile)
      builder.redirectInput(ProcessBuilder.Redirect.PIPE)
      builder.start()
    }.mapError(_ => TestError.Io(TestIo.Spawn))

  private def grouped(work: Path): IO[TestError, Unit] =
    read(work.resolve("group")).flatMap: status =>
      if status.text.trim.toIntOption.contains(0) then ZIO.unit else ZIO.fail(TestError.Group(GroupOp.Set))

  private def wait(process: Process): IO[TestError, ProcessExit] =
    ZIO.attemptBlockingInterrupt(process.waitFor()).mapBoth(_ => TestError.Io(TestIo.Wait), ProcessExit(_))

  private def read(path: Path): IO[TestError, ProcessOutput] =
    ZIO.attemptBlocking(ProcessOutput.from(Files.readString(path))).mapError(_ => TestError.Io(TestIo.Read))

  private def stop(process: Process): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      if process.isAlive then
        val pid = process.pid()
        if pid > 0 && pid <= Int.MaxValue then Posix.killGroup(pid.toInt)
        process.destroyForcibly()
        process.waitFor()
    }.ignore
