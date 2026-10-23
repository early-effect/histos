package histos.testrun

import histos.Platform
import histos.jdk.Toolchain
import histos.pack.MainClass
import histos.process.ProcessTree

import java.io.InputStream
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Scope, UIO, ZIO}
import zio.stream.{ZPipeline, ZStream}

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
    environment: Map[String, String] = Map.empty,
)

final case class TestOutcome(exit: ProcessExit, out: ProcessOutput, err: ProcessOutput)

/** One line from a test process, then its status. The lines arrive as the process writes them. */
enum TestEvent:
  case Out(text: String)
  case Err(text: String)
  case Exited(code: ProcessExit)

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

  /**
   * One subprocess for one main. Stdout and stderr are lines, merged so neither pipe fills up.
   * The environment is the request's map. Interrupt kills the process group and every descendant.
   */
  def events(request: TestRequest): ZStream[Any, TestError, TestEvent] =
    if request.platform != Platform.Jvm then ZStream.fail(TestError.UnsupportedPlatform(request.platform))
    else
      ZStream.unwrapScoped {
        for
          _        <- require(request)
          compiler <- ZIO.fromEither(javac(request.java))
          entry    <- bootstrap(request.work, compiler)
          child    <- spawn(start(request, entry), Group.Own)
        yield child.console ++ ZStream.fromZIO(finish(request, child.process).map(TestEvent.Exited(_)))
      }

  /**
   * A linked program: `node`, or a native binary. No JVM bootstrap, so the child has no group of its own.
   * Interrupt kills the process and its descendants. A descendant that outlives the leader is reparented and not reached.
   */
  def command(
      argv: List[String],
      work: Path,
      environment: Map[String, String],
  ): ZStream[Any, TestError, TestEvent] =
    argv match
      case Nil => ZStream.fail(TestError.Io(TestIo.Spawn))
      case _ =>
        ZStream.fromZIO(directory(work, TestError.Work(work))).flatMap: _ =>
          watched(startCommand(argv, work, environment), Group.Inherited)

  /** Collect [[events]] for a caller that wants the text after the process ends. */
  def run(request: TestRequest): IO[TestError, TestOutcome] =
    collect(events(request))

  private def watched(start: IO[TestError, Process], group: Group): ZStream[Any, TestError, TestEvent] =
    ZStream.unwrapScoped:
      spawn(start, group).map: child =>
        child.console ++ ZStream.fromZIO(wait(child.process).map(TestEvent.Exited(_)))

  private def collect(events: ZStream[Any, TestError, TestEvent]): IO[TestError, TestOutcome] =
    events.runFold(Captured(Transcript.empty, None))(capture).flatMap: captured =>
      captured.exit match
        case Some(code) => ZIO.succeed(TestOutcome(code, captured.heard.outText, captured.heard.errText))
        case None       => ZIO.fail(TestError.Io(TestIo.Wait))

  private final case class Captured(heard: Transcript, exit: Option[ProcessExit])

  private def capture(have: Captured, event: TestEvent): Captured =
    event match
      case TestEvent.Out(text)    => have.copy(heard = have.heard.withOut(text))
      case TestEvent.Err(text)    => have.copy(heard = have.heard.withErr(text))
      case TestEvent.Exited(code) => have.copy(exit = Some(code))

  /** Only a child can make itself a group leader once it has exec'd. The JVM bootstrap does. */
  private enum Group:
    case Own, Inherited

  private final case class Child(process: Process, console: ZStream[Any, TestError, TestEvent])

  /**
   * One fiber per pipe. The readers' scope is forked before the process starts, so it closes after `stop`:
   * a blocked pipe read does not return on interrupt, so the tree dies first and each reader then reaches
   * the end of its pipe. `HaltStrategy.Both`: one pipe ending does not cancel the other. A failure does.
   */
  private def spawn(start: IO[TestError, Process], group: Group): ZIO[Scope, TestError, Child] =
    for
      readers <- ZIO.scopeWith(_.fork)
      process <- ZIO.acquireRelease(start)(stop(_, group))
      out     <- readers.extend(text(process.getInputStream).map(TestEvent.Out(_)).toQueue())
      err     <- readers.extend(text(process.getErrorStream).map(TestEvent.Err(_)).toQueue())
    yield Child(process, ZStream.fromQueue(out).flattenTake.merge(ZStream.fromQueue(err).flattenTake))

  private def text(input: InputStream): ZStream[Any, TestError, String] =
    ZStream.fromInputStream(input).via(decode >>> ZPipeline.splitLines).mapError(_ => TestError.Io(TestIo.Read))

  /** A malformed byte becomes U+FFFD, where `utf8Decode` would fail the whole console. */
  private def decode: ZPipeline[Any, CharacterCodingException, Byte, String] =
    ZPipeline.decodeCharsWithDecoder(
      StandardCharsets.UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPLACE)
        .onUnmappableCharacter(CodingErrorAction.REPLACE)
    ) >>> ZPipeline.mapChunks((chars: Chunk[Char]) => Chunk.single(new String(chars.toArray)))

  private def finish(request: TestRequest, process: Process): IO[TestError, ProcessExit] =
    for
      code <- wait(process)
      _    <- grouped(request.work)
    yield code

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

  /** `javac` is a child like the test itself, so an interrupt kills it instead of waiting out the compile. */
  private def bootstrap(work: Path, javac: Path): IO[TestError, Path] =
    for
      written <- ZIO.attemptBlocking {
        val root    = work.resolve("bootstrap")
        val sources = root.resolve("subprocess")
        Files.createDirectories(sources)
        val source  = sources.resolve("Entry.java")
        Files.writeString(source, EntrySource)
        val classes = root.resolve("classes")
        Files.createDirectories(classes)
        (source, classes)
      }.mapError(_ => TestError.Io(TestIo.Bootstrap))
      (source, classes) = written
      compiled <- collect(watched(startJavac(javac, source, classes), Group.Inherited))
      _ <- ZIO.unless(compiled.exit == ProcessExit(0))(ZIO.fail(TestError.Bootstrap(compiled.out)))
    yield classes

  /** Stderr joins stdout, so a failed compile reports one text in order. */
  private def startJavac(javac: Path, source: Path, classes: Path): IO[TestError, Process] =
    ZIO.attemptBlocking {
      new ProcessBuilder(javac.toString, "-d", classes.toString, source.toString)
        .redirectErrorStream(true)
        .start()
    }.mapError(_ => TestError.Io(TestIo.Bootstrap))

  private def startCommand(
      argv: List[String],
      work: Path,
      environment: Map[String, String],
  ): IO[TestError, Process] =
    ZIO.attemptBlocking {
      val builder = new ProcessBuilder(argv.asJava)
      builder.directory(work.toFile)
      val env = builder.environment()
      env.clear()
      environment.foreach((key, value) => env.put(key, value))
      builder.redirectInput(ProcessBuilder.Redirect.PIPE)
      builder.start()
    }.mapError(_ => TestError.Io(TestIo.Spawn))

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
      val env = builder.environment()
      env.clear()
      request.environment.foreach((key, value) => env.put(key, value))
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

  /**
   * The tree is taken before anything dies. The group kill also reaches members that left the tree,
   * and it runs after the leader exited too, because the group outlives its leader.
   */
  private def stop(process: Process, group: Group): UIO[Unit] =
    for
      tree <- ProcessTree.of(process)
      _ <- group match
        case Group.Own       => killGroup(process)
        case Group.Inherited => ZIO.unit
      _ <- tree.destroy
      _ <- ZIO.succeed {
        close(process.getInputStream)
        close(process.getErrorStream)
        close(process.getOutputStream)
      }
    yield ()

  private def killGroup(process: Process): UIO[Unit] =
    ZIO.attempt {
      val pid = process.pid()
      if pid > 0 && pid <= Int.MaxValue then Posix.killGroup(pid.toInt)
    }.ignore

  private def close(stream: java.io.Closeable): Unit =
    try stream.close()
    catch case _: java.io.IOException => ()
