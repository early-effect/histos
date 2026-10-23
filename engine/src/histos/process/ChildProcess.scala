package histos.process

import histos.Platform
import histos.jdk.Toolchain
import histos.pack.MainClass
import histos.testrun.Transcript

import java.io.InputStream
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Duration, IO, Scope, System, UIO, ZIO}
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
final case class ChildClasspath(classes: Path, artifacts: List[Path]):
  def entries: List[Path] = classes :: artifacts

final case class ChildRequest(
    platform: Platform,
    java: Path,
    classpath: ChildClasspath,
    main: MainClass,
    work: Path,
    environment: Map[String, String] = Map.empty,
)

final case class ChildOutcome(exit: ProcessExit, out: ProcessOutput, err: ProcessOutput)

/** One line from a child, then its status. The lines arrive as the process writes them. */
enum ChildEvent:
  case Out(text: String)
  case Err(text: String)
  case Exited(code: ProcessExit)

enum GroupOp:
  case Lookup, Call, Set, Pid

enum ChildIo:
  case Work, Bootstrap, Spawn, Wait, Read

enum ChildError:
  case UnsupportedPlatform(platform: Platform)
  case Java(path: Path)
  case Classes(path: Path)
  case Artifact(path: Path)
  case Work(path: Path)
  case Group(op: GroupOp)
  case Bootstrap(output: ProcessOutput)
  case Io(op: ChildIo)

  def message: String = this match
    case UnsupportedPlatform(platform) => s"a jvm child is required, not ${platform.token}"
    case Java(path)                    => s"no java executable at $path"
    case Classes(path)                 => s"no class directory at $path"
    case Artifact(path)                => s"no classpath artifact at $path"
    case Work(path)                    => s"work directory is not an empty directory: $path"
    case Group(op)                     => s"process group failed while ${op.toString.toLowerCase}"
    case Bootstrap(output)             => s"child bootstrap failed: ${output.text}"
    case Io(op)                        => s"child failed while ${op.toString.toLowerCase}"

object ChildProcess:
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
  def events(request: ChildRequest): ZStream[Any, ChildError, ChildEvent] =
    if request.platform != Platform.Jvm then ZStream.fail(ChildError.UnsupportedPlatform(request.platform))
    else
      ZStream.unwrapScoped {
        for
          _        <- require(request)
          compiler <- ZIO.fromEither(javac(request.java))
          entry    <- bootstrap(request.work, compiler)
          child    <- spawn(start(request, entry), Group.Own)
        yield child.console ++ ZStream.fromZIO(finish(request, child.process).map(ChildEvent.Exited(_)))
      }

  /**
   * A linked program: `node`, or a native binary. A small JVM calls `setpgid(0, 0)` and then
   * `execvp`, so the program and everything it starts share that group. Interrupt kills the group,
   * which still names a descendant after its parent has exited. On Linux that also closes the
   * pipe a blocked read is waiting on.
   */
  def command(
      argv: List[String],
      work: Path,
      environment: Map[String, String],
  ): ZStream[Any, ChildError, ChildEvent] =
    argv match
      case Nil => ZStream.fail(ChildError.Io(ChildIo.Spawn))
      case _ =>
        ZStream.unwrapScoped:
          for
            _ <- directory(work, ChildError.Work(work))
            java <- runningJava
            classes <- launcher(java)
            child <- spawn(startCommand(java, classes, argv, work, environment), Group.Own)
          yield child.console ++ ZStream.fromZIO(wait(child.process).map(ChildEvent.Exited(_)))

  /**
   * A child that shares this process's terminal. Interrupt sends SIGTERM, waits `grace`, then kills
   * whatever is still running. The JDK hand-off uses this.
   */
  def attached(argv: List[String], extra: Map[String, String], grace: Duration): IO[ChildError, Int] =
    argv match
      case Nil => ZIO.fail(ChildError.Io(ChildIo.Spawn))
      case _ =>
        ZIO.scoped:
          ZIO.acquireRelease(startAttached(argv, extra))(process =>
            ProcessTree.of(process).flatMap(_.terminate(grace))
          ).flatMap: process =>
            ZIO.attemptBlockingInterrupt(process.waitFor()).mapError(_ => ChildError.Io(ChildIo.Wait))

  /** Collect [[events]] for a caller that wants the text after the process ends. */
  def run(request: ChildRequest): IO[ChildError, ChildOutcome] =
    collect(events(request))

  private def watched(start: IO[ChildError, Process], group: Group): ZStream[Any, ChildError, ChildEvent] =
    ZStream.unwrapScoped:
      spawn(start, group).map: child =>
        child.console ++ ZStream.fromZIO(wait(child.process).map(ChildEvent.Exited(_)))

  private def collect(events: ZStream[Any, ChildError, ChildEvent]): IO[ChildError, ChildOutcome] =
    events.runFold(Captured(Transcript.empty, None))(capture).flatMap: captured =>
      captured.exit match
        case Some(code) => ZIO.succeed(ChildOutcome(code, captured.heard.outText, captured.heard.errText))
        case None       => ZIO.fail(ChildError.Io(ChildIo.Wait))

  private final case class Captured(heard: Transcript, exit: Option[ProcessExit])

  private def capture(have: Captured, event: ChildEvent): Captured =
    event match
      case ChildEvent.Out(text)    => have.copy(heard = have.heard.withOut(text))
      case ChildEvent.Err(text)    => have.copy(heard = have.heard.withErr(text))
      case ChildEvent.Exited(code) => have.copy(exit = Some(code))

  /** Only a child can make itself a group leader once it has exec'd. The JVM bootstrap does. */
  private enum Group:
    case Own, Inherited

  private final case class Child(process: Process, console: ZStream[Any, ChildError, ChildEvent])

  /**
   * One fiber per pipe. The readers' scope is forked before the process starts, so it closes after `stop`:
   * a blocked pipe read does not return on interrupt, so the tree dies first and each reader then reaches
   * the end of its pipe. `HaltStrategy.Both`: one pipe ending does not cancel the other. A failure does.
   */
  private def spawn(start: IO[ChildError, Process], group: Group): ZIO[Scope, ChildError, Child] =
    for
      readers <- ZIO.scopeWith(_.fork)
      process <- ZIO.acquireRelease(start)(stop(_, group))
      out     <- readers.extend(text(process.getInputStream).map(ChildEvent.Out(_)).toQueue())
      err     <- readers.extend(text(process.getErrorStream).map(ChildEvent.Err(_)).toQueue())
    yield Child(process, ZStream.fromQueue(out).flattenTake.merge(ZStream.fromQueue(err).flattenTake))

  private def text(input: InputStream): ZStream[Any, ChildError, String] =
    ZStream.fromInputStream(input).via(decode >>> ZPipeline.splitLines).mapError(_ => ChildError.Io(ChildIo.Read))

  /** A malformed byte becomes U+FFFD, where `utf8Decode` would fail the whole console. */
  private def decode: ZPipeline[Any, CharacterCodingException, Byte, String] =
    ZPipeline.decodeCharsWithDecoder(
      StandardCharsets.UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPLACE)
        .onUnmappableCharacter(CodingErrorAction.REPLACE)
    ) >>> ZPipeline.mapChunks((chars: Chunk[Char]) => Chunk.single(new String(chars.toArray)))

  private def finish(request: ChildRequest, process: Process): IO[ChildError, ProcessExit] =
    for
      code <- wait(process)
      _    <- grouped(request.work)
    yield code

  private def require(request: ChildRequest): IO[ChildError, Unit] =
    for
      compiler <- ZIO.fromEither(javac(request.java))
      _ <- file(request.java, ChildError.Java(request.java))
      _ <- file(compiler, ChildError.Java(compiler))
      _ <- directory(request.classpath.classes, ChildError.Classes(request.classpath.classes))
      _ <- ZIO.foreachDiscard(request.classpath.artifacts)(path => present(path, ChildError.Artifact(path)))
      _ <- empty(request.work)
    yield ()

  /** A classpath entry is a jar or a classes directory. Main classes are a directory. */
  private def present(path: Path, error: ChildError): IO[ChildError, Unit] =
    ZIO.attemptBlocking(Files.isRegularFile(path) || Files.isDirectory(path)).mapError(_ => ChildError.Io(ChildIo.Read)).flatMap: found =>
      if found then ZIO.unit else ZIO.fail(error)

  private def file(path: Path, error: ChildError): IO[ChildError, Unit] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).mapError(_ => ChildError.Io(ChildIo.Read)).flatMap: present =>
      if present then ZIO.unit else ZIO.fail(error)

  private def directory(path: Path, error: ChildError): IO[ChildError, Unit] =
    ZIO.attemptBlocking(Files.isDirectory(path)).mapError(_ => ChildError.Io(ChildIo.Read)).flatMap: present =>
      if present then ZIO.unit else ZIO.fail(error)

  private def empty(work: Path): IO[ChildError, Unit] =
    ZIO.attemptBlocking {
      if !Files.isDirectory(work) then false
      else
        val listed = Files.list(work)
        try !listed.iterator.hasNext
        finally listed.close()
    }.mapError(_ => ChildError.Io(ChildIo.Work)).flatMap: vacant =>
      if vacant then ZIO.unit else ZIO.fail(ChildError.Work(work))

  private def javac(java: Path): Either[ChildError, Path] =
    Option(java.getParent) match
      case Some(bin) => Right(bin.resolve("javac"))
      case None      => Left(ChildError.Java(java))

  /** `javac` is a child like the test itself, so an interrupt kills it instead of waiting out the compile. */
  private def bootstrap(work: Path, javac: Path): IO[ChildError, Path] =
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
      }.mapError(_ => ChildError.Io(ChildIo.Bootstrap))
      (source, classes) = written
      compiled <- collect(watched(startJavac(javac, source, classes), Group.Inherited))
      _ <- ZIO.unless(compiled.exit == ProcessExit(0))(ZIO.fail(ChildError.Bootstrap(compiled.out)))
    yield classes

  /** Stderr joins stdout, so a failed compile reports one text in order. */
  private def startJavac(javac: Path, source: Path, classes: Path): IO[ChildError, Process] =
    ZIO.attemptBlocking {
      new ProcessBuilder(javac.toString, "-d", classes.toString, source.toString)
        .redirectErrorStream(true)
        .start()
    }.mapError(_ => ChildError.Io(ChildIo.Bootstrap))

  /**
   * `setpgid` then `execvp`. The JVM image is replaced, so the pid Java recorded is the program's pid
   * and the group id.
   */
  private val ExecSource: String =
    """package subprocess;
      |
      |import java.lang.foreign.Arena;
      |import java.lang.foreign.FunctionDescriptor;
      |import java.lang.foreign.Linker;
      |import java.lang.foreign.MemorySegment;
      |import java.lang.foreign.ValueLayout;
      |
      |public final class Exec {
      |  private Exec() {}
      |
      |  public static void main(String[] args) throws Throwable {
      |    Linker linker = Linker.nativeLinker();
      |    int group = (int) linker.downcallHandle(
      |      linker.defaultLookup().find("setpgid").orElseThrow(),
      |      FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT)
      |    ).invokeExact(0, 0);
      |    if (group != 0 || args.length == 0) System.exit(3);
      |    try (Arena arena = Arena.ofConfined()) {
      |      MemorySegment argv = arena.allocate(ValueLayout.ADDRESS, args.length + 1L);
      |      for (int i = 0; i < args.length; i++) {
      |        argv.setAtIndex(ValueLayout.ADDRESS, i, arena.allocateFrom(args[i]));
      |      }
      |      argv.setAtIndex(ValueLayout.ADDRESS, args.length, MemorySegment.NULL);
      |      int ran = (int) linker.downcallHandle(
      |        linker.defaultLookup().find("execvp").orElseThrow(),
      |        FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.ADDRESS)
      |      ).invokeExact(argv.getAtIndex(ValueLayout.ADDRESS, 0), argv);
      |    }
      |    System.exit(127);
      |  }
      |}
      |""".stripMargin

  private val launcherGate = new Object

  /** Changes when `ExecSource` changes, so a stale class file is not reused. */
  private val launcherId: String =
    histos.jdk.Sha256
      .of(Chunk.fromArray(ExecSource.getBytes(java.nio.charset.StandardCharsets.UTF_8)))
      .value
      .take(16)

  private def runningJava: IO[ChildError, Path] =
    System.property("java.home").mapError(_ => ChildError.Io(ChildIo.Spawn)).flatMap:
      case Some(home) if home.nonEmpty =>
        val bin = Path.of(home, "bin", "java")
        file(bin, ChildError.Java(bin)).as(bin)
      case _ => ZIO.fail(ChildError.Java(Path.of("java")))

  /** Compiled once per machine. A later command reuses the class file. */
  private def launcher(java: Path): IO[ChildError, Path] =
    for
      tmp <- System.property("java.io.tmpdir").mapError(_ => ChildError.Io(ChildIo.Bootstrap))
      root = Path.of(tmp.filter(_.nonEmpty).getOrElse("/tmp"), s"histos-exec-$launcherId")
      compiler <- ZIO.fromEither(javac(java))
      classes <- ensureLauncher(compiler, root)
    yield classes

  private def ensureLauncher(javac: Path, root: Path): IO[ChildError, Path] =
    val classes = root.resolve("classes")
    val mark = classes.resolve(".complete")
    ZIO.attemptBlocking {
      launcherGate.synchronized {
        if !Files.isRegularFile(mark) then
          val sources = root.resolve("subprocess")
          Files.createDirectories(sources)
          val source = sources.resolve("Exec.java")
          Files.writeString(source, ExecSource)
          Files.createDirectories(classes)
          val process = new ProcessBuilder(javac.toString, "-d", classes.toString, source.toString)
            .redirectErrorStream(true)
            .start()
          val text = new String(process.getInputStream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8)
          val code = process.waitFor()
          if code != 0 then Left(text)
          else
            Files.writeString(mark, "ok\n")
            Right(classes)
        else Right(classes)
      }
    }.mapError(_ => ChildError.Io(ChildIo.Bootstrap)).flatMap:
      case Left(text)     => ZIO.fail(ChildError.Bootstrap(ProcessOutput.from(text)))
      case Right(classes) => ZIO.succeed(classes)

  private def startCommand(
      java: Path,
      classes: Path,
      argv: List[String],
      work: Path,
      environment: Map[String, String],
  ): IO[ChildError, Process] =
    ZIO.attemptBlocking {
      val command = java.toString :: Toolchain.NativeAccess :: "-cp" :: classes.toString :: "subprocess.Exec" :: argv
      val builder = new ProcessBuilder(command.asJava)
      builder.directory(work.toFile)
      val env = builder.environment()
      env.clear()
      environment.foreach((key, value) => env.put(key, value))
      builder.redirectInput(ProcessBuilder.Redirect.PIPE)
      builder.start()
    }.mapError(_ => ChildError.Io(ChildIo.Spawn))

  private def startAttached(argv: List[String], extra: Map[String, String]): IO[ChildError, Process] =
    ZIO.attemptBlocking {
      val builder = new ProcessBuilder(argv.asJava).inheritIO()
      extra.foreach((key, value) => builder.environment().put(key, value))
      builder.start()
    }.mapError(_ => ChildError.Io(ChildIo.Spawn))

  private def start(request: ChildRequest, entry: Path): IO[ChildError, Process] =
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
    }.mapError(_ => ChildError.Io(ChildIo.Spawn))

  private def grouped(work: Path): IO[ChildError, Unit] =
    read(work.resolve("group")).flatMap: status =>
      if status.text.trim.toIntOption.contains(0) then ZIO.unit else ZIO.fail(ChildError.Group(GroupOp.Set))

  private def wait(process: Process): IO[ChildError, ProcessExit] =
    ZIO.attemptBlockingInterrupt(process.waitFor()).mapBoth(_ => ChildError.Io(ChildIo.Wait), ProcessExit(_))

  private def read(path: Path): IO[ChildError, ProcessOutput] =
    ZIO.attemptBlocking(ProcessOutput.from(Files.readString(path))).mapError(_ => ChildError.Io(ChildIo.Read))

  /**
   * The tree is taken before anything dies. The group kill also reaches a descendant that was
   * reparented, because `setpgid` ran before `exec`. That descendant closes its end of the pipe,
   * so a read blocked in this process returns on Linux as well as macOS.
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
