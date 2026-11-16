package histos.process

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.OptionConverters.*

import zio.stream.ZStream
import zio.{Queue, Scope, UIO, ZIO}

/** The caller's standard input, as relay commands.
  *
  * A signal handler only writes the self-pipe. The second interrupt exits the process so the JVM shutdown hook can
  * interrupt the run.
  */
object LiveInput:
  def open: ZIO[Scope, Nothing, RunRelay] =
    for
      queue <- Queue.unbounded[RelayCommand]
      _     <- serve(queue)
    yield RunRelay.Open(take(queue))

  /** `queue` receives commands until the scope closes. A second interrupt exits the process. */
  def serve(queue: Queue[RelayCommand]): ZIO[Scope, Nothing, Unit] =
    ZIO.suspendSucceed:
      Signals.pipe() match
        case None       => copy(queue)
        case Some(pipe) =>
          val hits = new AtomicInteger(0)
          for
            armed <- ZIO.succeed(Signals.arm(pipe.write, hits))
            _     <- ZIO.addFinalizer(ZIO.succeed {
              Signals.disarm(armed)
              Signals.stop(pipe.write)
              Signals.close(pipe.read)
              Signals.close(pipe.write)
            })
            _ <- notes(pipe.read, queue).forkScoped
            _ <- copy(queue)
          yield ()

  private def copy(queue: Queue[RelayCommand]): ZIO[Scope, Nothing, Unit] =
    ZStream
      .fromInputStream(java.lang.System.in, 4096)
      .chunks
      .runForeach(bytes => queue.offer(RelayCommand.Stdin(bytes)).unit)
      .ensuring(queue.offer(RelayCommand.End).unit)
      .forkScoped
      .unit

  private def notes(fd: Int, queue: Queue[RelayCommand]): UIO[Unit] =
    def loop: UIO[Unit] =
      ZIO
        .attemptBlocking(Signals.readByte(fd))
        .orDie
        .flatMap:
          case Some(Signals.Interrupt) => queue.offer(RelayCommand.Interrupt) *> loop
          case Some(Signals.Halt)      => halt
          case _                       => ZIO.unit
    loop

  /** Does not complete. `exit` runs the shutdown hook, which interrupts this process. */
  private def halt: UIO[Nothing] =
    ZIO.async[Any, Nothing, Nothing](_ => java.lang.System.exit(130))

  private def take(queue: Queue[RelayCommand]): UIO[Option[RelayCommand]] =
    queue.take.map(Some(_)).catchAllCause { cause =>
      if cause.isInterruptedOnly then ZIO.succeed(None) else ZIO.failCause(cause)
    }

  /** The upcall target. `signal` runs it, so it only writes the pipe. */
  private[process] final class Hooks(writeFd: Int, hits: AtomicInteger):
    def onInterrupt(sig: Int): Unit =
      if hits.incrementAndGet() == 1 then Signals.mark(writeFd)
      else Signals.halt(writeFd)
end LiveInput

/** The self-pipe a [[LiveInput]] signal handler writes. A failed libc call leaves the handler unset. */
private[process] object Signals:
  final case class Pipe(read: Int, write: Int)
  final case class Armed(previous: MemorySegment, arena: Arena)

  val Interrupt: Byte = 'I'.toByte
  val Halt: Byte      = 'H'.toByte
  private val Stop    = 'S'.toByte
  private val SigInt  = 2
  private val Eintr   = 4
  private val FSetFd  = 2
  private val Cloexec = 1

  private val notes = Arena.global().allocate(3)
  notes.set(ValueLayout.JAVA_BYTE, 0, Interrupt)
  notes.set(ValueLayout.JAVA_BYTE, 1, Halt)
  notes.set(ValueLayout.JAVA_BYTE, 2, Stop)

  def pipe(): Option[Pipe] =
    val arena = Arena.ofConfined()
    try
      val ends = arena.allocate(8)
      downcall("pipe", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS)).flatMap { pipe =>
        invoke(pipe, ends) match
          case Some(code: Integer) if code.intValue == 0 =>
            val read  = ends.get(ValueLayout.JAVA_INT, 0)
            val write = ends.get(ValueLayout.JAVA_INT, 4)
            cloexec(read)
            cloexec(write)
            Some(Pipe(read, write))
          case _ => None
      }
    catch case _: Exception => None
    finally arena.close()

  def arm(writeFd: Int, hits: AtomicInteger): Option[Armed] =
    val arena = Arena.ofShared()
    try
      val hooks = new LiveInput.Hooks(writeFd, hits)
      val kind  = MethodType.methodType(Void.TYPE, Integer.TYPE)
      val bound = MethodHandles.lookup().findVirtual(classOf[LiveInput.Hooks], "onInterrupt", kind).bindTo(hooks)
      val stub  = linker.upcallStub(bound, FunctionDescriptor.ofVoid(ValueLayout.JAVA_INT), arena)
      Some(Armed(install(stub), arena))
    catch
      case _: Exception =>
        arena.close()
        None

  def disarm(armed: Option[Armed]): Unit =
    armed match
      case None       => ()
      case Some(held) =>
        install(held.previous)
        held.arena.close()

  def mark(fd: Int): Unit = write(fd, 0)

  def halt(fd: Int): Unit = write(fd, 1)

  def stop(fd: Int): Unit = write(fd, 2)

  def close(fd: Int): Unit =
    if fd >= 0 then
      downcall("close", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT)).foreach { closed =>
        invoke(closed, Integer.valueOf(fd))
        ()
      }

  /** One byte. `None` is the end of the pipe or a failed read. `EINTR` retries. */
  def readByte(fd: Int): Option[Byte] =
    val arena = Arena.ofConfined()
    try
      val buf = arena.allocate(1)
      downcall(
        "read",
        FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG),
      ).flatMap { read =>
        @scala.annotation.tailrec
        def once: Option[Byte] =
          invoke(read, Integer.valueOf(fd), buf, java.lang.Long.valueOf(1L)) match
            case Some(n: java.lang.Long) if n.longValue == 1                  => Some(buf.get(ValueLayout.JAVA_BYTE, 0))
            case Some(n: java.lang.Long) if n.longValue < 0 && errno == Eintr => once
            case _                                                            => None
        once
      }
    catch case _: Exception => None
    finally arena.close()

  private def write(fd: Int, offset: Long): Unit =
    if fd >= 0 then
      downcall(
        "write",
        FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG),
      ).foreach { write =>
        invoke(write, Integer.valueOf(fd), notes.asSlice(offset, 1), java.lang.Long.valueOf(1L))
        ()
      }

  private def cloexec(fd: Int): Unit =
    downcall(
      "fcntl",
      FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT),
    ).foreach { fcntl =>
      invoke(fcntl, Integer.valueOf(fd), Integer.valueOf(FSetFd), Integer.valueOf(Cloexec))
      ()
    }

  private def install(handler: MemorySegment): MemorySegment =
    downcall("signal", FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.JAVA_INT, ValueLayout.ADDRESS))
      .flatMap(call => invoke(call, Integer.valueOf(SigInt), handler)) match
      case Some(previous: MemorySegment) => previous
      case _                             => MemorySegment.NULL

  private def errno: Int =
    val name = libc match
      case Libc.Linux  => "__errno_location"
      case Libc.Darwin => "__error"
    downcall(name, FunctionDescriptor.of(ValueLayout.ADDRESS)).flatMap(located => invoke(located)) match
      case Some(addr: MemorySegment) => addr.get(ValueLayout.JAVA_INT, 0)
      case _                         => 0

  private def downcall(name: String, descriptor: FunctionDescriptor): Option[MethodHandle] =
    try linker.defaultLookup().find(name).toScala.map(symbol => linker.downcallHandle(symbol, descriptor))
    catch case _: Exception => None

  private def invoke(handle: MethodHandle, args: Object*): Option[Object] =
    try Some(handle.invokeWithArguments(args*))
    catch case _: Throwable => None

  private def linker: Linker = Linker.nativeLinker()

  private def libc: Libc =
    sys.props.get("os.name") match
      case Some(name) if name.toLowerCase.startsWith("linux") => Libc.Linux
      case _                                                  => Libc.Darwin

  private enum Libc:
    case Linux, Darwin
end Signals
