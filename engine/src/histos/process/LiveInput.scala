package histos.process

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, ValueLayout}
import java.lang.invoke.{MethodHandles, MethodType}
import java.util.concurrent.LinkedTransferQueue
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.OptionConverters.*

import zio.stream.ZStream
import zio.{Scope, UIO, ZIO}

/** This process's standard input, as relay commands.
  *
  * The copy is a stream. A signal upcall only writes a self-pipe. The first interrupt becomes a command. The second
  * ends this process, so the invocation, including a watch, ends with it. The program's streams stay pipes.
  */
object LiveInput:
  def open: ZIO[Scope, Nothing, RunRelay] =
    for
      box <- ZIO.succeed(new LinkedTransferQueue[RelayCommand])
      _   <- serve(box)
    yield RunRelay.Open(taken(box))

  /** `box` receives commands until the scope closes. A second interrupt exits the process. */
  def serve(box: LinkedTransferQueue[RelayCommand]): ZIO[Scope, Nothing, Unit] =
    val wake = Signals.pipe()
    for
      _ <-
        if wake.read < 0 then ZIO.unit
        else
          val hits = new AtomicInteger(0)
          for
            armed <- ZIO.succeed(Signals.arm(wake.write, hits))
            _     <- ZIO.addFinalizer(ZIO.succeed {
              Signals.disarm(armed)
              Signals.stop(wake.write)
              Signals.close(wake.read)
              Signals.close(wake.write)
            })
            _ <- notes(wake.read, box).forkScoped
          yield ()
      _ <- copy(box)
    yield ()

  private def copy(box: LinkedTransferQueue[RelayCommand]): ZIO[Scope, Nothing, Unit] =
    ZStream
      .fromInputStream(java.lang.System.in, 4096)
      .chunks
      .runForeach(bytes => ZIO.succeed(box.offer(RelayCommand.Stdin(bytes))).unit)
      .catchAll(_ => ZIO.unit)
      .ensuring(ZIO.succeed(box.offer(RelayCommand.End)).unit)
      .forkScoped
      .unit

  private def notes(fd: Int, box: LinkedTransferQueue[RelayCommand]): UIO[Unit] =
    def step: UIO[Boolean] =
      ZIO.attemptBlocking(Signals.readByte(fd)).orDie.flatMap {
        case Some(Signals.Interrupt) => ZIO.succeed(box.offer(RelayCommand.Interrupt)).as(true)
        case Some(Signals.Halt)      => ZIO.succeed(java.lang.System.exit(130)).as(false)
        case _                       => ZIO.succeed(false)
      }
    def loop: UIO[Unit] =
      step.flatMap(again => if again then loop else ZIO.unit)
    loop

  private def taken(box: LinkedTransferQueue[RelayCommand]): UIO[Option[RelayCommand]] =
    ZIO
      .attemptBlocking(box.take())
      .map(Some(_))
      .orDie
      .catchAllCause(cause => if cause.isInterruptedOnly then ZIO.succeed(None) else ZIO.failCause(cause))

  /** The upcall target. `signal` runs it, so it only writes the pipe. */
  private[process] final class Hooks(writeFd: Int, hits: AtomicInteger):
    def onInterrupt(sig: Int): Unit =
      if hits.incrementAndGet() == 1 then Signals.mark(writeFd)
      else Signals.halt(writeFd)
end LiveInput

/** The self-pipe a [[LiveInput]] signal handler writes. Failures leave the handler unset. */
private[process] object Signals:
  final class Pipe(val read: Int, val write: Int)
  final class Armed(val previous: MemorySegment, val arena: Arena)

  val Interrupt: Byte = 'I'.toByte
  val Halt: Byte      = 'H'.toByte
  private val Stop    = 'S'.toByte
  private val SigInt  = 2

  private val notes = Arena.global().allocate(3)
  notes.set(ValueLayout.JAVA_BYTE, 0, Interrupt)
  notes.set(ValueLayout.JAVA_BYTE, 1, Halt)
  notes.set(ValueLayout.JAVA_BYTE, 2, Stop)

  def pipe(): Pipe =
    val arena = Arena.ofConfined()
    try
      val ends = arena.allocate(8)
      call("pipe", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS)) match
        case None       => Pipe(-1, -1)
        case Some(pipe) =>
          pipe.invokeWithArguments(ends) match
            case code: Integer if code.intValue == 0 =>
              val read  = ends.get(ValueLayout.JAVA_INT, 0)
              val write = ends.get(ValueLayout.JAVA_INT, 4)
              cloexec(read)
              cloexec(write)
              Pipe(read, write)
            case _ => Pipe(-1, -1)
    catch case _: Throwable => Pipe(-1, -1)
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
      case _: Throwable =>
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
      call("close", FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT)).foreach: closed =>
        try closed.invokeWithArguments(Integer.valueOf(fd))
        catch case _: Throwable => ()

  /** One byte. `None` is the end, or a read that failed. */
  def readByte(fd: Int): Option[Byte] =
    val arena = Arena.ofConfined()
    try
      val buf = arena.allocate(1)
      call(
        "read",
        FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG),
      ) match
        case None       => None
        case Some(read) =>
          def once: Option[Byte] =
            read.invokeWithArguments(Integer.valueOf(fd), buf, java.lang.Long.valueOf(1L)) match
              case n: java.lang.Long if n.longValue == 1              => Some(buf.get(ValueLayout.JAVA_BYTE, 0))
              case n: java.lang.Long if n.longValue < 0 && errno == 4 => once
              case _                                                  => None
          once
    catch case _: Throwable => None
    finally arena.close()

  private def write(fd: Int, offset: Long): Unit =
    if fd >= 0 then
      call(
        "write",
        FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG),
      ).foreach: write =>
        try write.invokeWithArguments(Integer.valueOf(fd), notes.asSlice(offset, 1), java.lang.Long.valueOf(1L))
        catch case _: Throwable => ()

  private def cloexec(fd: Int): Unit =
    call(
      "fcntl",
      FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT),
    ).foreach: fcntl =>
      try fcntl.invokeWithArguments(Integer.valueOf(fd), Integer.valueOf(2), Integer.valueOf(1))
      catch case _: Throwable => ()

  private def install(handler: MemorySegment): MemorySegment =
    call("signal", FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.JAVA_INT, ValueLayout.ADDRESS)) match
      case None       => MemorySegment.NULL
      case Some(call) =>
        try
          call.invokeWithArguments(Integer.valueOf(SigInt), handler) match
            case previous: MemorySegment => previous
            case _                       => MemorySegment.NULL
        catch case _: Throwable => MemorySegment.NULL

  private def errno: Int =
    val name = if linux then "__errno_location" else "__error"
    call(name, FunctionDescriptor.of(ValueLayout.ADDRESS)) match
      case None          => 0
      case Some(located) =>
        try
          located.invokeWithArguments() match
            case addr: MemorySegment => addr.get(ValueLayout.JAVA_INT, 0)
            case _                   => 0
        catch case _: Throwable => 0

  private def call(name: String, descriptor: FunctionDescriptor) =
    linker.defaultLookup().find(name).toScala.map(symbol => linker.downcallHandle(symbol, descriptor))

  private def linker: Linker = Linker.nativeLinker()

  private def linux: Boolean =
    sys.props.get("os.name").exists(_.toLowerCase.startsWith("linux"))
end Signals
