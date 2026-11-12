package histos.schedule

import java.io.{ByteArrayInputStream, InputStream}
import java.util.concurrent.{
  ExecutorService,
  RejectedExecutionException,
  ScheduledFuture,
  ScheduledThreadPoolExecutor,
  TimeUnit,
}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import io.grpc.{CallOptions, ClientCall, ManagedChannel, Metadata, MethodDescriptor, Status}
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder

import zio.{Chunk, Duration, IO, Scope, UIO, ZIO, durationInt}

/** One channel, one outbound thread. `ClientCall` is not safe to send from two threads, and a blocking stub would pin a
  * ZIO worker for the whole round trip. `send`, `halfClose`, `cancel`, and the idle timer run on this channel's
  * executor. The first `request` does too, before the request is on the wire. A server-streaming listener asks for the
  * next frame itself, the way grpc-java does, so that ask is not queued behind the frame it is answering. A client
  * stream sends a frame only while the call is ready, so a sent frame means the bytes before it moved. The fiber waits
  * in `ZIO.asyncInterrupt` and cancels the call when it is interrupted.
  */
private[schedule] object GrpcChannel:
  val MaxInbound: Int = 64 * 1024 * 1024

  /** The longest a call waits with no frame moving. */
  val Idle: Duration = 30.seconds

  val getAction: MethodDescriptor[Chunk[Byte], Chunk[Byte]] =
    method(Reapi.GetActionResult, MethodDescriptor.MethodType.UNARY)

  val updateAction: MethodDescriptor[Chunk[Byte], Chunk[Byte]] =
    method(Reapi.UpdateActionResult, MethodDescriptor.MethodType.UNARY)

  val findMissing: MethodDescriptor[Chunk[Byte], Chunk[Byte]] =
    method(Reapi.FindMissingBlobs, MethodDescriptor.MethodType.UNARY)

  val read: MethodDescriptor[Chunk[Byte], Chunk[Byte]] =
    method(Reapi.Read, MethodDescriptor.MethodType.SERVER_STREAMING)

  val write: MethodDescriptor[Chunk[Byte], Chunk[Byte]] =
    method(Reapi.Write, MethodDescriptor.MethodType.CLIENT_STREAMING)

  def open(target: CacheConfig.Target.Grpc, idle: Duration): ZIO[Scope, RemoteError, Session] =
    for
      exec    <- ZIO.acquireRelease(ZIO.succeed(timers))(stop)
      channel <- ZIO.acquireRelease(connect(target))(stop)
    yield Session(channel, exec, target.host, target.port, idle)

  final class Session(
      channel: ManagedChannel,
      exec: ScheduledThreadPoolExecutor,
      host: String,
      port: Int,
      idle: Duration,
  ):
    /** `None` is `NOT_FOUND`. Any other status fails, and the description is dropped. */
    def unary(
        method: MethodDescriptor[Chunk[Byte], Chunk[Byte]],
        request: Chunk[Byte],
    ): IO[RemoteError, Option[Chunk[Byte]]] =
      invoke(method, Chunk(request), more = false).flatMap:
        case None       => ZIO.none
        case Some(body) => one(body).map(Some(_))

    def serverStream(
        method: MethodDescriptor[Chunk[Byte], Chunk[Byte]],
        request: Chunk[Byte],
    ): IO[RemoteError, Option[Chunk[Chunk[Byte]]]] =
      invoke(method, Chunk(request), more = true)

    def clientStream(
        method: MethodDescriptor[Chunk[Byte], Chunk[Byte]],
        frames: Chunk[Chunk[Byte]],
    ): IO[RemoteError, Chunk[Byte]] =
      invoke(method, frames, more = false).flatMap:
        case None       => ZIO.fail(RemoteError.Rpc(Status.Code.NOT_FOUND.name))
        case Some(body) => one(body)

    /** One response. A second one is a broken server, the same check grpc-java makes by asking for two. */
    private def one(messages: Chunk[Chunk[Byte]]): IO[RemoteError, Chunk[Byte]] =
      if messages.length <= 1 then ZIO.succeed(messages.headOption.getOrElse(Chunk.empty))
      else ZIO.fail(RemoteError.Rpc(Status.Code.INTERNAL.name))

    private def invoke(
        method: MethodDescriptor[Chunk[Byte], Chunk[Byte]],
        frames: Chunk[Chunk[Byte]],
        more: Boolean,
    ): IO[RemoteError, Option[Chunk[Chunk[Byte]]]] =
      ZIO.asyncInterrupt: cb =>
        val call     = channel.newCall(method, CallOptions.DEFAULT)
        val received = new AtomicReference(List.empty[Chunk[Byte]])
        val once     = new AtomicBoolean(false)
        val index    = new AtomicInteger(0)
        val closed   = new AtomicBoolean(false)
        val started  = new AtomicBoolean(false)
        val timer    = new AtomicReference(Option.empty[ScheduledFuture[?]])
        def finish(result: IO[RemoteError, Option[Chunk[Chunk[Byte]]]]): Unit =
          if once.compareAndSet(false, true) then
            timer.get.foreach(_.cancel(false))
            cb(result)
        // A call ends as a read timeout once no frame has moved for `idle`. gRPC's own deadline is fixed for the whole
        // call and would end a large blob that is still moving, so this timer re-arms on every frame sent or received:
        // a unary call's one request frame makes `idle` its deadline, and a stream lives while its frames move.
        def moved(): Unit =
          if !once.get then later(stalled()).foreach(next => timer.getAndSet(Some(next)).foreach(_.cancel(false)))
        def stalled(): Unit =
          finish(ZIO.fail(RemoteError.Exchange(CacheExchange.ReadTimeout(host, port))))
          call.cancel("no frame moved", null)
        // grpc-java never calls `onReady` on a call whose client sends one message, so only a client stream waits for it.
        val paced        = !method.getType.clientSendsOneMessage
        def send(): Unit =
          if !started.get || once.get || closed.get then ()
          else
            var sending = true
            while sending && !once.get && (!paced || call.isReady) do
              val at = index.get
              if at < frames.length then
                frames.drop(at).headOption match
                  case None        => sending = false
                  case Some(frame) =>
                    index.incrementAndGet()
                    try
                      call.sendMessage(frame)
                      moved()
                    catch case _: IllegalStateException => sending = false
              else
                closed.set(true)
                try call.halfClose()
                catch case _: IllegalStateException => ()
                sending = false
        val listener = new ClientCall.Listener[Chunk[Byte]]:
          override def onMessage(message: Chunk[Byte]): Unit =
            received.updateAndGet(message :: _)
            moved()
            // The next server-streaming frame has to be asked for before this callback returns.
            // Queueing it on the call executor lets a fast server finish the stream first, and
            // grpc then drops the frame that was never requested.
            if more && !once.get then
              try call.request(1)
              catch case _: IllegalStateException => ()
          override def onReady(): Unit =
            on(exec)(send())
          override def onClose(status: Status, trailers: Metadata): Unit =
            val result =
              if status.isOk then ZIO.succeed(Some(Chunk.fromIterable(received.get.reverse)))
              else if status.getCode == Status.Code.NOT_FOUND then ZIO.succeed(None)
              else ZIO.fail(RemoteError.Rpc(status.getCode.name))
            finish(result)
        on(exec):
          call.start(listener, new Metadata())
          // Same order as grpc-java ClientCalls: request, then send, then halfClose.
          // Unary and client-streaming ask for two so a second response is visible.
          // Server-streaming asks for one, then one more from onMessage.
          try call.request(if more then 1 else 2)
          catch case _: IllegalStateException => ()
          started.set(true)
          moved()
          send()
        Left(ZIO.succeed(on(exec)(call.cancel("interrupted", null))))

    private def on(exec: ExecutorService)(op: => Unit): Unit =
      try exec.execute(() => op)
      catch case _: RejectedExecutionException => ()

    private def later(op: => Unit): Option[ScheduledFuture[?]] =
      try Some(exec.schedule((() => op): Runnable, idle.toNanos, TimeUnit.NANOSECONDS))
      catch case _: RejectedExecutionException => None
  end Session

  private def method(
      name: String,
      kind: MethodDescriptor.MethodType,
  ): MethodDescriptor[Chunk[Byte], Chunk[Byte]] =
    MethodDescriptor
      .newBuilder(Bytes, Bytes)
      .setType(kind)
      .setFullMethodName(name)
      .build()

  private object Bytes extends MethodDescriptor.Marshaller[Chunk[Byte]]:
    def stream(value: Chunk[Byte]): InputStream =
      new ByteArrayInputStream(value.toArray)

    def parse(stream: InputStream): Chunk[Byte] =
      Chunk.fromArray(stream.readAllBytes())
  end Bytes

  private val threads = new java.util.concurrent.ThreadFactory:
    private val ids                           = new AtomicInteger(1)
    def newThread(runnable: Runnable): Thread =
      val thread = new Thread(runnable, s"histos-grpc-${ids.getAndIncrement()}")
      thread.setDaemon(true)
      thread

  /** One thread. A re-armed timer leaves the queue when it is cancelled, and shutdown drops the timers still waiting.
    */
  private def timers: ScheduledThreadPoolExecutor =
    val exec = new ScheduledThreadPoolExecutor(1, threads)
    exec.setRemoveOnCancelPolicy(true)
    exec.setExecuteExistingDelayedTasksAfterShutdownPolicy(false)
    exec

  private def connect(target: CacheConfig.Target.Grpc): IO[RemoteError, ManagedChannel] =
    ZIO
      .attempt:
        val builder = NettyChannelBuilder.forAddress(target.host, target.port).maxInboundMessageSize(MaxInbound)
        if target.tls then builder.useTransportSecurity() else builder.usePlaintext()
        builder.build()
      .mapError(_ => RemoteError.Exchange(CacheExchange.Connect(target.host, target.port)))

  private def stop(exec: ExecutorService): UIO[Unit] =
    ZIO.succeed(exec.shutdown()) *>
      ZIO.attemptBlockingInterrupt(exec.awaitTermination(5, TimeUnit.SECONDS)).ignore *>
      ZIO.succeed(exec.shutdownNow()).unit

  private def stop(channel: ManagedChannel): UIO[Unit] =
    ZIO.succeed(channel.shutdown()) *>
      ZIO.attemptBlockingInterrupt(channel.awaitTermination(5, TimeUnit.SECONDS)).ignore *>
      ZIO.succeed(channel.shutdownNow()).unit
end GrpcChannel
