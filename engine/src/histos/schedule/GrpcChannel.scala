package histos.schedule

import java.io.{ByteArrayInputStream, InputStream}
import java.util.concurrent.{ExecutorService, Executors, RejectedExecutionException, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import io.grpc.{CallOptions, ClientCall, ManagedChannel, Metadata, MethodDescriptor, Status}
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder

import zio.{Chunk, IO, Scope, UIO, ZIO}

/** One channel, one outbound thread. `ClientCall` is not safe to send from two threads, and a blocking stub would pin a
  * ZIO worker for the whole round trip. `send`, `halfClose`, and `cancel` run on this channel's executor. The first
  * `request` does too, before the request is on the wire. A server-streaming listener asks for the next frame itself,
  * the way grpc-java does, so that ask is not queued behind the frame it is answering. The fiber waits in
  * `ZIO.asyncInterrupt` and cancels the call when it is interrupted.
  */
private[schedule] object GrpcChannel:
  val MaxInbound: Int = 64 * 1024 * 1024

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

  def open(host: String, port: Int, tls: Boolean): ZIO[Scope, RemoteError, Session] =
    for
      exec    <- ZIO.acquireRelease(ZIO.succeed(Executors.newSingleThreadExecutor(threads)))(stop)
      channel <- ZIO.acquireRelease(connect(host, port, tls))(stop)
    yield Session(channel, exec, host, port)

  final class Session(
      channel: ManagedChannel,
      exec: ExecutorService,
      host: String,
      port: Int,
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
        def finish(result: IO[RemoteError, Option[Chunk[Chunk[Byte]]]]): Unit =
          if once.compareAndSet(false, true) then cb(result)
        def send(): Unit =
          if !started.get || once.get || closed.get then ()
          else
            var sending = true
            while sending && !once.get do
              val at = index.get
              if at < frames.length then
                frames.drop(at).headOption match
                  case None        => sending = false
                  case Some(frame) =>
                    index.incrementAndGet()
                    try call.sendMessage(frame)
                    catch case _: IllegalStateException => sending = false
              else
                closed.set(true)
                try call.halfClose()
                catch case _: IllegalStateException => ()
                sending = false
        val listener = new ClientCall.Listener[Chunk[Byte]]:
          override def onMessage(message: Chunk[Byte]): Unit =
            received.updateAndGet(message :: _)
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
          send()
        Left(ZIO.succeed(on(exec)(call.cancel("interrupted", null))))

    private def on(exec: ExecutorService)(op: => Unit): Unit =
      try exec.execute(() => op)
      catch case _: RejectedExecutionException => ()
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

  private def connect(host: String, port: Int, tls: Boolean): IO[RemoteError, ManagedChannel] =
    ZIO
      .attempt:
        val builder = NettyChannelBuilder.forAddress(host, port).maxInboundMessageSize(MaxInbound)
        if tls then builder.useTransportSecurity() else builder.usePlaintext()
        builder.build()
      .mapError(_ => RemoteError.Exchange(CacheExchange.Connect(host, port)))

  private def stop(exec: ExecutorService): UIO[Unit] =
    ZIO.succeed(exec.shutdown()) *>
      ZIO.attemptBlockingInterrupt(exec.awaitTermination(5, TimeUnit.SECONDS)).ignore *>
      ZIO.succeed(exec.shutdownNow()).unit

  private def stop(channel: ManagedChannel): UIO[Unit] =
    ZIO.succeed(channel.shutdown()) *>
      ZIO.attemptBlockingInterrupt(channel.awaitTermination(5, TimeUnit.SECONDS)).ignore *>
      ZIO.succeed(channel.shutdownNow()).unit
end GrpcChannel
