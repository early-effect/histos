package histos.schedule

import java.net.InetSocketAddress
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import java.util.logging.Level

import io.grpc.{MethodDescriptor, Server, ServerCallHandler, ServerServiceDefinition, Status}
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.stub.{ServerCalls, StreamObserver}

import zio.{Chunk, Scope, UIO, ZIO}

/** A remote-execution cache on 127.0.0.1, bound for the scope that opened it, so no other process can take its port.
  * [[Answer.Store]] keeps action results and blobs in memory. [[Answer.Fixed]] answers every call with that code.
  */
private[schedule] object GrpcLoopback:
  enum Answer:
    case Store
    case Fixed(code: Status.Code)

  /** The port the server listens on. The scope stops it. A refused handshake is not logged while it runs. */
  def serve(answer: Answer): ZIO[Scope, Nothing, Int] =
    quiet *> ZIO.acquireRelease(ZIO.attemptBlocking(listen(answer)).orDie)(stop).map(_.getPort)

  /** Netty logs a TLS hello on a plaintext port as a failed transport, at INFO, with a stack trace. */
  private def quiet: ZIO[Scope, Nothing, Unit] =
    val logger = java.util.logging.Logger.getLogger("io.grpc.netty.shaded.io.grpc.netty.NettyServerTransport")
    ZIO
      .acquireRelease(ZIO.succeed(Option(logger.getLevel)) <* ZIO.succeed(logger.setLevel(Level.WARNING)))(prior =>
        ZIO.succeed(logger.setLevel(prior.orNull))
      )
      .unit

  private type Handler = ServerCallHandler[Chunk[Byte], Chunk[Byte]]

  private def listen(answer: Answer): Server =
    val actions                                                           = new ConcurrentHashMap[String, Chunk[Byte]]
    val blobs                                                             = new ConcurrentHashMap[String, Chunk[Byte]]
    def unary(reply: Chunk[Byte] => Either[Status, Chunk[Byte]]): Handler =
      ServerCalls.asyncUnaryCall[Chunk[Byte], Chunk[Byte]]((request, observer) =>
        answered(answer, observer)(complete(observer, reply(request)))
      )
    NettyServerBuilder
      .forAddress(new InetSocketAddress("127.0.0.1", 0))
      .maxInboundMessageSize(GrpcChannel.MaxInbound)
      .addService(
        bind(
          Reapi.ActionCache,
          GrpcChannel.getAction    -> unary(get(actions, _)),
          GrpcChannel.updateAction -> unary(update(actions, _)),
        )
      )
      .addService(bind(Reapi.ContentAddressable, GrpcChannel.findMissing -> unary(missing(blobs, _))))
      .addService(
        bind(
          Reapi.ByteStream,
          GrpcChannel.read -> ServerCalls.asyncServerStreamingCall[Chunk[Byte], Chunk[Byte]]((request, observer) =>
            answered(answer, observer)(complete(observer, read(blobs, request)))
          ),
          GrpcChannel.write -> ServerCalls.asyncClientStreamingCall[Chunk[Byte], Chunk[Byte]](observer =>
            intake(answer, blobs, observer)
          ),
        )
      )
      .build()
      .start()

  /** A fixed status replaces whatever the store would have said. */
  private def answered(answer: Answer, observer: StreamObserver[Chunk[Byte]])(stored: => Unit): Unit =
    answer match
      case Answer.Store       => stored
      case Answer.Fixed(code) => observer.onError(Status.fromCode(code).asRuntimeException())

  private def complete(observer: StreamObserver[Chunk[Byte]], reply: Either[Status, Chunk[Byte]]): Unit =
    reply match
      case Left(status) => observer.onError(status.asRuntimeException())
      case Right(body)  =>
        observer.onNext(body)
        observer.onCompleted()

  private def get(actions: ConcurrentHashMap[String, Chunk[Byte]], request: Chunk[Byte]): Either[Status, Chunk[Byte]] =
    Reapi.decodeGet(request) match
      case None                     => Left(Status.INVALID_ARGUMENT)
      case Some((instance, action)) => Option(actions.get(key(instance, action))).toRight(Status.NOT_FOUND)

  private def update(
      actions: ConcurrentHashMap[String, Chunk[Byte]],
      request: Chunk[Byte],
  ): Either[Status, Chunk[Byte]] =
    Reapi.decodeUpdate(request) match
      case None                             => Left(Status.INVALID_ARGUMENT)
      case Some((instance, action, result)) =>
        actions.put(key(instance, action), result)
        Right(result)

  private def missing(
      blobs: ConcurrentHashMap[String, Chunk[Byte]],
      request: Chunk[Byte],
  ): Either[Status, Chunk[Byte]] =
    Reapi.decodeFind(request) match
      case None               => Left(Status.INVALID_ARGUMENT)
      case Some((_, digests)) =>
        Right(Reapi.missingResponse(digests.filterNot(digest => blobs.containsKey(digest.value))))

  private def read(blobs: ConcurrentHashMap[String, Chunk[Byte]], request: Chunk[Byte]): Either[Status, Chunk[Byte]] =
    Reapi.decodeRead(request).flatMap(Reapi.blob) match
      case None               => Left(Status.INVALID_ARGUMENT)
      case Some((hash, size)) =>
        Option(blobs.get(hash)) match
          case None                                       => Left(Status.NOT_FOUND)
          case Some(bytes) if bytes.length.toLong != size => Left(Status.INVALID_ARGUMENT)
          case Some(bytes)                                => Right(Reapi.readResponse(bytes))

  /** One upload: the first frame names the resource, the last one says it is finished. */
  private def intake(
      answer: Answer,
      blobs: ConcurrentHashMap[String, Chunk[Byte]],
      observer: StreamObserver[Chunk[Byte]],
  ): StreamObserver[Chunk[Byte]] =
    val frames = new AtomicReference(Chunk.empty[Reapi.WriteFrame])
    new StreamObserver[Chunk[Byte]]:
      def onNext(value: Chunk[Byte]): Unit =
        Reapi.decodeWrite(value).foreach(frame => frames.updateAndGet(_ :+ frame))

      def onError(error: Throwable): Unit = ()

      def onCompleted(): Unit =
        answered(answer, observer):
          val seen = frames.get
          val data = seen.flatMap(_.data)
          seen.headOption.flatMap(_.resource).flatMap(Reapi.blob) match
            case Some((hash, size)) if seen.lastOption.exists(_.finish) && data.length.toLong == size =>
              blobs.put(hash, data)
              complete(observer, Right(Reapi.writeResponse(size)))
            case _ => complete(observer, Left(Status.INVALID_ARGUMENT))
    end new
  end intake

  private def bind(
      service: String,
      methods: (MethodDescriptor[Chunk[Byte], Chunk[Byte]], Handler)*
  ): ServerServiceDefinition =
    methods
      .foldLeft(ServerServiceDefinition.builder(service)):
        case (builder, (method, handler)) => builder.addMethod(method, handler)
      .build()

  private def key(instance: String, action: Digest): String =
    s"$instance ${action.value}"

  private def stop(server: Server): UIO[Unit] =
    ZIO.succeed(server.shutdownNow()) *>
      ZIO.attemptBlockingInterrupt(server.awaitTermination(5, TimeUnit.SECONDS)).ignore
end GrpcLoopback
