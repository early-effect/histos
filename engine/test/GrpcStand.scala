package histos.schedule

import java.net.InetSocketAddress
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import io.grpc.{Server, ServerServiceDefinition, Status}
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.stub.{ServerCalls, StreamObserver}

import zio.{Chunk, Scope, UIO, ZIO}

/** In-process remote-execution server. The default suite uses it so a docker daemon is not required. `hang` never
  * answers `GetActionResult` for that hash. `fail` answers `INTERNAL` for that instance and hash, and does not store a
  * description.
  */
private[schedule] object GrpcStand:
  def serve(
      hang: Set[String] = Set.empty,
      fail: Set[(String, String)] = Set.empty,
  ): ZIO[Scope, Nothing, Int] =
    ZIO
      .acquireRelease(ZIO.attempt(listen(hang, fail)).orDie)(stop)
      .map(_.getPort)

  private def listen(hang: Set[String], fail: Set[(String, String)]): Server =
    val cas    = new ConcurrentHashMap[String, Chunk[Byte]]
    val ac     = new ConcurrentHashMap[String, Chunk[Byte]]
    val server = NettyServerBuilder
      .forAddress(new InetSocketAddress("127.0.0.1", 0))
      .maxInboundMessageSize(GrpcChannel.MaxInbound)
      .addService(actionCache(ac, hang, fail))
      .addService(blobs(cas))
      .addService(byteStream(cas))
      .build()
    server.start()
    server

  private def actionCache(
      ac: ConcurrentHashMap[String, Chunk[Byte]],
      hang: Set[String],
      fail: Set[(String, String)],
  ): ServerServiceDefinition =
    bind(
      Reapi.ActionCache,
      List(
        GrpcChannel.getAction -> ServerCalls.asyncUnaryCall[Chunk[Byte], Chunk[Byte]]((request, observer) =>
          get(ac, hang, fail, request, observer)
        ),
        GrpcChannel.updateAction -> ServerCalls.asyncUnaryCall[Chunk[Byte], Chunk[Byte]]((request, observer) =>
          update(ac, request, observer)
        ),
      ),
    )

  private def blobs(cas: ConcurrentHashMap[String, Chunk[Byte]]): ServerServiceDefinition =
    bind(
      Reapi.ContentAddressable,
      List(
        GrpcChannel.findMissing -> ServerCalls.asyncUnaryCall[Chunk[Byte], Chunk[Byte]]((request, observer) =>
          missing(cas, request, observer)
        )
      ),
    )

  private def byteStream(cas: ConcurrentHashMap[String, Chunk[Byte]]): ServerServiceDefinition =
    bind(
      Reapi.ByteStream,
      List(
        GrpcChannel.read -> ServerCalls.asyncServerStreamingCall[Chunk[Byte], Chunk[Byte]]((request, observer) =>
          read(cas, request, observer)
        ),
        GrpcChannel.write -> ServerCalls.asyncClientStreamingCall[Chunk[Byte], Chunk[Byte]](observer =>
          intake(cas, observer)
        ),
      ),
    )

  private def get(
      ac: ConcurrentHashMap[String, Chunk[Byte]],
      hang: Set[String],
      fail: Set[(String, String)],
      request: Chunk[Byte],
      observer: StreamObserver[Chunk[Byte]],
  ): Unit =
    Reapi.decodeGet(request) match
      case None =>
        observer.onError(Status.INVALID_ARGUMENT.asRuntimeException())
      case Some((_, action)) if hang.contains(action.value) =>
        ()
      case Some((instance, action)) if fail.contains(instance -> action.value) =>
        observer.onError(Status.INTERNAL.asRuntimeException())
      case Some((instance, action)) =>
        Option(ac.get(key(instance, action.value))) match
          case None =>
            observer.onError(Status.NOT_FOUND.asRuntimeException())
          case Some(body) =>
            observer.onNext(body)
            observer.onCompleted()

  private def update(
      ac: ConcurrentHashMap[String, Chunk[Byte]],
      request: Chunk[Byte],
      observer: StreamObserver[Chunk[Byte]],
  ): Unit =
    Reapi.decodeUpdate(request) match
      case None =>
        observer.onError(Status.INVALID_ARGUMENT.asRuntimeException())
      case Some((instance, action, result)) =>
        ac.put(key(instance, action.value), result)
        observer.onNext(result)
        observer.onCompleted()

  private def missing(
      cas: ConcurrentHashMap[String, Chunk[Byte]],
      request: Chunk[Byte],
      observer: StreamObserver[Chunk[Byte]],
  ): Unit =
    Reapi.decodeFind(request) match
      case None =>
        observer.onError(Status.INVALID_ARGUMENT.asRuntimeException())
      case Some((_, digests)) =>
        val gone = digests.filter(digest => !cas.containsKey(digest.value))
        observer.onNext(Reapi.missingResponse(gone))
        observer.onCompleted()

  private def read(
      cas: ConcurrentHashMap[String, Chunk[Byte]],
      request: Chunk[Byte],
      observer: StreamObserver[Chunk[Byte]],
  ): Unit =
    Reapi.decodeRead(request).flatMap(Reapi.blob) match
      case None =>
        observer.onError(Status.INVALID_ARGUMENT.asRuntimeException())
      case Some((hash, size)) =>
        Option(cas.get(hash)) match
          case None =>
            observer.onError(Status.NOT_FOUND.asRuntimeException())
          case Some(bytes) if bytes.length.toLong != size =>
            observer.onError(Status.INVALID_ARGUMENT.asRuntimeException())
          case Some(bytes) =>
            observer.onNext(Reapi.readResponse(bytes))
            observer.onCompleted()

  private def intake(
      cas: ConcurrentHashMap[String, Chunk[Byte]],
      observer: StreamObserver[Chunk[Byte]],
  ): StreamObserver[Chunk[Byte]] =
    new StreamObserver[Chunk[Byte]]:
      private val resource = new AtomicReference[Option[String]](None)
      private val data     = new AtomicReference(Chunk.empty[Byte])
      private val finish   = new AtomicBoolean(false)

      def onNext(value: Chunk[Byte]): Unit =
        Reapi.decodeWrite(value) match
          case None        => ()
          case Some(frame) =>
            resource.compareAndSet(None, frame.resource)
            data.updateAndGet(_ ++ frame.data)
            if frame.finish then finish.set(true)

      def onError(error: Throwable): Unit = ()

      def onCompleted(): Unit =
        resource.get.flatMap(Reapi.blob) match
          case Some((hash, size)) if finish.get && data.get.length.toLong == size =>
            cas.put(hash, data.get)
            observer.onNext(Reapi.writeResponse(size))
            observer.onCompleted()
          case _ =>
            observer.onError(Status.INVALID_ARGUMENT.asRuntimeException())

  private def bind(
      name: String,
      methods: List[
        (io.grpc.MethodDescriptor[Chunk[Byte], Chunk[Byte]], io.grpc.ServerCallHandler[Chunk[Byte], Chunk[Byte]])
      ],
  ): ServerServiceDefinition =
    methods
      .foldLeft(ServerServiceDefinition.builder(name)): (builder, entry) =>
        entry match
          case (descriptor, handler) => builder.addMethod(descriptor, handler)
      .build()

  private def key(instance: String, hash: String): String =
    s"$instance $hash"

  private def stop(server: Server): UIO[Unit] =
    ZIO.succeed(server.shutdownNow()) *>
      ZIO.attemptBlockingInterrupt(server.awaitTermination(5, TimeUnit.SECONDS)).ignore
end GrpcStand
