package histos.schedule

import zio.{Chunk, IO, Scope, ZIO}

/** Bazel gRPC cache. The action cache is `GetActionResult` and `UpdateActionResult`. The blob is `ByteStream.Read` and
  * `ByteStream.Write`, after `FindMissingBlobs`. The instance name is the namespace token (`main`, `pr/1`, `dev/name`).
  * A lookup walks that role's prefixes. `NOT_FOUND` tries the next one. Any other status stops. The action-cache body
  * is the same `ActionResult` the HTTP client stores, so the two protocols can read each other. The blob is written
  * first. `purge` is not a remote-execution method.
  */
object GrpcCache:
  def open(
      host: String,
      port: Int,
      tls: Boolean,
      mode: RemoteMode,
      namespace: CacheNamespace,
  ): ZIO[Scope, RemoteError, RemoteCache] =
    GrpcChannel.open(host, port, tls).map(session => new Cache(session, host, port, mode, namespace))

  private final class Cache(
      session: GrpcChannel.Session,
      host: String,
      port: Int,
      val mode: RemoteMode,
      namespace: CacheNamespace,
  ) extends RemoteCache:
    def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]] =
      mode match
        case RemoteMode.Off                     => ZIO.none
        case RemoteMode.Read | RemoteMode.Write => lookup(namespace.reads, action)

    def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit] =
      mode match
        case RemoteMode.Off   => ZIO.fail(RemoteError.Off)
        case RemoteMode.Read  => ZIO.fail(RemoteError.ReadOnly)
        case RemoteMode.Write =>
          val output = ActionDigest.ofBytes(bytes)
          val size   = bytes.length.toLong
          absent(output, size).flatMap: missing =>
            val upload =
              if missing then writeBlob(output, size, bytes)
              else ZIO.unit
            upload *> putIndex(action, ActionResult.encode(output, bytes.length))

    def purge: IO[RemoteError, Unit] =
      ZIO.fail(RemoteError.HttpOnly("purge"))

    private def lookup(rest: List[CacheNamespace], action: Digest): IO[RemoteError, Option[Chunk[Byte]]] =
      rest match
        case Nil          => ZIO.none
        case role :: more =>
          session
            .unary(GrpcChannel.getAction, Reapi.getAction(role.token, action))
            .flatMap:
              case None       => lookup(more, action)
              case Some(body) =>
                materialize(role, action, body)

    private def materialize(
        role: CacheNamespace,
        action: Digest,
        body: Chunk[Byte],
    ): IO[RemoteError, Option[Chunk[Byte]]] =
      ActionResult.decode(body) match
        case None          => ZIO.fail(RemoteError.Index(action))
        case Some(pointer) =>
          readBlob(role, pointer).flatMap:
            case None        => ZIO.fail(RemoteError.Dangling(action, pointer.output))
            case Some(bytes) =>
              if bytes.length.toLong == pointer.size && ActionDigest.ofBytes(bytes) == pointer.output then
                ZIO.some(bytes)
              else ZIO.fail(RemoteError.Mismatch(action, pointer.output))

    private def absent(output: Digest, size: Long): IO[RemoteError, Boolean] =
      session
        .unary(GrpcChannel.findMissing, Reapi.findMissing(namespace.token, output, size))
        .flatMap:
          case None       => ZIO.fail(RemoteError.Rpc(StatusName))
          case Some(body) =>
            Reapi.missingDigests(body) match
              case None          => ZIO.fail(protocol)
              case Some(digests) => ZIO.succeed(digests.contains(output))

    private def writeBlob(output: Digest, size: Long, bytes: Chunk[Byte]): IO[RemoteError, Unit] =
      val resource = Reapi.writeResource(namespace.token, output, size)
      session.clientStream(GrpcChannel.write, Reapi.frames(resource, bytes)).unit

    private def putIndex(action: Digest, result: Chunk[Byte]): IO[RemoteError, Unit] =
      session
        .unary(GrpcChannel.updateAction, Reapi.updateAction(namespace.token, action, result))
        .flatMap:
          case None    => ZIO.fail(RemoteError.Rpc(StatusName))
          case Some(_) => ZIO.unit

    private def readBlob(role: CacheNamespace, pointer: ActionResult.Pointer): IO[RemoteError, Option[Chunk[Byte]]] =
      val resource = Reapi.readResource(role.token, pointer.output, pointer.size)
      session
        .serverStream(GrpcChannel.read, Reapi.readRequest(resource))
        .flatMap:
          case None           => ZIO.none
          case Some(messages) =>
            ZIO
              .foreach(messages.toList): message =>
                ZIO.fromOption(Reapi.readData(message)).orElseFail(protocol)
              .map(parts => Some(parts.foldLeft(Chunk.empty[Byte])(_ ++ _)))

    private def protocol: RemoteError =
      RemoteError.Exchange(CacheExchange.Protocol(host, port))

    private val StatusName = "NOT_FOUND"
  end Cache
end GrpcCache
