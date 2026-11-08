package histos.jdk

import java.security.MessageDigest

import zio.Chunk

/** SHA-256 of bytes on the JVM. `wire` keeps the [[Sha256]] value; Scala Native links libcrypto for MessageDigest. */
object Sha256Digest:
  def of(bytes: Chunk[Byte]): Sha256 =
    Sha256.fromDigest(MessageDigest.getInstance("SHA-256").digest(bytes.toArray))
