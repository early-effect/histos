package histos.server

import histos.jdk.Sha256

import java.nio.file.{Files, Path}

import zio.{UIO, ZIO}

/** `.histos/assembly.sha256`, the checksum the wrapper demands of a local jar. Absent when this checkout has no local
  * assembly. The server records the value it saw at boot.
  */
object AssemblyMark:
  def file(repo: Path): Path =
    repo.resolve(".histos").resolve("assembly.sha256")

  def read(repo: Path): UIO[Option[String]] =
    ZIO
      .attemptBlocking {
        val path = file(repo)
        if !Files.isRegularFile(path) then None
        else
          val hex = Files.readString(path).filterNot(_.isWhitespace)
          Sha256.from(hex).toOption.map(_.value)
      }
      .orElseSucceed(None)
end AssemblyMark
