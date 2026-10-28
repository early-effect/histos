package histos.zinc

import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.Chunk

/** Zinc's `apiHash` is the public signature. The compilation timestamp and the bytecode hash are not part of it.
  */
object ApiDigest:
  val fileName: String = "api.sha256"

  def of(analysis: sbt.internal.inc.Analysis): Sha256 =
    val body = analysis.apis.allInternalClasses.toList.sorted.map: name =>
      s"$name ${analysis.apis.internalAPI(name).apiHash()}"
    Sha256.of(Chunk.fromArray(body.mkString("\n").getBytes(StandardCharsets.UTF_8)))

  def write(dir: Path, analysis: sbt.internal.inc.Analysis): Either[CompileError, Sha256] =
    val digest = of(analysis)
    val path   = dir.resolve(fileName)
    try
      Files.createDirectories(dir)
      Files.writeString(path, digest.value + "\n", StandardCharsets.UTF_8)
      Right(digest)
    catch case _: IOException => Left(CompileError.Io(IoFailure(path, IoFault.Write)))
end ApiDigest
