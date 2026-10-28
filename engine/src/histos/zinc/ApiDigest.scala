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

  /** The signature of a compilation that defined no classes. */
  val empty: Sha256 = digest(Nil)

  def of(analysis: sbt.internal.inc.Analysis): Sha256 =
    digest(
      analysis.apis.allInternalClasses.toList.sorted.map: name =>
        s"$name ${analysis.apis.internalAPI(name).apiHash()}"
    )

  def read(dir: Path): Either[CompileError, Sha256] =
    val path = dir.resolve(fileName)
    try
      Sha256
        .from(Files.readString(path, StandardCharsets.UTF_8))
        .left
        .map: _ =>
          CompileError.Io(IoFailure(path, IoFault.Read))
    catch case _: IOException => Left(CompileError.Io(IoFailure(path, IoFault.Read)))

  def write(dir: Path, analysis: sbt.internal.inc.Analysis): Either[CompileError, Sha256] =
    store(dir, of(analysis))

  def writeEmpty(dir: Path): Either[CompileError, Sha256] =
    store(dir, empty)

  private def digest(lines: List[String]): Sha256 =
    Sha256.of(Chunk.fromArray(lines.mkString("\n").getBytes(StandardCharsets.UTF_8)))

  private def store(dir: Path, sha: Sha256): Either[CompileError, Sha256] =
    val path = dir.resolve(fileName)
    try
      Files.createDirectories(dir)
      Files.writeString(path, sha.value + "\n", StandardCharsets.UTF_8)
      Right(sha)
    catch case _: IOException => Left(CompileError.Io(IoFailure(path, IoFault.Write)))
end ApiDigest
