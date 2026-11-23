package histos.js

import histos.jdk.{Sha256, Sha256Digest}
import histos.workspace.{IoFailure, IoFault}

import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

/** The child consumes only these private copies; their identities hash exactly the bytes copied. */
private[js] final case class JsInput(source: Path, canonical: Path, path: Path, digest: Sha256, bytes: Long)

private[js] object JsInputs:
  def capture(paths: List[Path], owned: Path): IO[JsLinkError, List[JsInput]] =
    ZIO.foreach(paths.zipWithIndex): (source, index) =>
      for
        root <- io(source, IoFault.Stat)(source.toRealPath())
        name <- ZIO.fromOption(Option(root.getFileName)).orElseFail(JsLinkError.Bridge)
        destination = owned.resolve(index.toString).resolve(name.toString)
        files <- inventory(root)
        rows  <- ZIO.foreach(files): file =>
          val relative = root.relativize(file)
          val target   = if root == file then destination else destination.resolve(relative)
          for
            symbolic  <- io(file, IoFault.Stat)(Files.isSymbolicLink(file))
            _         <- ZIO.fail(JsLinkError.Io(IoFailure(file, IoFault.Read))).when(symbolic)
            directory <- io(file, IoFault.Stat)(Files.isDirectory(file))
            row       <-
              if directory then io(target, IoFault.Create)(Files.createDirectories(target)).as(None)
              else
                for
                  parent  <- ZIO.fromOption(Option(target.getParent)).orElseFail(JsLinkError.Bridge)
                  _       <- io(parent, IoFault.Create)(Files.createDirectories(parent))
                  content <- fileContent(file, Some(target))
                yield Some((relative.toString, content._1, content._2))
          yield row
        content = rows.flatten
        digest  = identity(source, root, content.map((name, hash, _) => name -> hash))
      yield JsInput(source, root, destination, digest, content.map(_._3).sum)

  def unchanged(inputs: List[JsInput]): IO[JsLinkError, Unit] =
    ZIO.foreachDiscard(inputs): input =>
      fingerprint(input.source).flatMap(found => ZIO.fail(JsLinkError.InputsChanged).unless(found == input.digest))

  private def fingerprint(source: Path): IO[JsLinkError, Sha256] =
    for
      root  <- io(source, IoFault.Stat)(source.toRealPath())
      files <- inventory(root)
      rows  <- ZIO.foreach(files): file =>
        for
          symbolic <- io(file, IoFault.Stat)(Files.isSymbolicLink(file))
          _        <- ZIO.fail(JsLinkError.Io(IoFailure(file, IoFault.Read))).when(symbolic)
          regular  <- io(file, IoFault.Stat)(Files.isRegularFile(file))
          row      <-
            if regular then fileContent(file, None).map(content => Some(root.relativize(file).toString -> content._1))
            else ZIO.none
        yield row
    yield identity(source, root, rows.flatten)

  private def inventory(root: Path): IO[JsLinkError, List[Path]] =
    io(root, IoFault.Walk) {
      if Files.isDirectory(root) then
        val walk = Files.walk(root)
        try
          walk.iterator.asScala
            .takeWhile(_ => !Thread.currentThread().isInterrupted)
            .toList
            .sortBy(file => root.relativize(file).toString)
        finally walk.close()
      else List(root)
    }

  private def fileContent(source: Path, destination: Option[Path]): IO[JsLinkError, (Sha256, Long)] =
    ZIO.scoped:
      for
        input <- ZIO.acquireRelease(io(source, IoFault.Read)(Files.newInputStream(source)))(input =>
          ZIO.attemptBlocking(input.close()).ignore
        )
        output <- ZIO.foreach(destination)(target =>
          ZIO
            .acquireRelease(io(target, IoFault.Write)(Files.newOutputStream(target)))(output =>
              ZIO.attemptBlocking(output.close()).ignore
            )
            .map(stream => target -> stream)
        )
        digest <- io(source, IoFault.Read)(MessageDigest.getInstance("SHA-256"))
        buffer = new Array[Byte](65536)
        content <- transfer(source, input, output, digest, buffer)(0L)
      yield content

  private def transfer(
      source: Path,
      input: java.io.InputStream,
      output: Option[(Path, OutputStream)],
      digest: MessageDigest,
      buffer: Array[Byte],
  )(total: Long): IO[JsLinkError, (Sha256, Long)] =
    io(source, IoFault.Read)(input.read(buffer)).flatMap: size =>
      if size < 0 then ZIO.succeed(Sha256.fromDigest(digest.digest()) -> total)
      else
        ZIO.foreachDiscard(output)((target, stream) => io(target, IoFault.Write)(stream.write(buffer, 0, size))) *>
          ZIO.succeed(digest.update(buffer, 0, size)) *> transfer(source, input, output, digest, buffer)(total + size)

  private def identity(source: Path, root: Path, rows: List[(String, Sha256)]): Sha256 =
    val entries = rows.map((name, hash) => JsKey.encoded(List(name, hash.value)))
    textDigest(JsKey.encoded(source.toAbsolutePath.normalize.toString :: root.toString :: entries))

  private[js] def textDigest(text: String): Sha256 =
    Sha256Digest.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  private def io[A](path: Path, fault: IoFault)(body: => A): IO[JsLinkError, A] =
    ZIO.attemptBlockingInterrupt(body).mapError(_ => JsLinkError.Io(IoFailure(path, fault)))
end JsInputs
