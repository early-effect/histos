package histos.load

import histos.jdk.Sha256Digest
import histos.plugin.*
import histos.workspace.{IoFailure, IoFault}
import java.nio.file.{Files, LinkOption, Path}
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import zio.*

private[load] object PluginProductFiles:
  val NoLinks: LinkOption = LinkOption.NOFOLLOW_LINKS

  /** Filesystem calls finish before scoped cleanup can remove their paths. */
  def io[A](path: Path, fault: IoFault)(body: => A): IO[ProductError, A] =
    ZIO.attemptBlocking(body).mapError(_ => ProductError.Io(IoFailure(path, fault))).uninterruptible

  def ancestors(base: Path, path: Path): IO[ProductError, Unit] =
    if !path.startsWith(base) then ZIO.fail(ProductError.UnsafePath(path))
    else
      ZIO.foreachDiscard((0 to base.relativize(path).getNameCount).toList): count =>
        val relative = base.relativize(path)
        val part     = if count == 0 then base else base.resolve(relative.subpath(0, count))
        io(part, IoFault.Stat)(Files.isSymbolicLink(part)).flatMap:
          case true  => ZIO.fail(ProductError.Link(part))
          case false => ZIO.unit

  def unlinkFile(path: Path): IO[ProductError, Unit] =
    io(path, IoFault.Delete):
      if Files.notExists(path, NoLinks) then Right(())
      else if Files.isSymbolicLink(path) || Files.isRegularFile(path, NoLinks) then
        Files.deleteIfExists(path)
        Right(())
      else Left(ProductError.Kind(path, OutputKind.File))
    .flatMap(ZIO.fromEither(_))

  def delete(path: Path): IO[ProductError, Unit] =
    paths(path).flatMap: entries =>
      ZIO.foreachDiscard(entries.sortBy(_.getNameCount).reverse): entry =>
        io(entry, IoFault.Delete)(Files.deleteIfExists(entry)).unit

  def release(path: Path): ZIO[Any, Nothing, Unit] =
    delete(path).catchAll(error => ZIO.logWarning(error.message))

  def paths(path: Path): IO[ProductError, List[Path]] =
    io(path, IoFault.Walk):
      if Files.notExists(path, NoLinks) then Nil
      else
        val walk = Files.walk(path)
        try walk.iterator.asScala.toList
        finally walk.close()

  def scan(output: DeclaredOutput, root: Path): IO[ProductError, NamedProduct] =
    for
      entries <- paths(root)
      _       <- ZIO.fail(ProductError.Missing(root)).when(entries.isEmpty)
      items   <- ZIO.foreach(entries.sortBy(path => root.relativize(path).toString)): path =>
        for
          link <- io(path, IoFault.Stat)(Files.isSymbolicLink(path))
          _    <- ZIO.fail(ProductError.Link(path)).when(link)
          dir  <- io(path, IoFault.Stat)(Files.isDirectory(path, NoLinks))
          file <- io(path, IoFault.Stat)(Files.isRegularFile(path, NoLinks))
          _    <- ZIO.fail(ProductError.Kind(path, output.kind)).unless(dir || file)
          _    <- ZIO
            .fail(ProductError.Kind(path, output.kind))
            .when(path == root && (dir != (output.kind == OutputKind.Directory)))
          relative <-
            if path == root then ZIO.none
            else
              ZIO
                .fromEither(
                  ProductEntryPath
                    .from(root.relativize(path).iterator.asScala.map(_.toString).mkString("/"))
                    .left
                    .map(_ => ProductError.InvalidEntry(path.toString))
                )
                .asSome
          entry <-
            if dir then ZIO.succeed(relative.fold[ProductEntry](ProductEntry.RootDirectory)(ProductEntry.Directory(_)))
            else
              io(path, IoFault.Read)(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(path)))).map: hash =>
                relative.fold[ProductEntry](ProductEntry.RootFile(hash))(ProductEntry.File(_, hash))
        yield entry
    yield NamedProduct(output, items)

  /** Preserve declared File names for suffix-sensitive tools. */
  def destination(root: Path, output: DeclaredOutput): Path =
    val namespace = Sha256Digest.of(Chunk.fromArray(output.name.value.getBytes(UTF_8))).value
    val named     = root.resolve(namespace)
    output.kind match
      case OutputKind.File      => named.resolve(Path.of(output.path.value).getFileName)
      case OutputKind.Directory => named

  def destinations(root: Path, outputs: List[DeclaredOutput]): OutputDestinations =
    OutputDestinations(outputs.map: output =>
      output.kind match
        case OutputKind.File      => OutputDestination.File(output.name, destination(root, output))
        case OutputKind.Directory => OutputDestination.Directory(output.name, destination(root, output)))
end PluginProductFiles
