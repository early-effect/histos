package histos.schedule

import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.*
import histos.load.{NamedProduct, OutputManifest, ProductEntry, ProductError}
import zio.json.*

import java.nio.file.{Files, LinkOption, Path}

import zio.{Chunk, UIO, ZIO}

/** Version 2 records exact ownership and complete relative trees. Bytes live in ContentStore. Version 1 is readable for
  * legacy file validation, but is not a replayable product manifest.
  */
object PluginManifest:
  val Header: String         = "histos-manifest v1"
  private val ProductsHeader = "histos-products v2\n"

  private enum EntryData derives JsonCodec:
    case RootDirectory
    case RootFile(hash: String)
    case Directory(path: String)
    case File(path: String, hash: String)

  private enum OutputData derives JsonCodec:
    case File(name: String, path: String, entries: List[EntryData])
    case Directory(name: String, path: String, entries: List[EntryData])

  private final case class ManifestData(text: String, outputs: List[OutputData]) derives JsonCodec

  def store(manifest: OutputManifest): String =
    val outputs = manifest.products.map: product =>
      val entries = product.entries.map:
        case ProductEntry.RootDirectory    => EntryData.RootDirectory
        case ProductEntry.RootFile(hash)   => EntryData.RootFile(hash.value)
        case ProductEntry.Directory(path)  => EntryData.Directory(path.value)
        case ProductEntry.File(path, hash) => EntryData.File(path.value, hash.value)
      product.output match
        case DeclaredOutput.File(name, path)      => OutputData.File(name.value, path.value, entries)
        case DeclaredOutput.Directory(name, path) => OutputData.Directory(name.value, path.value, entries)
    ProductsHeader + ManifestData(manifest.text, outputs).toJson

  def output(manifest: OutputManifest): PluginOutput =
    val files = manifest.products.collect:
      case NamedProduct(DeclaredOutput.File(name, path), _) => WrittenFile(name, path)
    val directories = manifest.products.collect:
      case NamedProduct(DeclaredOutput.Directory(name, path), _) => WrittenDirectory(name, path)
    PluginOutput(manifest.text, files, directories)

  def products(stored: String): Either[ProductError, Option[OutputManifest]] =
    if !stored.startsWith(ProductsHeader) then Right(None)
    else
      for
        data    <- stored.stripPrefix(ProductsHeader).fromJson[ManifestData].left.map(_ => ProductError.InvalidManifest)
        outputs <- sequence(data.outputs.map:
          case OutputData.File(name, path, entries)      => named(name, path, OutputKind.File, entries)
          case OutputData.Directory(name, path, entries) => named(name, path, OutputKind.Directory, entries))
      yield Some(OutputManifest(data.text, outputs))

  private def named(
      name: String,
      path: String,
      kind: OutputKind,
      entries: List[EntryData],
  ): Either[ProductError, NamedProduct] =
    for
      parsedName    <- OutputName.from(name).left.map(_ => ProductError.InvalidManifest)
      parsedPath    <- RepoPath.from(path).left.map(_ => ProductError.InvalidManifest)
      parsedEntries <- sequence(entries.map:
        case EntryData.RootDirectory  => Right(ProductEntry.RootDirectory)
        case EntryData.RootFile(hash) =>
          Sha256.from(hash).left.map(_ => ProductError.InvalidManifest).map(ProductEntry.RootFile(_))
        case EntryData.Directory(path) =>
          ArtifactPath.from(path).left.map(_ => ProductError.InvalidEntry(path)).map(ProductEntry.Directory(_))
        case EntryData.File(path, hash) =>
          for
            p <- ArtifactPath.from(path).left.map(_ => ProductError.InvalidEntry(path))
            h <- Sha256.from(hash).left.map(_ => ProductError.InvalidManifest)
          yield ProductEntry.File(p, h))
      output = kind match
        case OutputKind.File      => DeclaredOutput.File(parsedName, parsedPath)
        case OutputKind.Directory => DeclaredOutput.Directory(parsedName, parsedPath)
    yield NamedProduct(output, parsedEntries)

  private def sequence[A](values: List[Either[ProductError, A]]): Either[ProductError, List[A]] =
    values.foldRight[Either[ProductError, List[A]]](Right(Nil))((value, rest) =>
      for a <- value; as <- rest yield a :: as
    )

  def store(text: String, files: List[(RepoPath, Sha256)]): String =
    if files.isEmpty then text
    else
      val lines = files.map((path, sha) => s"${sha.value} ${path.value}")
      (Header :: lines.size.toString :: lines ::: List(text)).mkString("\n")

  /** The task text under a file manifest. A text hit returns the stored string.
    */
  def text(stored: String): String =
    products(stored) match
      case Right(Some(manifest)) => manifest.text
      case Right(None)           => read(stored).fold(stored)(_.text)
      case Left(_)               => stored

  /** A plugin hit still has to have every declared file, with the sha256 the manifest recorded. No declared outputs
    * skips the check. A manifest that does not parse is a miss.
    */
  def accept(
      stored: String,
      root: Path,
      outputs: List[RepoPath],
  ): UIO[Boolean] =
    if outputs.isEmpty then ZIO.succeed(true)
    else ZIO.attemptBlocking(matches(stored, root, outputs)).orElseSucceed(false)

  private final case class Listed(files: List[(String, String)], text: String)

  private def matches(
      stored: String,
      root: Path,
      outputs: List[RepoPath],
  ): Boolean =
    read(stored) match
      case None         => false
      case Some(listed) =>
        val expected = outputs.map(_.value).toSet
        val found    = listed.files.map(_._2).toSet
        expected == found && listed.files.forall((sha, path) => same(root, sha, path))

  private def same(root: Path, sha: String, path: String): Boolean =
    val file = root.resolve(path)
    Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) &&
    Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(file))).value == sha

  private def read(stored: String): Option[Listed] =
    stored.split("\n", -1).toList match
      case Header :: countText :: rest =>
        countText.toIntOption match
          case Some(count) if count >= 0 && rest.length >= count =>
            val (listed, body) = rest.splitAt(count)
            val files          =
              listed.foldLeft[Option[List[(String, String)]]](Some(Nil)): (acc, line) =>
                acc.flatMap: soFar =>
                  line.split(" ", 2).toList match
                    case sha :: path :: Nil =>
                      Sha256
                        .from(sha)
                        .toOption
                        .flatMap: parsed =>
                          RepoPath
                            .from(path)
                            .toOption
                            .map(ok => soFar :+ (parsed.value -> ok.value))
                    case _ => None
            files.map(parsed => Listed(parsed, body.mkString("\n")))
          case _ => None
      case _ => None
end PluginManifest
