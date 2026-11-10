package histos.pack

import histos.plugin.ApiDigest
import histos.{AssetError, GeneratedAsset, ResourceName}

import java.io.IOException
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Copies unmanaged resources and generated assets into a class directory.
  *
  * `manifest` sits outside `classes`. The ownership list must survive a class-directory wipe, and a jar walk must not
  * pack it. `None` is a directory filled once, then moved or content-addressed.
  */
object AssetSync:
  val Manifest: String = "resources.list"

  def sync(
      classes: Path,
      manifest: Option[Path],
      unmanaged: List[(ResourceName, Path)],
      generated: List[GeneratedAsset],
      repo: Path,
  ): IO[AssetError, Unit] =
    val unmanagedKept = unmanaged.filterNot(row => row._1.value == ApiDigest.resource)
    val generatedKept = generated.filterNot(asset => asset.resource.value == ApiDigest.resource)
    for
      _        <- refuseEntry(unmanaged, generated, repo)
      _        <- ZIO.fromEither(distinct(generatedKept))
      desired  <- prepare(unmanagedKept, generatedKept, repo)
      _        <- collide(classes, desired)
      previous <- readManifest(manifest)
      _        <- dropStale(classes, previous, desired.map(_._1).toSet)
      _        <- dropReserved(classes)
      _        <- writeAll(classes, desired)
      _        <- writeManifest(manifest, desired.map(_._1))
    yield ()

  private enum Payload:
    case Copy(path: Path)
    case Text(text: String)

  private def refuseEntry(
      unmanaged: List[(ResourceName, Path)],
      generated: List[GeneratedAsset],
      repo: Path,
  ): IO[AssetError, Unit] =
    val fromFile = unmanaged.collectFirst:
      case (name, path) if name.value == AssetError.refusedName => path
    val fromGenerated = generated.collectFirst:
      case GeneratedAsset.File(name, path) if name.value == AssetError.refusedName =>
        repo.resolve(path.value)
      case GeneratedAsset.Text(name, _) if name.value == AssetError.refusedName =>
        Path.of(name.value)
    fromFile.orElse(fromGenerated) match
      case Some(path) => ZIO.fail(AssetError.CommittedEntry(path))
      case None       => ZIO.unit

  /** A class directory from an older publish may still hold these two files. They are not resources of the module. */
  private def dropReserved(classes: Path): IO[AssetError, Unit] =
    ZIO
      .attemptBlocking:
        List(AssetError.refusedName, ApiDigest.resource).foreach: name =>
          val dest = classes.resolve(name)
          if Files.isRegularFile(dest) then Files.delete(dest)
      .mapError(_ => AssetError.Unreadable(classes))
      .unit

  private def distinct(generated: List[GeneratedAsset]): Either[AssetError, Unit] =
    generated
      .map(_.resource)
      .foldLeft[Either[AssetError, Set[ResourceName]]](Right(Set.empty)):
        case (Left(error), _)                  => Left(error)
        case (Right(seen), name) if seen(name) => Left(AssetError.Duplicate(name))
        case (Right(seen), name)               => Right(seen + name)
      .map(_ => ())

  private def prepare(
      unmanaged: List[(ResourceName, Path)],
      generated: List[GeneratedAsset],
      repo: Path,
  ): IO[AssetError, List[(ResourceName, Payload)]] =
    ZIO
      .attemptBlocking:
        val base = unmanaged.map((name, path) => name -> Payload.Copy(path)).toMap
        generated
          .foldLeft[Either[AssetError, Map[ResourceName, Payload]]](Right(base)):
            case (Left(error), _)                              => Left(error)
            case (Right(acc), GeneratedAsset.Text(name, text)) =>
              Right(acc.updated(name, Payload.Text(text)))
            case (Right(acc), GeneratedAsset.File(name, path)) =>
              val file = repo.resolve(path.value)
              if Files.isRegularFile(file) then Right(acc.updated(name, Payload.Copy(file)))
              else Left(AssetError.Missing(file))
          .map: found =>
            found.toList.sortBy((name, _) => name.value)
      .mapError(_ => AssetError.Unreadable(repo))
      .flatMap(ZIO.fromEither)

  private def collide(classes: Path, desired: List[(ResourceName, Payload)]): IO[AssetError, Unit] =
    ZIO
      .attemptBlocking:
        val root = classes.toAbsolutePath.normalize
        desired.foldLeft[Either[AssetError, Unit]](Right(())):
          case (Left(error), _)      => Left(error)
          case (Right(_), (name, _)) =>
            val dest = root.resolve(name.value).normalize
            if !dest.startsWith(root) || dest == root then Left(AssetError.Escaped(name.value))
            else if name.value.endsWith(".class") && Files.isRegularFile(dest) then Left(AssetError.Collides(name))
            else Right(())
      .mapError(_ => AssetError.Unreadable(classes))
      .flatMap(ZIO.fromEither)

  private def readManifest(manifest: Option[Path]): IO[AssetError, List[ResourceName]] =
    manifest match
      case None       => ZIO.succeed(Nil)
      case Some(file) =>
        ZIO
          .attemptBlocking:
            if !Files.isRegularFile(file) then Right(Nil)
            else
              Files
                .readString(file)
                .split("\n", -1)
                .toList
                .map(_.stripSuffix("\r"))
                .filter(_.nonEmpty)
                .foldLeft[Either[AssetError, List[ResourceName]]](Right(Nil)):
                  case (Left(error), _)   => Left(error)
                  case (Right(acc), line) =>
                    ResourceName.from(line) match
                      case Right(name) => Right(acc :+ name)
                      case Left(_)     => Left(AssetError.Unreadable(file))
          .mapError(_ => AssetError.Unreadable(file))
          .flatMap(ZIO.fromEither)

  private def dropStale(
      classes: Path,
      previous: List[ResourceName],
      keep: Set[ResourceName],
  ): IO[AssetError, Unit] =
    ZIO
      .attemptBlocking:
        val root = classes.toAbsolutePath.normalize
        previous
          .filterNot(keep.contains)
          .foldLeft[Either[AssetError, Unit]](Right(())):
            case (Left(error), _)                                  => Left(error)
            case (Right(_), name) if name.value.endsWith(".class") => Right(())
            case (Right(_), name)                                  =>
              val dest = root.resolve(name.value).normalize
              if !dest.startsWith(root) || dest == root then Left(AssetError.Escaped(name.value))
              else
                try
                  if Files.isRegularFile(dest) then Files.delete(dest)
                  Right(())
                catch case _: IOException => Left(AssetError.Unreadable(dest))
      .mapError(_ => AssetError.Unreadable(classes))
      .flatMap(ZIO.fromEither)

  private def writeAll(classes: Path, desired: List[(ResourceName, Payload)]): IO[AssetError, Unit] =
    ZIO
      .attemptBlocking:
        val root = classes.toAbsolutePath.normalize
        desired.foldLeft[Either[AssetError, Unit]](Right(())):
          case (Left(error), _)            => Left(error)
          case (Right(_), (name, payload)) =>
            val dest = root.resolve(name.value).normalize
            if !dest.startsWith(root) || dest == root then Left(AssetError.Escaped(name.value))
            else
              try
                Option(dest.getParent).foreach(Files.createDirectories(_))
                payload match
                  case Payload.Text(text) =>
                    Files.writeString(dest, text)
                    Right(())
                  case Payload.Copy(path) =>
                    if !Files.isRegularFile(path) then Left(AssetError.Missing(path))
                    else
                      Files.copy(path, dest, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
                      Right(())
              catch case _: IOException => Left(AssetError.Unreadable(dest))
      .mapError(_ => AssetError.Unreadable(classes))
      .flatMap(ZIO.fromEither)

  private def writeManifest(manifest: Option[Path], names: List[ResourceName]): IO[AssetError, Unit] =
    manifest match
      case None       => ZIO.unit
      case Some(file) =>
        val body =
          val lines = names.map(_.value).sorted
          if lines.isEmpty then "" else lines.mkString("\n") + "\n"
        ZIO
          .attemptBlocking:
            Option(file.getParent).foreach(Files.createDirectories(_))
            Files.writeString(file, body)
          .mapError(_ => AssetError.Unreadable(file))
          .unit
end AssetSync
