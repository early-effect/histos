package histos.load

import histos.cache.ContentStore
import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.*
import histos.schedule.Digest
import histos.workspace.IoFault
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.Locale
import zio.*

/** The hook runs before the non-interruptible pointer switch, never after publication. */
final case class ProductPublication(beforeSwitch: IO[ProductError, Unit])
private enum PublicationState:
  case Prepared, Published

object ProductPublication:
  val live: ProductPublication = ProductPublication(ZIO.unit)

final class StagedOutputs private[load] (
    val request: ProductRequest,
    val destinations: OutputDestinations,
    private[load] val container: Path,
    private[load] val prepared: Ref[Option[OutputManifest]],
)

final case class ProductLocation(output: DeclaredOutput, path: Path)

final case class ProductLocations(values: List[ProductLocation]):
  def file(name: OutputName): Either[ProductError, Path]      = lookup(name, OutputKind.File)
  def directory(name: OutputName): Either[ProductError, Path] = lookup(name, OutputKind.Directory)

  private def lookup(name: OutputName, kind: OutputKind): Either[ProductError, Path] =
    values.find(_.output.name == name) match
      case None => Left(ProductError.Report(PluginRunError.MissingDestination(name)))
      case Some(location) if location.output.kind != kind =>
        Left(ProductError.Report(PluginRunError.OutputKindMismatch(name, kind, location.output.kind)))
      case Some(location) => Right(location.path)
end ProductLocations

final class CommittedOutputs private[load] (
    val manifest: OutputManifest,
    val locations: ProductLocations,
    val read: ZIO[Scope, ProductError, ProductLocations],
)

enum RestoreResult:
  case Hit(outputs: CommittedOutputs)
  case Miss

/** All names switch through one task-wide pointer. Old generations remain until explicit clean so unleased preview
  * requests that have already canonicalized a path can finish. Embedded links are never products.
  */
final class PluginProducts private (
    gates: Ref.Synchronized[Map[Path, Semaphore]],
    readGate: Semaphore,
    readers: Ref[Map[Path, Int]],
    publication: ProductPublication,
):
  import PluginProductFiles.*

  def stage(request: ProductRequest): ZIO[Scope, ProductError, StagedOutputs] =
    for
      _    <- validateRequest(request)
      gate <- lock(request)
      _    <- gate.withPermitScoped
      home = anchor(request)
      _         <- ancestors(base(request), home)
      container <- ZIO.acquireRelease(
        io(home, IoFault.Temp):
          Files.createDirectories(home)
          Files.createTempDirectory(home, ".stage-")
      )(release(_))
      root = container.resolve("writes")
      _ <- io(root, IoFault.Create)(Files.createDirectories(root))
      _ <- ZIO.foreachDiscard(request.task.outputs): output =>
        val path = output.kind match
          case OutputKind.File      => destination(root, output).getParent
          case OutputKind.Directory => destination(root, output)
        io(path, IoFault.Create)(Files.createDirectories(path)).unit
      prepared <- Ref.make(Option.empty[OutputManifest])
    yield StagedOutputs(request, destinations(root, request.task.outputs), container, prepared)

  def seal(staged: StagedOutputs, reported: PluginOutput): ZIO[ContentStore, ProductError, OutputManifest] =
    for
      _ <- ZIO
        .fromEither(PluginProducts.validateReport(staged.request.task.outputs, reported))
        .mapError(ProductError.Report(_))
      source = staged.container.resolve("writes")
      products <- scanAll(staged.request, source)
      ready = staged.container.resolve("ready")
      _      <- delete(ready)
      _      <- io(ready, IoFault.Create)(Files.createDirectories(ready))
      _      <- copyTree(products, source, ready)
      copied <- scanAll(staged.request, ready)
      _      <- ZIO.fail(ProductError.Changed).unless(products == copied)
      manifest = OutputManifest(reported.text, products)
      _ <- persist(manifest, ready)
      _ <- staged.prepared.set(Some(manifest))
    yield manifest

  def commit(staged: StagedOutputs, manifest: OutputManifest): IO[ProductError, CommittedOutputs] =
    for
      prepared <- staged.prepared.get
      _        <- ZIO.fail(ProductError.Changed).unless(prepared.contains(manifest))
      ready = staged.container.resolve("ready")
      actual    <- scanAll(staged.request, ready)
      _         <- ZIO.fail(ProductError.Changed).unless(actual == manifest.products)
      _         <- validateTargets(staged.request)
      committed <- promote(staged, manifest)
    yield committed

  def restore(
      request: ProductRequest,
      manifest: OutputManifest,
  ): ZIO[Scope & ContentStore, ProductError, RestoreResult] =
    for
      _     <- validateRequest(request)
      _     <- ZIO.fromEither(validateManifest(request, manifest))
      store <- ZIO.service[ContentStore]
      hashes = manifest.products.flatMap(_.entries.flatMap(hashOf)).distinct
      blobs <- ZIO.foreach(hashes): hash =>
        store
          .read(digest(hash))
          .mapError(ProductError.Cache(_))
          .flatMap:
            case None        => ZIO.succeed(hash -> Option.empty[Chunk[Byte]])
            case Some(bytes) =>
              ZIO
                .fail(ProductError.Cache(histos.schedule.CacheError.Corrupt(digest(hash))))
                .unless(Sha256Digest.of(bytes) == hash)
                .as(hash -> Some(bytes))
      result <-
        if blobs.exists(_._2.isEmpty) then ZIO.succeed(RestoreResult.Miss)
        else
          for
            staged   <- stage(request)
            existing <- existing(request, manifest)
            result   <- existing match
              case Some(outputs) => ZIO.succeed(RestoreResult.Hit(outputs))
              case None          =>
                val ready = staged.container.resolve("ready")
                for
                  _ <- io(ready, IoFault.Create)(Files.createDirectories(ready))
                  _ <- materialize(manifest, ready, blobs.collect { case (hash, Some(bytes)) => hash -> bytes }.toMap)
                  actual  <- scanAll(request, ready)
                  _       <- ZIO.fail(ProductError.InvalidManifest).unless(actual == manifest.products)
                  _       <- staged.prepared.set(Some(manifest))
                  outputs <- commit(staged, manifest)
                yield RestoreResult.Hit(outputs)
          yield result
    yield result

  def clean(request: ProductRequest): IO[ProductError, Unit] =
    validateRequest(request) *> lock(request).flatMap: gate =>
      gate.withPermit(readGate.withPermit:
        for
          active <- readers.get
          _      <- ZIO
            .fail(ProductError.ActiveReaders)
            .when(active.exists((path, count) => path.startsWith(anchor(request)) && count > 0))
          _ <- ZIO.foreachDiscard(request.task.outputs): output =>
            val target = base(request).resolve(output.path.value)
            ancestors(base(request), target.getParent) *> (output.kind match
              case OutputKind.File      => unlinkFile(target)
              case OutputKind.Directory => delete(target))
          _ <- ancestors(base(request), anchor(request)) *> delete(anchor(request))
        yield ())

  private def promote(staged: StagedOutputs, manifest: OutputManifest): IO[ProductError, CommittedOutputs] =
    val request = staged.request
    val home    = anchor(request)
    val current = home.resolve("current")
    ZIO.scoped:
      for
        created    <- Ref.make(List.empty[Path])
        published  <- Ref.make(PublicationState.Prepared)
        generation <- ZIO.acquireRelease(io(home, IoFault.Temp):
          val generations = home.resolve("generations")
          Files.createDirectories(generations)
          Files.createTempDirectory(generations, "g-"))(generation =>
          published.get.flatMap:
            case PublicationState.Published => ZIO.unit
            case PublicationState.Prepared  =>
              created.get.flatMap(ZIO.foreachDiscard(_)(release(_))) *> release(generation)
        )
        _ <- io(generation, IoFault.Write)(
          Files.move(staged.container.resolve("ready"), generation.resolve("outputs"), StandardCopyOption.ATOMIC_MOVE)
        ).uninterruptible
        _ <- ZIO.foreachDiscard(request.task.outputs): output =>
          val target   = base(request).resolve(output.path.value)
          val expected = destination(current.resolve("outputs"), output)
          io(target, IoFault.Stat)(Files.exists(target, NoLinks)).flatMap:
            case true  => ZIO.unit
            case false =>
              (io(target, IoFault.Create):
                Files.createDirectories(target.getParent)
                Files.createSymbolicLink(target, expected)
              .unit *> created.update(target :: _)).uninterruptible
        temporary = staged.container.resolve("pointer")
        _      <- io(temporary, IoFault.Create)(Files.createSymbolicLink(temporary, generation))
        _      <- validateTargets(request)
        _      <- publication.beforeSwitch
        result <- (io(current, IoFault.Write)(
          Files.move(temporary, current, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        ) *>
          published.set(PublicationState.Published)).as(committed(request, manifest, generation)).uninterruptible
      yield result

  private def committed(request: ProductRequest, manifest: OutputManifest, generation: Path): CommittedOutputs =
    val root  = generation.resolve("outputs")
    val lease = ZIO
      .acquireRelease(
        readGate.withPermit(
          io(generation, IoFault.Stat)(Files.isDirectory(generation, NoLinks)).flatMap:
            case false => ZIO.fail(ProductError.Missing(generation))
            case true  => readers.update(counts => counts.updated(generation, counts.getOrElse(generation, 0) + 1))
        )
      )(_ =>
        readers.update(counts =>
          counts.get(generation) match
            case Some(count) if count > 1 => counts.updated(generation, count - 1)
            case Some(_)                  => counts - generation
            case None                     => counts
        )
      )
      .as(ProductLocations(request.task.outputs.map(output => ProductLocation(output, destination(root, output)))))
    CommittedOutputs(
      manifest,
      ProductLocations(
        request.task.outputs.map(output => ProductLocation(output, base(request).resolve(output.path.value)))
      ),
      lease,
    )

  private def existing(request: ProductRequest, manifest: OutputManifest): IO[ProductError, Option[CommittedOutputs]] =
    val current = anchor(request).resolve("current")
    for
      _       <- validateTargets(request)
      present <- io(current, IoFault.Stat)(
        Files.exists(current, NoLinks) && request.task.outputs.forall(output =>
          Files.exists(base(request).resolve(output.path.value), NoLinks)
        )
      )
      result <-
        if !present then ZIO.none
        else
          for
            generation <- io(current, IoFault.Read)(Files.readSymbolicLink(current))
            actual     <- scanAll(request, generation.resolve("outputs"))
              .map(Some(_))
              .catchSome:
                case ProductError.Missing(_) => ZIO.none
          yield actual.filter(_ == manifest.products).map(_ => committed(request, manifest, generation))
    yield result

  private def validateTargets(request: ProductRequest): IO[ProductError, Unit] =
    val home    = anchor(request)
    val current = home.resolve("current")
    for
      _       <- ancestors(base(request), home.resolve("generations"))
      pointer <- io(current, IoFault.Stat):
        if Files.notExists(current, NoLinks) then Right(())
        else if !Files.isSymbolicLink(current) then Left(ProductError.UnsafePath(current))
        else
          val target = Files.readSymbolicLink(current)
          if target.isAbsolute && target.getParent == home.resolve("generations") &&
            (Files.isDirectory(target, NoLinks) || Files.notExists(target, NoLinks))
          then Right(())
          else Left(ProductError.UnsafePath(current))
      _ <- ZIO.fromEither(pointer)
      _ <- ZIO.foreachDiscard(request.task.outputs): output =>
        val target   = base(request).resolve(output.path.value)
        val expected = destination(current.resolve("outputs"), output)
        ancestors(base(request), target.getParent) *> io(target, IoFault.Stat):
          if Files.notExists(target, NoLinks) then Right(())
          else if Files.isSymbolicLink(target) && Files.readSymbolicLink(target) == expected then Right(())
          else Left(ProductError.Unmanaged(target))
        .flatMap(ZIO.fromEither(_))
    yield ()

  private def validateRequest(request: ProductRequest): IO[ProductError, Unit] =
    val outputs = request.task.outputs
    val paths   = outputs.map(_.path.value)
    val valid   = outputs.map(_.name).distinct.size == outputs.size &&
      paths.distinct.size == paths.size &&
      !paths.exists(path => paths.exists(other => other != path && other.startsWith(path + "/"))) &&
      !paths.exists(path =>
        path
          .split("/", 2)
          .headOption
          .exists(segment => Set(".histos", ".git", ".bsp").contains(segment.toLowerCase(Locale.ROOT)))
      )
    ZIO.fail(ProductError.InvalidManifest).unless(valid).unit

  private def scanAll(request: ProductRequest, root: Path): IO[ProductError, List[NamedProduct]] =
    ancestors(base(request), root) *>
      ZIO.foreach(request.task.outputs.sortBy(_.name.value))(output => scan(output, destination(root, output)))

  private def copyTree(products: List[NamedProduct], from: Path, to: Path): IO[ProductError, Unit] =
    ZIO.foreachDiscard(products): product =>
      val source = destination(from, product.output)
      val target = destination(to, product.output)
      ZIO.foreachDiscard(product.entries): entry =>
        val src = entryPath(source, entry)
        val dst = entryPath(target, entry)
        io(dst, IoFault.Create):
          entry match
            case ProductEntry.RootDirectory | ProductEntry.Directory(_) => Files.createDirectories(dst); ()
            case ProductEntry.RootFile(_) | ProductEntry.File(_, _)     =>
              Files.createDirectories(dst.getParent)
              Files.copy(src, dst)
              ()

  private def persist(manifest: OutputManifest, root: Path): ZIO[ContentStore, ProductError, Unit] =
    for
      store <- ZIO.service[ContentStore]
      now   <- Clock.instant
      _     <- ZIO.foreachDiscard(manifest.products): product =>
        ZIO.foreachDiscard(product.entries): entry =>
          hashOf(entry) match
            case None       => ZIO.unit
            case Some(hash) =>
              val path = entryPath(destination(root, product.output), entry)
              io(path, IoFault.Read)(Chunk.fromArray(Files.readAllBytes(path)))
                .flatMap: bytes =>
                  ZIO.fail(ProductError.Changed).unless(Sha256Digest.of(bytes) == hash) *>
                    store.put(digest(hash), bytes, now).mapError(ProductError.Cache(_))
    yield ()

  private def materialize(
      manifest: OutputManifest,
      root: Path,
      bytes: Map[Sha256, Chunk[Byte]],
  ): IO[ProductError, Unit] =
    ZIO.foreachDiscard(manifest.products): product =>
      ZIO.foreachDiscard(product.entries): entry =>
        val path = entryPath(destination(root, product.output), entry)
        hashOf(entry) match
          case None       => io(path, IoFault.Create)(Files.createDirectories(path)).unit
          case Some(hash) =>
            ZIO
              .fromOption(bytes.get(hash))
              .orElseFail(ProductError.InvalidManifest)
              .flatMap: content =>
                io(path, IoFault.Write):
                  Files.createDirectories(path.getParent)
                  Files.write(path, content.toArray)
                  ()

  private def lock(request: ProductRequest): ZIO[Any, Nothing, Semaphore] =
    gates.modifyZIO: known =>
      known.get(anchor(request)) match
        case Some(gate) => ZIO.succeed((gate, known))
        case None       => Semaphore.make(1).map(gate => (gate, known.updated(anchor(request), gate)))

  private def base(request: ProductRequest): Path = request.repo.toAbsolutePath.normalize

  private def anchor(request: ProductRequest): Path =
    val identity = request.task.outputs
      .sortBy(_.name.value)
      .map(output => s"${output.name.value} ${output.kind} ${output.path.value}")
      .mkString("\n")
    val key = Sha256Digest.of(Chunk.fromArray(identity.getBytes(UTF_8))).value
    base(request).resolve(".histos/products").resolve(key)

  private def entryPath(root: Path, entry: ProductEntry): Path = entry match
    case ProductEntry.RootDirectory | ProductEntry.RootFile(_) => root
    case ProductEntry.Directory(path)                          => root.resolve(path.value)
    case ProductEntry.File(path, _)                            => root.resolve(path.value)

  private def hashOf(entry: ProductEntry): Option[Sha256] = entry match
    case ProductEntry.RootDirectory | ProductEntry.Directory(_) => None
    case ProductEntry.RootFile(hash)                            => Some(hash)
    case ProductEntry.File(_, hash)                             => Some(hash)

  private def digest(hash: Sha256): Digest = Digest.trusted(hash.value)

  private def validateManifest(request: ProductRequest, manifest: OutputManifest): Either[ProductError, Unit] =
    val expected = request.task.outputs.sortBy(_.name.value)
    val valid    = manifest.products.map(_.output) == expected && manifest.products.forall: product =>
      val paths     = product.entries.map(entry => entryPath(Path.of("root"), entry))
      val rootValid = product.entries match
        case ProductEntry.RootDirectory :: _ => product.output.kind == OutputKind.Directory
        case ProductEntry.RootFile(_) :: Nil => product.output.kind == OutputKind.File
        case _                               => false
      rootValid && paths.distinct.size == paths.size && paths == paths.sortBy(_.toString)
    if valid then Right(()) else Left(ProductError.InvalidManifest)

end PluginProducts

object PluginProducts:
  def validateReport(outputs: List[DeclaredOutput], reported: PluginOutput): Either[PluginRunError, Unit] =
    val found = reported.files.map(file => DeclaredOutput.File(file.name, file.path)) ++
      reported.directories.map(dir => DeclaredOutput.Directory(dir.name, dir.path))
    val checked = found.foldLeft[Either[PluginRunError, Set[OutputName]]](Right(Set.empty)): (acc, written) =>
      acc.flatMap: names =>
        outputs.find(_.name == written.name) match
          case None => Left(PluginRunError.ExtraOutput(written.path.value))
          case Some(declared) if names.contains(written.name) || declared.path != written.path =>
            Left(PluginRunError.ExtraOutput(written.path.value))
          case Some(declared) if declared.kind != written.kind =>
            Left(PluginRunError.OutputKindMismatch(written.name, declared.kind, written.kind))
          case Some(_) => Right(names + written.name)
    checked.flatMap: names =>
      outputs.find(output => !names.contains(output.name)) match
        case Some(missing) => Left(PluginRunError.MissingOutput(missing.name, missing.path))
        case None          => Right(())
  def make(publication: ProductPublication = ProductPublication.live): ZIO[Any, Nothing, PluginProducts] =
    for
      gates    <- Ref.Synchronized.make(Map.empty[Path, Semaphore])
      readGate <- Semaphore.make(1)
      readers  <- Ref.make(Map.empty[Path, Int])
    yield new PluginProducts(gates, readGate, readers, publication)

  val live: ZLayer[Any, Nothing, PluginProducts] = ZLayer.fromZIO(make())
end PluginProducts
