package histos.program

import histos.cache.{ContentStore, LinkerStarts}
import histos.jdk.{Machine, Toolchain}
import histos.native.{NativeError, NativeLink, NativeLinkRequest, NativeOrigin, NativeToolchain}
import histos.resolve.{BuildResolvers, DependencyLock, LockedTools, ResolutionService, Resolve}
import histos.schedule.RemoteCache
import histos.{ClangMajor, Resolution, ToolCatalog}

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, ZLayer}

enum ClientLinkError:
  case KitMissing(path: Path)
  case KitCorrupt(detail: String)
  case Clang(detail: String)
  case Jdk(detail: String)
  case Link(detail: String)
  case Work(path: Path)

  def message: String = this match
    case KitMissing(path) =>
      s"no client kit at $path. Publish the toolchain again, then run histos toolchain client."
    case KitCorrupt(detail) => detail
    case Clang(detail)      => detail
    case Jdk(detail)        => detail
    case Link(detail)       => detail
    case Work(path)         => s"could not prepare $path"
end ClientLinkError

object ClientLink:
  /** Link the kit beside the running jar. The binary lands next to that jar. */
  def run: ZIO[Resolve & Scope, ClientLinkError, Path] =
    for
      jar    <- locate
      parent <- parentOf(jar)
      kit = parent.resolve(ClientKit.FileName)
      present <- ZIO.attemptBlocking(Files.isRegularFile(kit)).orElseFail(ClientLinkError.KitMissing(kit))
      _       <- ZIO.unless(present)(ZIO.fail(ClientLinkError.KitMissing(kit)))
      linked  <- link(kit, parent.resolve(ClientBinary.fileName))
    yield linked

  private def locate: ZIO[Any, ClientLinkError, Path] =
    zio.System
      .env("HISTOS_JAR")
      .orElseSucceed(None)
      .flatMap:
        case Some(raw) if raw.nonEmpty =>
          val path = Path.of(raw)
          ZIO
            .attemptBlocking(Files.isRegularFile(path))
            .orElseSucceed(false)
            .flatMap: found =>
              if found then ZIO.succeed(path) else ZIO.fail(ClientLinkError.KitMissing(path))
        case _ =>
          codeSource(classOf[ClientKit]) match
            case Some(path) => ZIO.succeed(path)
            case None       => ZIO.fail(ClientLinkError.KitMissing(Path.of("histos.jar")))

  private def codeSource(cls: Class[?]): Option[Path] =
    Option(cls.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap: source =>
        Option(source.getLocation).flatMap: url =>
          scala.util.Try(Path.of(url.toURI)).toOption

  private def parentOf(path: Path): ZIO[Any, ClientLinkError, Path] =
    Option(path.getParent) match
      case Some(parent) => ZIO.succeed(parent)
      case None         => ZIO.fail(ClientLinkError.Work(path))

  private def link(kit: Path, destination: Path): ZIO[Resolve & Scope, ClientLinkError, Path] =
    for
      parent <- parentOf(destination)
      work = parent.resolve("client-link")
      linked <- ZIO.acquireRelease(prepare(work))(dir => ZIO.attemptBlocking(deleteTree(dir)).ignore).flatMap { root =>
        build(kit, destination, root)
      }
    yield linked

  private def prepare(work: Path): ZIO[Any, ClientLinkError, Path] =
    ZIO.attemptBlocking(Files.createDirectories(work)).mapBoth(_ => ClientLinkError.Work(work), _ => work)

  private def build(kit: Path, destination: Path, root: Path): ZIO[Resolve, ClientLinkError, Path] =
    for
      unpacked <- ClientKit.read(kit, root.resolve("out")).mapError(carried(kit))
      machine  <- Machine.load(root).mapError(error => ClientLinkError.Jdk(error.message))
      jdk      <- Toolchain.resolve(machine).mapError(error => ClientLinkError.Jdk(error.message))
      service  <- ResolutionService
        .open(root, Resolution.Solve)
        .mapError(error => ClientLinkError.Link(error.message))
      resolve <- ZIO.service[Resolve]
      tools = LockedTools(
        DependencyLock(Nil),
        ToolCatalog.default,
        BuildResolvers(Nil),
        resolve,
        Some(service),
      )
      origin = NativeOrigin.read(machine.env)
      toolchain <- ZIO
        .fromEither(NativeToolchain.of(unpacked.native, origin))
        .mapError(error => ClientLinkError.Link(error.message))
      binary <- NativeLink
        .linkClasspath(
          NativeLinkRequest(
            unpacked.clang,
            toolchain,
            machine.env,
            root.resolve("link"),
            jdk,
            unpacked.main,
            unpacked.scala,
            optimize = true,
          ),
          unpacked.classpath,
        )
        .provide(
          ZLayer.succeed(tools) ++
            ZLayer.succeed(ContentStore.in(root.resolve("store"))) ++
            RemoteCache.off ++
            LinkerStarts.live
        )
        .mapError(error => linked(unpacked.clang, error))
      placed <- copy(binary, destination)
    yield placed

  private def carried(kit: Path)(failure: ClientKit.Failure): ClientLinkError =
    failure match
      case ClientKit.Failure.Read(_)         => ClientLinkError.KitMissing(kit)
      case ClientKit.Failure.Write(path)     => ClientLinkError.Work(path)
      case ClientKit.Failure.Corrupt(detail) => ClientLinkError.KitCorrupt(detail)

  private def linked(major: ClangMajor, error: NativeError): ClientLinkError =
    error match
      case NativeError.Clang(miss) =>
        ClientLinkError.Clang(
          s"${miss.message}. histos toolchain client builds the native client once clang ${major.value} is on PATH. Histos does not install clang."
        )
      case other => ClientLinkError.Link(other.message)

  private def copy(binary: Path, dest: Path): ZIO[Any, ClientLinkError, Path] =
    parentOf(dest).flatMap: parent =>
      ZIO
        .attemptBlocking:
          Files.createDirectories(parent)
          Files.copy(binary, dest, StandardCopyOption.REPLACE_EXISTING)
          Files.setPosixFilePermissions(dest, PosixFilePermissions.fromString("rwxr-xr-x"))
          dest
        .mapError(_ => ClientLinkError.Work(dest))

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
end ClientLink
