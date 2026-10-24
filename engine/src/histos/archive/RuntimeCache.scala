package histos.archive

import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, LinkOption, Path}
import java.nio.file.attribute.{
  AclEntry,
  AclEntryFlag,
  AclEntryPermission,
  AclEntryType,
  AclFileAttributeView,
  FileAttribute,
  PosixFileAttributeView,
  PosixFilePermission,
  PosixFilePermissions,
  UserPrincipal
}
import java.util.EnumSet

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum RuntimeCacheError:
  case Shared(path: Path)
  case Owner(path: Path)
  case NotDirectory(path: Path)
  case Unsupported(path: Path)
  case Io(failure: IoFailure)

  def message: String = this match
    case Shared(path)       => s"runtime cache is not owner-private: $path"
    case Owner(path)        => s"runtime cache has an untrusted owner: $path"
    case NotDirectory(path) => s"runtime cache is not a directory: $path"
    case Unsupported(path)  =>
      s"runtime cache permissions cannot be secured: $path"
    case Io(failure) => failure.message

private[histos] object RuntimeCache:
  private val privateMode = PosixFilePermissions.fromString("rwx------")
  private val others = Set(
    PosixFilePermission.GROUP_READ,
    PosixFilePermission.GROUP_WRITE,
    PosixFilePermission.GROUP_EXECUTE,
    PosixFilePermission.OTHERS_READ,
    PosixFilePermission.OTHERS_WRITE,
    PosixFilePermission.OTHERS_EXECUTE
  )
  private val writes = Set(
    AclEntryPermission.WRITE_DATA,
    AclEntryPermission.APPEND_DATA,
    AclEntryPermission.DELETE_CHILD,
    AclEntryPermission.WRITE_ACL,
    AclEntryPermission.WRITE_OWNER,
    AclEntryPermission.DELETE
  )

  def prepare(root: Path): IO[RuntimeCacheError, Unit] = owner.flatMap:
    principal =>
      for
        // Lexical normalization can erase an alias before physical ancestry is checked.
        absolute <- io(root, IoFault.Stat)(root.toAbsolutePath)
        filesystemRoot <- ZIO
          .fromOption(Option(absolute.getRoot))
          .orElseFail(RuntimeCacheError.Unsupported(absolute))
        system <- io(filesystemRoot, IoFault.Stat)(
          Files.getOwner(filesystemRoot)
        )
        administrators <- io(filesystemRoot, IoFault.Stat):
          Option(
            Files.getFileAttributeView(
              filesystemRoot,
              classOf[AclFileAttributeView]
            )
          ).toList
            .flatMap(view =>
              view.getAcl.asScala.collect:
                case entry
                    if entry.`type`() == AclEntryType.ALLOW && entry
                      .permissions()
                      .contains(AclEntryPermission.WRITE_OWNER) && entry
                      .permissions()
                      .contains(AclEntryPermission.WRITE_ACL) =>
                  entry.principal()
            )
            .toSet + system
        ancestors = Iterator
          .iterate(Option(absolute.getParent))(
            _.flatMap(path => Option(path.getParent))
          )
          .takeWhile(_.nonEmpty)
          .flatten
          .toList
          .reverse
        _ <- ZIO.foreachDiscard(ancestors)(path =>
          ensure(path, principal, administrators) *>
            ancestor(path, principal, administrators) *>
            physicalAncestors(path, principal, administrators)
        )
        _ <- ensure(absolute, principal, Set.empty)
        _ <- physicalAncestors(absolute, principal, administrators)
        _ <- privateDirectory(absolute, principal)
      yield ()

  private def owner: IO[RuntimeCacheError, UserPrincipal] =
    ZIO.scoped:
      // A newly created private directory supplies the process's filesystem principal without another environment reader.
      Workspace
        .work("histos-runtime-owner")
        .mapError(RuntimeCacheError.Io(_))
        .flatMap(path => io(path, IoFault.Stat)(Files.getOwner(path)))

  private def ensure(
      path: Path,
      principal: UserPrincipal,
      allowedOwners: Set[UserPrincipal]
  ): IO[RuntimeCacheError, Unit] =
    for
      exists <- io(path, IoFault.Stat)(
        Files.exists(path, LinkOption.NOFOLLOW_LINKS)
      )
      _ <-
        if exists then ZIO.unit
        else
          create(path, principal).catchSome {
            case RuntimeCacheError.Io(failure @ IoFailure(_, IoFault.Create)) =>
              io(path, IoFault.Stat)(Files.isDirectory(path)).flatMap(present =>
                if present then ZIO.unit
                else ZIO.fail(RuntimeCacheError.Io(failure))
              )
          }
      actual <- io(path, IoFault.Stat)(
        Files.getOwner(path, LinkOption.NOFOLLOW_LINKS)
      )
      _ <- ZIO
        .fail(RuntimeCacheError.Owner(path))
        .unless(actual == principal || allowedOwners.contains(actual))
      directory <- io(path, IoFault.Stat)(Files.isDirectory(path))
      _ <- ZIO.fail(RuntimeCacheError.NotDirectory(path)).unless(directory)
    yield ()

  private def physicalAncestors(
      path: Path,
      principal: UserPrincipal,
      administrators: Set[UserPrincipal]
  ): IO[RuntimeCacheError, Unit] =
    io(path, IoFault.Stat)(path.toRealPath()).flatMap: physical =>
      val chain = Iterator
        .iterate(Option(physical))(
          _.flatMap(parent => Option(parent.getParent))
        )
        .takeWhile(_.nonEmpty)
        .flatten
        .toList
        .reverse
      ZIO.foreachDiscard(chain)(ancestor(_, principal, administrators))

  private def create(
      path: Path,
      principal: UserPrincipal
  ): IO[RuntimeCacheError, Unit] =
    for
      views <- io(path, IoFault.Stat)(
        path.getFileSystem.supportedFileAttributeViews()
      )
      _ <-
        if views.contains("posix") then
          io(path, IoFault.Create)(
            Files.createDirectory(
              path,
              PosixFilePermissions.asFileAttribute(privateMode)
            )
          ).unit
        else if views.contains("acl") then
          val attribute = new FileAttribute[java.util.List[AclEntry]]:
            def name(): String = "acl:acl"
            def value(): java.util.List[AclEntry] = List(
              ownerEntry(principal)
            ).asJava
          io(path, IoFault.Create)(Files.createDirectory(path, attribute)).unit
        else ZIO.fail(RuntimeCacheError.Unsupported(path))
    yield ()

  private def privateDirectory(
      path: Path,
      principal: UserPrincipal
  ): IO[RuntimeCacheError, Unit] =
    for
      actual <- io(path, IoFault.Stat)(Files.getOwner(path))
      _ <- ZIO.fail(RuntimeCacheError.Owner(path)).unless(actual == principal)
      directory <- io(path, IoFault.Stat)(Files.isDirectory(path))
      _ <- ZIO.fail(RuntimeCacheError.NotDirectory(path)).unless(directory)
      posix <- io(path, IoFault.Stat)(
        Option(
          Files.getFileAttributeView(path, classOf[PosixFileAttributeView])
        )
      )
      _ <- posix match
        case Some(view) =>
          io(path, IoFault.Stat)(
            view.readAttributes().permissions().asScala.toSet
          ).flatMap(mode =>
            ZIO
              .fail(RuntimeCacheError.Shared(path))
              .when(mode.exists(others.contains))
          ).unit
        case None =>
          acl(path).flatMap(view =>
            io(path, IoFault.Stat)(view.getAcl.asScala.toList)
              .flatMap(entries =>
                ZIO
                  .fail(RuntimeCacheError.Shared(path))
                  .when(
                    entries.exists(entry =>
                      entry.`type`() == AclEntryType.ALLOW && entry
                        .principal() != principal
                    )
                  )
              )
              .unit
          )
    yield ()

  private def ancestor(
      path: Path,
      principal: UserPrincipal,
      administrators: Set[UserPrincipal]
  ): IO[RuntimeCacheError, Unit] =
    for
      actual <- io(path, IoFault.Stat)(Files.getOwner(path))
      _ <- ZIO
        .fail(RuntimeCacheError.Owner(path))
        .unless(actual == principal || administrators.contains(actual))
      posix <- io(path, IoFault.Stat)(
        Option(
          Files.getFileAttributeView(path, classOf[PosixFileAttributeView])
        )
      )
      _ <- posix match
        case Some(view) =>
          for
            mode <- io(path, IoFault.Stat)(
              view.readAttributes().permissions().asScala.toSet
            )
            sticky <- io(path, IoFault.Stat)(
              Files.getAttribute(path, "unix:mode") match
                case value: java.lang.Integer => (value.intValue() & 512) != 0
                case _                        => false
            )
            _ <- ZIO
              .fail(RuntimeCacheError.Shared(path))
              .when(
                !sticky && (mode.contains(
                  PosixFilePermission.GROUP_WRITE
                ) || mode.contains(PosixFilePermission.OTHERS_WRITE))
              )
          yield ()
        case None =>
          acl(path).flatMap(view =>
            io(path, IoFault.Stat)(view.getAcl.asScala.toList)
              .flatMap(entries =>
                ZIO
                  .fail(RuntimeCacheError.Shared(path))
                  .when(
                    entries.exists(entry =>
                      entry.`type`() == AclEntryType.ALLOW && entry
                        .principal() != principal && !administrators
                        .contains(entry.principal()) && entry
                        .permissions()
                        .asScala
                        .exists(writes.contains)
                    )
                  )
              )
              .unit
          )
    yield ()

  private def acl(path: Path): IO[RuntimeCacheError, AclFileAttributeView] =
    io(path, IoFault.Stat)(
      Option(Files.getFileAttributeView(path, classOf[AclFileAttributeView]))
    ).someOrFail(RuntimeCacheError.Unsupported(path))

  private def ownerEntry(principal: UserPrincipal): AclEntry = AclEntry
    .newBuilder()
    .setType(AclEntryType.ALLOW)
    .setPrincipal(principal)
    .setPermissions(EnumSet.allOf(classOf[AclEntryPermission]))
    .setFlags(AclEntryFlag.DIRECTORY_INHERIT, AclEntryFlag.FILE_INHERIT)
    .build()

  private def io[A](path: Path, fault: IoFault)(
      value: => A
  ): IO[RuntimeCacheError, A] =
    ZIO
      .attemptBlocking(value)
      .mapError(_ => RuntimeCacheError.Io(IoFailure(path, fault)))
