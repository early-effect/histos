package histos.scaffold

import histos.jdk.Machine
import histos.load.{LoadError, Loader}
import histos.publish.{LauncherScript, PublishedShim, ScriptError, UpgradeNotice}
import histos.Sona
import histos.resolve.{BuildResolvers, Resolve, ResolveError}
import histos.supply.DoctorSurface
import histos.workflow.{CiBranch, CiError, RenderingShip, Workflow}
import histos.{BuildError, ProjectName, Publishing, ToolchainIdentity}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, UIO, URIO, ZIO, ZLayer}

/** `histos new` and `histos init`. Full writes the scaffold, then CI, a lock, and doctor outside CI. The doctor reads
  * the registry feed, git, and the terminal from the caller's environment.
  */
object Provision:
  val DefaultBranch: String = "main"

  final case class Request(
      kind: ScaffoldKind,
      source: Option[ImportSource],
      branch: Option[String],
      askBranch: UIO[String],
      env: Map[String, String],
  )

  final case class Report(dir: Path, out: String)

  def create(
      parent: Path,
      name: String,
      version: String,
      apiJar: Path,
      machine: Machine,
      request: Request,
  ): ZIO[Loader & Resolve & DoctorSurface.Needs, ProvisionError, Report] =
    prepare(request.kind).flatMap { ready =>
      Skeleton
        .create(parent, name, version, request.kind, ready.shim)
        .mapError(ProvisionError.Files(_))
        .flatMap(dir => finish(dir, apiJar, machine, request, ready))
    }

  def init(
      dir: Path,
      version: String,
      apiJar: Path,
      machine: Machine,
      request: Request,
  ): ZIO[Loader & Resolve & DoctorSurface.Needs, ProvisionError, Report] =
    prepare(request.kind).flatMap { ready =>
      draft(dir, request.source).flatMap { text =>
        Skeleton
          .init(dir, version, request.kind, text, ready.shim)
          .mapError(ProvisionError.Files(_))
          .flatMap(written => finish(written, apiJar, machine, request, ready))
      }
    }

  private def draft(dir: Path, source: Option[ImportSource]): IO[ProvisionError, Option[String]] =
    source match
      case None                   => ZIO.succeed(None)
      case Some(ImportSource.Sbt) =>
        val build   = dir.resolve("build.sbt")
        val plugins = dir.resolve("project").resolve("plugins.sbt")
        readRequired(build, "no build.sbt").flatMap { text =>
          readOptional(plugins).map(extra => Some(ImportDraft.sbt(text, extra, directoryName(dir))))
        }
      case Some(ImportSource.Zipx) =>
        val versions = dir.resolve("project").resolve("ZipxVersions.scala")
        readRequired(versions, "no project/ZipxVersions.scala").flatMap { text =>
          modulesOf(dir).map(names => Some(ImportDraft.zipx(text, names, directoryName(dir))))
        }

  private final case class Ready(shim: Option[histos.jdk.Sha256], ship: Option[RenderingShip])

  private def prepare(kind: ScaffoldKind): IO[ProvisionError, Ready] =
    kind match
      case ScaffoldKind.Bare => ZIO.succeed(Ready(None, None))
      case ScaffoldKind.Full =>
        for
          sha  <- ZIO.fromEither(PublishedShim.load).mapError(error => ProvisionError.Workflow(error.message))
          ship <- rendering
        yield Ready(Some(sha), Some(ship))

  private def rendering: IO[ProvisionError, RenderingShip] =
    ToolchainIdentity.loaded match
      case None        => ZIO.fail(ProvisionError.Workflow(CiError.NoShip.message))
      case Some(found) =>
        ZIO.fromEither(RenderingShip.from(found.commit)).mapError(error => ProvisionError.Workflow(error.message))

  private def finish(
      dir: Path,
      apiJar: Path,
      machine: Machine,
      request: Request,
      ready: Ready,
  ): ZIO[Loader & Resolve & DoctorSurface.Needs, ProvisionError, Report] =
    val head = s"${dir.toAbsolutePath.normalize}\nhistos test"
    request.kind match
      case ScaffoldKind.Bare => ZIO.succeed(Report(dir, head))
      case ScaffoldKind.Full =>
        (ready.shim, ready.ship) match
          case (None, _)               => ZIO.fail(ProvisionError.Workflow(ScriptError.Shim.message))
          case (_, None)               => ZIO.fail(ProvisionError.Workflow(CiError.NoShip.message))
          case (Some(sha), Some(ship)) =>
            loaded(dir, apiJar).flatMap: (graph, sona) =>
              branchOf(dir, request).flatMap: name =>
                val branch = CiBranch(name)
                for
                  file <- Workflow
                    .update(dir, graph, sona)(using branch, ship)
                    .mapError:
                      case error: CiError    => ProvisionError.Workflow(error.message)
                      case error: BuildError => ProvisionError.Load(LoadError.BrokenGraph(error))
                  _    <- LauncherScript.update(dir, sha).mapError(error => ProvisionError.Workflow(error.message))
                  lock <- Resolve
                    .update(dir, graph, BuildResolvers(graph.resolvers), machine.native)
                    .mapError(ProvisionError.Lock(_))
                  report <- doctor(dir, graph, request.env, machine)
                yield
                  val notes = List(file.toString, lock, report).filter(_.nonEmpty).mkString("\n")
                  val out   = if notes.isEmpty then head else s"$head\n$notes"
                  Report(dir, out)

  private def loaded(dir: Path, apiJar: Path): ZIO[Loader, ProvisionError, (histos.ProjectGraph, Publishing)] =
    ZIO.serviceWithZIO[Loader]: loader =>
      ZIO.scoped {
        loader.load(dir, apiJar).mapError(ProvisionError.Load(_)).flatMap { loaded =>
          ZIO
            .service[Publishing]
            .provideLayer(ZLayer.succeed(Sona.idle) >>> loaded.build.sona)
            .map(sona => (loaded.graph, sona))
        }
      }

  private def branchOf(dir: Path, request: Request): IO[ProvisionError, String] =
    request.branch match
      case Some(name) => ZIO.fromEither(accepted(name))
      case None       =>
        CiBranch
          .read(dir)
          .either
          .flatMap:
            case Right(branch)                 => ZIO.succeed(branch.name)
            case Left(CiError.NoDefaultBranch) => request.askBranch.flatMap(name => ZIO.fromEither(accepted(name)))
            case Left(error)                   => ZIO.fail(ProvisionError.Workflow(error.message))

  private def accepted(raw: String): Either[ProvisionError, String] =
    if raw.nonEmpty && raw.forall(char => !char.isWhitespace && !char.isControl) then Right(raw)
    else Left(ProvisionError.Branch(raw))

  private def doctor(
      dir: Path,
      graph: histos.ProjectGraph,
      env: Map[String, String],
      machine: Machine,
  ): URIO[DoctorSurface.Needs, String] =
    if UpgradeNotice.inCi(env) then ZIO.succeed("")
    else
      DoctorSurface
        .run(dir, graph, DoctorSurface.Request(None, false, None, false, true, Nil, false), machine)
        .map(_._2)

  private def directoryName(dir: Path): String =
    val raw = Option(dir.getFileName).fold("app")(_.toString)
    if ProjectName.from(raw).isRight then raw else "app"

  private def modulesOf(dir: Path): IO[ProvisionError, List[String]] =
    ZIO
      .attemptBlocking:
        val children = Files.list(dir)
        val names    =
          try
            children.iterator.asScala
              .filter(child => Files.isDirectory(child))
              .flatMap { child =>
                val name = Option(child.getFileName).fold("")(_.toString)
                if name.startsWith(".") || name == "project" || name == "target" then None
                else if Files.isDirectory(child.resolve("src")) && ProjectName.from(name).isRight then Some(name)
                else None
              }
              .toList
              .sorted
          finally children.close()
        if names.nonEmpty then names
        else if Files.isDirectory(dir.resolve("src")) && ProjectName.from(directoryName(dir)).isRight then
          List(directoryName(dir))
        else Nil
      .mapError(_ => ProvisionError.Import(s"could not read $dir"))

  private def readRequired(path: Path, missing: String): IO[ProvisionError, String] =
    ZIO
      .attemptBlocking:
        if !Files.isRegularFile(path) then Left(missing) else Right(Files.readString(path))
      .mapError(_ => ProvisionError.Import(s"could not read $path"))
      .flatMap(ZIO.fromEither(_).mapError(ProvisionError.Import(_)))

  private def readOptional(path: Path): IO[ProvisionError, String] =
    ZIO
      .attemptBlocking:
        if Files.isRegularFile(path) then Files.readString(path) else ""
      .mapError(_ => ProvisionError.Import(s"could not read $path"))
end Provision

enum ProvisionError:
  case Files(error: SkeletonError)
  case Import(detail: String)
  case Load(error: LoadError)
  case Workflow(detail: String)
  case Lock(error: ResolveError)
  case Branch(raw: String)

  def message: String = this match
    case Files(error)     => error.message
    case Import(detail)   => detail
    case Load(error)      => error.message
    case Workflow(detail) => detail
    case Lock(error)      =>
      s"${error.message}\nThe project is written. Run 'histos resolve --update'."
    case Branch(raw) => s"'$raw' is not a branch name"
end ProvisionError
