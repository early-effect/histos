package histos.pack

import histos.resolve.{LockError, Repositories, Repository, Resolve}

import java.nio.file.Path

import zio.{Console, ExitCode, Scope, ZIO, ZIOAppDefault}

/** `java -cp <resolved> histos.pack.Pack`. The shim resolves the coordinate, then this writes the toolchain jar. */
object Pack extends ZIOAppDefault:
  override val bootstrap =
    zio.Runtime.enableLoomBasedExecutor ++ zio.Runtime.enableLoomBasedBlockingExecutor

  def run =
    for
      args <- getArgs.map(_.toList)
      code <- execute(args)
    yield code

  private def execute(args: List[String]) =
    (for
      request <- ZIO.fromEither(Args.read(args))
      _       <- Distribution
        .install(request.coordinate, request.repositories, request.out)
        .mapError(PackError.Failed(_))
        .provideSomeLayer[Scope](Resolve.recheck)
    yield ()).foldZIO(
      error => Console.printLineError(error.message).ignore.as(ExitCode.failure),
      _ => ZIO.succeed(ExitCode.success),
    )
end Pack

enum PackError:
  case Usage(text: String)
  case Repository(error: LockError)
  case Failed(error: DistributionError)

  def message: String = this match
    case Usage(text)       => text
    case Repository(error) => error.message
    case Failed(error)     => error.message
end PackError

private object Args:
  final case class Request(out: Path, coordinate: Coordinate, repositories: Repositories)

  def read(args: List[String]): Either[PackError, Request] =
    gather(args, Draft()).flatMap(finish)

  private final case class Draft(
      out: Option[Path] = None,
      organization: Option[String] = None,
      name: Option[String] = None,
      revision: Option[String] = None,
      repositories: List[Repository] = Nil,
  )

  private def gather(args: List[String], draft: Draft): Either[PackError, Draft] =
    args match
      case Nil                      => Right(draft)
      case "--out" :: value :: rest =>
        fresh(value, "--out", draft.out.isDefined).flatMap: path =>
          gather(rest, draft.copy(out = Some(Path.of(path))))
      case "--group" :: value :: rest =>
        fresh(value, "--group", draft.organization.isDefined).flatMap: organization =>
          gather(rest, draft.copy(organization = Some(organization)))
      case "--artifact" :: value :: rest =>
        fresh(value, "--artifact", draft.name.isDefined).flatMap: name =>
          gather(rest, draft.copy(name = Some(name)))
      case "--revision" :: value :: rest =>
        fresh(value, "--revision", draft.revision.isDefined).flatMap: revision =>
          gather(rest, draft.copy(revision = Some(revision)))
      case "--repository" :: value :: rest =>
        if value.isEmpty || value.startsWith("--") then Left(PackError.Usage("--repository needs a value"))
        else
          Repository.parse(value) match
            case Left(error)       => Left(PackError.Repository(error))
            case Right(repository) => gather(rest, draft.copy(repositories = draft.repositories :+ repository))
      case other :: _ => Left(PackError.Usage(s"unexpected argument: $other"))

  private def fresh(value: String, flag: String, seen: Boolean): Either[PackError, String] =
    if seen then Left(PackError.Usage(s"$flag is repeated"))
    else if value.isEmpty || value.startsWith("--") then Left(PackError.Usage(s"$flag needs a value"))
    else Right(value)

  private def finish(draft: Draft): Either[PackError, Request] =
    (draft.out, draft.organization, draft.name, draft.revision) match
      case (Some(out), Some(organization), Some(name), Some(revision)) =>
        Right(Request(out, Coordinate(organization, name, revision), repositories(draft.repositories)))
      case _ =>
        Left(PackError.Usage("need --out, --group, --artifact, and --revision"))

  private def repositories(listed: List[Repository]): Repositories =
    listed.distinct match
      case first :: rest => Repositories(first, rest)
      case Nil           => Repositories.central
end Args
