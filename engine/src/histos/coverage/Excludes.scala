package histos.coverage

import java.util.regex.{Pattern, PatternSyntaxException}

/** Which exclusion flag a regex belongs to. */
enum ExcludeKind:
  case Classlike, File

  def token: String = this match
    case Classlike => "class"
    case File      => "file"
end ExcludeKind

/** Exclusion regexes the command accepted. Each series joins them with the separator its compiler accepts. */
final case class Excludes(classlikes: List[String], files: List[String])

object Excludes:
  def check(classlikes: List[String], files: List[String]): Either[CoverageError, Excludes] =
    checked(ExcludeKind.Classlike, classlikes)
      .flatMap(_ => checked(ExcludeKind.File, files))
      .map(_ => Excludes(classlikes, files))

  private def checked(kind: ExcludeKind, raws: List[String]): Either[CoverageError, Unit] =
    raws match
      case Nil         => Right(())
      case raw :: rest => compile(kind, raw).flatMap(_ => checked(kind, rest))

  private def compile(kind: ExcludeKind, raw: String): Either[CoverageError, Unit] =
    try
      Pattern.compile(raw)
      Right(())
    catch case _: PatternSyntaxException => Left(CoverageError.BadRegex(kind, raw))
end Excludes
