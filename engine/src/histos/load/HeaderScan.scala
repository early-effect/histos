package histos.load

import histos.plugin.{LinkStrategy, PluginLoadError}
import histos.resolve.{PluginRequest, Resolve}
import histos.{Dep, ProjectPath}

/** Header requests, and the compilation unit with those lines blanked. */
final case class ScannedBuild(requests: List[PluginRequest], source: String)

/** Text scan of the `histos.scala` prefix. It does not resolve, lock, or compile. */
private[histos] object HeaderScan:
  def scan(text: String): Either[PluginLoadError, ScannedBuild] =
    walk(text.split("\n", -1).toList, 1, header = true, Nil, Nil)

  /** Path-plugin directories. A failed scan contributes none: the build file bytes already changed. */
  def paths(text: String): List[ProjectPath] =
    scan(text) match
      case Right(scanned) => scanned.requests.collect { case PluginRequest.Path(path) => path }
      case Left(_)        => Nil

  @scala.annotation.tailrec
  private def walk(
      lines: List[String],
      number: Int,
      header: Boolean,
      requests: List[PluginRequest],
      kept: List[String],
  ): Either[PluginLoadError, ScannedBuild] =
    lines match
      case Nil => Right(ScannedBuild(requests.reverse, kept.reverse.mkString("\n")))
      case line :: rest =>
        val trimmed = line.trim
        if header && (trimmed.isEmpty || trimmed.startsWith("//")) then
          walk(rest, number + 1, true, requests, line :: kept)
        else
          attempt(trimmed) match
            case Some(Right(request)) if header =>
              walk(rest, number + 1, true, request :: requests, "" :: kept)
            case Some(Left(error)) if header => Left(error)
            case Some(_)                      => Left(PluginLoadError.Misplaced(number))
            case None if header && trimmed.startsWith("plugin(") =>
              Left(PluginLoadError.Misplaced(number))
            case None =>
              walk(rest, number + 1, false, requests, line :: kept)

  private def attempt(line: String): Option[Either[PluginLoadError, PluginRequest]] =
    if line.startsWith("plugin(dep\"") then Some(parse(line, dependency = true))
    else if line.startsWith("plugin(path\"") then Some(parse(line, dependency = false))
    else None

  private def parse(line: String, dependency: Boolean): Either[PluginLoadError, PluginRequest] =
    val prefix = if dependency then "plugin(dep\"" else "plugin(path\""
    val close = line.indexOf("\")", prefix.length)
    if close < 0 then Left(PluginLoadError.BadCoordinate(line))
    else
      val raw = line.substring(prefix.length, close)
      linkOf(line, line.substring(close + 2)).flatMap: link =>
        if dependency then depRequest(raw, link) else pathRequest(raw, link)

  private def linkOf(line: String, rest: String): Either[PluginLoadError, Option[LinkStrategy]] =
    if rest.isEmpty then Right(None)
    else
      val prefix = ".link("
      if rest.startsWith(prefix) && rest.endsWith(")") && rest.length > prefix.length + 1 then
        strategy(rest.substring(prefix.length, rest.length - 1)) match
          case Some(link) => Right(Some(link))
          case None       => Left(PluginLoadError.BadCoordinate(line))
      else Left(PluginLoadError.BadCoordinate(line))

  private def strategy(name: String): Option[LinkStrategy] =
    name match
      case "ExactBinary"    => Some(LinkStrategy.ExactBinary)
      case "Source"         => Some(LinkStrategy.Source)
      case "BinaryOrSource" => Some(LinkStrategy.BinaryOrSource)
      case "Api"            => Some(LinkStrategy.Api)
      case _                => None

  /** A dep with no `.link` is `BinaryOrSource`. `Api` is refused here and writes no lock cell. */
  private def depRequest(raw: String, link: Option[LinkStrategy]): Either[PluginLoadError, PluginRequest] =
    Dep.from(raw) match
      case Left(_)                                         => Left(PluginLoadError.BadCoordinate(raw))
      case Right(dependency) if Resolve.ranged(dependency) => Left(PluginLoadError.BadCoordinate(raw))
      case Right(dependency)                               =>
        link match
          case Some(LinkStrategy.Api) => Left(PluginLoadError.ApiLink(dependency))
          case Some(chosen)           => Right(PluginRequest.Dep(dependency, chosen))
          case None                   => Right(PluginRequest.Dep(dependency, LinkStrategy.BinaryOrSource))

  private def pathRequest(raw: String, link: Option[LinkStrategy]): Either[PluginLoadError, PluginRequest] =
    ProjectPath.from(raw) match
      case Left(_)     => Left(PluginLoadError.BadCoordinate(raw))
      case Right(path) =>
        link match
          case None | Some(LinkStrategy.Source) => Right(PluginRequest.Path(path))
          case Some(_)                          => Left(PluginLoadError.PathIsSource(path))
