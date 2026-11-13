package histos

import histos.report.BuildFault
import histos.server.{AssemblyMark, Call, ServerError, ServerFile}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{UIO, ZIO}

/** The assembly on disk against the ship revision this checkout would publish. The revision is
  * `META-INF/histos/toolchain.json` in the running jar. No source file carries the commit.
  */
object ApiClock:
  val hatchName: String = "allow-drift"

  enum Admit:
    case Go
    case Warn(line: String)
    case Stop(fault: BuildFault)

  private final case class Drift(have: String, want: String)

  def hatchFile(repo: Path): Path =
    repo.resolve(".histos").resolve(hatchName)

  /** `baked` is the revision in the process under test. `current` is the revision this checkout would publish.
    * Production passes the jar resource and [[SnapshotId.current]].
    */
  def judge(repo: Path, call: Call, baked: String, current: Option[String]): UIO[Admit] =
    if allows(call) then ZIO.succeed(Admit.Go)
    else
      drift(repo, baked, current).flatMap {
        case None        => ZIO.succeed(Admit.Go)
        case Some(found) =>
          hatching(repo).map: open =>
            if open then Admit.Warn(warn(found))
            else Admit.Stop(BuildFault.StaleApi(found.have, found.want))
      }

  def admit(repo: Path, call: Call): UIO[Admit] =
    SnapshotId.current(repo, SnapshotId.head).flatMap(now => judge(repo, call, baked, now))

  /** Assembly and version stay on the server already up. Reboot is the command that replaces it. */
  def skipsReboot(call: Call): Boolean =
    call match
      case Call.Assembly(_) | Call.Version => true
      case Call.Watch(inner, _)            => skipsReboot(inner)
      case _                               => false

  def drifted(repo: Path): UIO[Boolean] =
    SnapshotId.current(repo, SnapshotId.head).flatMap(now => drift(repo, baked, now).map(_.isDefined))

  def rebootLine(repo: Path): UIO[String] =
    marks(repo).map: (onto, was, socket) =>
      rebootText(onto, was, socket)

  def restart(repo: Path, socket: Path): UIO[ServerError] =
    AssemblyMark
      .read(repo)
      .zip(record(repo))
      .map: (wanted, stored) =>
        ServerError.Restart(socket, wanted, stored.flatMap(_.assembly))

  def versionLines(repo: Path, versionLine: String): UIO[String] =
    SnapshotId.current(repo, SnapshotId.head).flatMap(now => render(repo, versionLine, baked, now))

  def render(repo: Path, versionLine: String, baked: String, current: Option[String]): UIO[String] =
    AssemblyMark
      .read(repo)
      .map: mark =>
        val held     = present(baked)
        val number   = versionLine.stripPrefix("histos ").trim
        val assembly = mark match
          case Some(hex) => s"server $number assembly ${brief(hex)}"
          case None      => s"server $number"
        val snapshot = (mark, current, held) match
          case (None, _, _)                                => "no local assembly"
          case (_, None, _)                                => "no ship snapshot"
          case (_, Some(want), Some(have)) if have == want => s"snapshot $want"
          case (_, Some(want), have)                       =>
            BuildFault.StaleApi(have.getOrElse(""), want).message
        s"$versionLine\n$assembly\n$snapshot"

  def mcpLine: UIO[String] =
    ZIO.succeed(mcpNote(coordinate))

  def mcpNote(own: Option[String]): String =
    own match
      case Some(here) => s"mcp $here"
      case None       => "mcp is not this assembly"

  def brief(hex: String): String =
    if hex.length <= 8 then hex else hex.take(8)

  def rebootText(onto: Option[String], was: Option[String], socket: String): String =
    val next = onto.fold("none")(brief)
    val prev = was.fold("none")(brief)
    s"rebooting server onto $next (was $prev) at $socket"

  private def allows(call: Call): Boolean =
    call match
      case Call.Assembly(_) | Call.Version | Call.Reboot(_) => true
      case Call.Watch(inner, _)                             => allows(inner)
      case _                                                => false

  /** Some when a local assembly exists and its revision is not the one this checkout would publish. */
  private def drift(repo: Path, baked: String, current: Option[String]): UIO[Option[Drift]] =
    AssemblyMark
      .read(repo)
      .map:
        case None    => None
        case Some(_) =>
          current match
            case None       => None
            case Some(want) =>
              val held = present(baked)
              if held.contains(want) then None else Some(Drift(held.getOrElse(""), want))

  /** The coordinate in this jar. Absent when the running jar has no toolchain resource. */
  private def coordinate: Option[String] =
    present(baked)

  private def baked: String =
    coordinateOf match
      case Some(found) => found
      case None        => ""

  private def coordinateOf: Option[String] =
    val loader = classOf[ToolchainIdentity].getClassLoader
    Option(loader.getResourceAsStream(ToolchainIdentity.entry)).flatMap: stream =>
      try
        val text = String(stream.readAllBytes, StandardCharsets.UTF_8)
        ToolchainIdentity.coordinateOf(text)
      catch case _: java.io.IOException => None
      finally stream.close()

  private def present(raw: String): Option[String] =
    if raw.isEmpty then None else Some(raw)

  private def marks(repo: Path): UIO[(Option[String], Option[String], String)] =
    AssemblyMark
      .read(repo)
      .zip(record(repo))
      .zip(histos.server.SocketPath.at(repo))
      .map: (onto, stored, path) =>
        val socket = stored.map(_.socket).getOrElse(path.toString)
        (onto, stored.flatMap(_.assembly), socket)

  private def record(repo: Path): UIO[Option[ServerFile]] =
    val path = repo.resolve(".histos").resolve("server.json")
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(path) then None
        else ServerFile.parse(Files.readString(path, StandardCharsets.UTF_8))
      }
      .orElseSucceed(None)

  private def hatching(repo: Path): UIO[Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(hatchFile(repo))).orElseSucceed(false)

  private def warn(found: Drift): String =
    s"${BuildFault.StaleApi(found.have, found.want).message} .histos/$hatchName is set, so this command continues."
end ApiClock
