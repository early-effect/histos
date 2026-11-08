package histos.version

/** The next step when a publish session or the releases file is not ready. Doctor and publish share this text. */
object ShipAdvice:
  val snapshotExport: String =
    "export HISTOS_SESSION=snapshot HISTOS_SHA=$(git rev-parse HEAD) HISTOS_BRANCH=default"

  val versionsCommand: String = "histos run versions"

  val releasesFile: String =
    "Create releases (an empty file is fine) or add a line: <ship> matches, <ship> moved, or <ship> unknown."

  /** `publish release` in a development session. A local or snapshot write names HEAD and does not ask for this. */
  def development(ship: ShipName, compile: CiVersion): String =
    s"publishing ${ship.value} ${compile.ship.render} needs export HISTOS_SESSION=release. Then run $versionsCommand."

  def incomplete(error: SessionError): String =
    s"${error.message} $snapshotExport, or export HISTOS_SESSION=release. Then run $versionsCommand."

  def unchecked: String =
    s"the release registry was not checked, so nothing publishes. $releasesFile"

  def unreadable: String =
    s"could not read releases. $releasesFile"

  def shouldBump(ship: ShipName, number: ShipNumber, reason: BumpReason): String =
    val state = reason match
      case BumpReason.Tagged             => s"${ship.value} ${number.render} is already released"
      case BumpReason.Shadowed           => s"${ship.value} ${number.render} is released and has changes since its tag"
      case BumpReason.Published(release) =>
        s"${ship.value} ${release.render} is published and the catalog still names ${number.render}"
    s"$state. Edit Ship(\"${ship.value}\").next(...) to the next number. Do not open a bump pull request."
end ShipAdvice
