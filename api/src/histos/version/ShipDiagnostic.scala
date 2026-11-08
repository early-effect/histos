package histos.version

import histos.doctor.{CheckId, Diagnostic, Fix, Level, ShellLine}

private def fixes(head: Fix, tail: Fix*): ::[Fix] = ::(head, tail.toList)

private def lines(head: String, tail: String*): ::[String] = ::(head, tail.toList)

/** Ship findings. The first fix is the one to apply. Nothing opens a bump pull request. */
enum ShipDiagnostic(
    val check: CheckId,
    val level: Level,
    val title: String,
    val summary: String,
    val explain: String,
    val fixes: ::[Fix],
) extends Diagnostic:
  case Session(detail: String)
      extends ShipDiagnostic(
        CheckId.trusted("ship.session"),
        Level.Error,
        "the publish session is not ready",
        detail,
        "A snapshot session needs HISTOS_SHA and HISTOS_BRANCH. A pull-request session needs HISTOS_PR and HISTOS_SHA. publish release needs HISTOS_SESSION=release. publish local and publish snapshots use the commit at HEAD when the session is development.",
        fixes(
          Fix.Command(ShellLine.trusted(ShipAdvice.snapshotExport)),
          Fix.Steps(lines(ShipAdvice.versionsCommand, "Read the session line before publishing.")),
        ),
      )

  case Releases(detail: String)
      extends ShipDiagnostic(
        CheckId.trusted("ship.releases"),
        Level.Error,
        "releases was not read",
        detail,
        "A line catalog derives snapshots and releases from this file. An empty file means nothing in the catalog is released yet.",
        fixes(
          Fix.Command(ShellLine.trusted("touch releases")),
          Fix.Steps(
            lines(
              "Leave releases empty until a ship is released.",
              "Or add one line: histos matches, histos moved, or histos unknown.",
            )
          ),
        ),
      )

  case ShouldBump(ship: ShipName, number: ShipNumber, shadowed: Boolean)
      extends ShipDiagnostic(
        CheckId.trusted("ship.should-bump"),
        Level.Warning,
        s"${ship.value} ${number.render} should move",
        ShipAdvice.shouldBump(ship, number, shadowed),
        "The catalog stores the next release. After that number is on the registry, edit .next(...). Histos does not open a bump pull request.",
        fixes(
          Fix.Steps(
            lines(
              s"Edit Ship(\"${ship.value}\").next(...) in the catalog.",
              "Do not open a bump pull request.",
            )
          )
        ),
      )
end ShipDiagnostic
