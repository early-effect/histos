package histos.version

import histos.doctor.ShellLine

/** A `histos.version` pin that a published Histos release has passed. Each case names the command that moves it. */
enum PinLag:
  /** A release pin, and a newer release is published. */
  case NewerRelease(pin: ShipNumber, published: ShipNumber)

  /** A snapshot pin, and its line or a later one is released. `0.2.1-SNAPSHOT` trails `0.2.1`. */
  case LineReleased(line: ShipNumber, published: ShipNumber)

  def command: ShellLine = this match
    case NewerRelease(_, _) => ShellLine.trusted("histos update")
    case LineReleased(_, _) => ShellLine.trusted("histos update --stable")

  def text: String = this match
    case NewerRelease(pin, published) =>
      s"histos ${published.render} is released and histos.version pins ${pin.render}. Run `${command.value}`."
    case LineReleased(line, published) =>
      s"histos ${published.render} is released and histos.version follows ${line.render} snapshots. Run `${command.value}`."
end PinLag
