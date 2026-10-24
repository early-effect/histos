package histos.toolchain

import histos.jdk.Os

import scala.quoted.*

enum Architecture:
  case X86_64, Aarch64
  case Unsupported(raw: String)

  def label: String = this match
    case X86_64           => "x86_64"
    case Aarch64          => "aarch64"
    case Unsupported(raw) => raw

object Architecture:
  def fromMachine(raw: String): Architecture =
    raw.strip().toLowerCase(java.util.Locale.ROOT) match
      case "x86_64" | "amd64"  => Architecture.X86_64
      case "aarch64" | "arm64" => Architecture.Aarch64
      case _                   => Architecture.Unsupported(raw)

enum HostPlatform:
  case MacosX86_64, MacosAarch64, LinuxX86_64, LinuxAarch64, WindowsX86_64,
    WindowsAarch64
  case Unsupported(os: Os, architecture: Architecture)

  def token: Option[String] = this match
    case MacosX86_64       => Some("macos-x86_64")
    case MacosAarch64      => Some("macos-aarch64")
    case LinuxX86_64       => Some("linux-x86_64")
    case LinuxAarch64      => Some("linux-aarch64")
    case WindowsX86_64     => Some("windows-x86_64")
    case WindowsAarch64    => Some("windows-aarch64")
    case Unsupported(_, _) => None

  def description: String = this match
    case Unsupported(os, architecture) => s"$os/${architecture.label}"
    case supported => supported.token.getOrElse("unsupported")

object HostPlatform:
  inline def apply(inline raw: String): HostPlatform = ${
    HostPlatformLiteral.literal('raw)
  }

  def from(raw: String): Either[HostPlatformError, HostPlatform] = raw match
    case "macos-x86_64"    => Right(HostPlatform.MacosX86_64)
    case "macos-aarch64"   => Right(HostPlatform.MacosAarch64)
    case "linux-x86_64"    => Right(HostPlatform.LinuxX86_64)
    case "linux-aarch64"   => Right(HostPlatform.LinuxAarch64)
    case "windows-x86_64"  => Right(HostPlatform.WindowsX86_64)
    case "windows-aarch64" => Right(HostPlatform.WindowsAarch64)
    case _                 => Left(HostPlatformError.Unknown(raw))

  def of(os: Os, architecture: Architecture): HostPlatform =
    (os, architecture) match
      case (Os.Mac, Architecture.X86_64)      => HostPlatform.MacosX86_64
      case (Os.Mac, Architecture.Aarch64)     => HostPlatform.MacosAarch64
      case (Os.Linux, Architecture.X86_64)    => HostPlatform.LinuxX86_64
      case (Os.Linux, Architecture.Aarch64)   => HostPlatform.LinuxAarch64
      case (Os.Windows, Architecture.X86_64)  => HostPlatform.WindowsX86_64
      case (Os.Windows, Architecture.Aarch64) => HostPlatform.WindowsAarch64
      case _ => HostPlatform.Unsupported(os, architecture)

enum HostPlatformError:
  case Unknown(raw: String)
  def message: String = this match
    case Unknown(raw) => s"unknown canonical runtime host: $raw"

private object HostPlatformLiteral:
  def literal(raw: Expr[String])(using Quotes): Expr[HostPlatform] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "HostPlatform takes a literal. Use HostPlatform.from at runtime.",
          raw
        )
      case Some(value) =>
        HostPlatform.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(HostPlatform.MacosX86_64)  => '{ HostPlatform.MacosX86_64 }
          case Right(HostPlatform.MacosAarch64) =>
            '{ HostPlatform.MacosAarch64 }
          case Right(HostPlatform.LinuxX86_64)  => '{ HostPlatform.LinuxX86_64 }
          case Right(HostPlatform.LinuxAarch64) =>
            '{ HostPlatform.LinuxAarch64 }
          case Right(HostPlatform.WindowsX86_64) =>
            '{ HostPlatform.WindowsX86_64 }
          case Right(HostPlatform.WindowsAarch64) =>
            '{ HostPlatform.WindowsAarch64 }
          case Right(HostPlatform.Unsupported(_, _)) =>
            quotes.reflect.report.errorAndAbort("unsupported literal host", raw)
