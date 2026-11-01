package histos.zinc

import histos.{Platform, ProjectName, ScalaVersion}

/** One named compile input. The token is what a miss prints. */
enum CompileInput:
  case Scala, Platform, Jdk, Runtime, Flags, Sources, Artifacts, Apis, Tools, Compiler

  def token: String = this match
    case Scala     => "scala"
    case Platform  => "platform"
    case Jdk       => "jdk"
    case Runtime   => "runtime"
    case Flags     => "flags"
    case Sources   => "sources"
    case Artifacts => "artifacts"
    case Apis      => "apis"
    case Tools     => "tools"
    case Compiler  => "compiler"
end CompileInput

object CompileInput:
  def from(raw: String): Option[CompileInput] =
    raw match
      case "scala"     => Some(CompileInput.Scala)
      case "platform"  => Some(CompileInput.Platform)
      case "jdk"       => Some(CompileInput.Jdk)
      case "runtime"   => Some(CompileInput.Runtime)
      case "flags"     => Some(CompileInput.Flags)
      case "sources"   => Some(CompileInput.Sources)
      case "artifacts" => Some(CompileInput.Artifacts)
      case "apis"      => Some(CompileInput.Apis)
      case "tools"     => Some(CompileInput.Tools)
      case "compiler"  => Some(CompileInput.Compiler)
      case _           => None

/** Why this compile key was not stored. */
enum KeyMiss:
  case NoPrior
  case Changed(names: List[CompileInput])

object KeyMiss:
  def line(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      change: KeyMiss,
  ): String =
    val id = s"${module.value} ${version.value} ${platform.token}"
    change match
      case KeyMiss.NoPrior        => s"$id no prior key"
      case KeyMiss.Changed(Nil)   => id
      case KeyMiss.Changed(names) => s"$id ${names.map(_.token).mkString(" ")}"
