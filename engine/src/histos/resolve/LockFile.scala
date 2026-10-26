package histos.resolve

import histos.jdk.Sha256
import histos.workspace.{IoFailure, Workspace}
import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.{Aggregate, Dep, DepKind, Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import zio.{IO, ULayer, ZIO, ZLayer}

enum LockField:
  case Cell, Test, Plugin, Tool, Root, Resolver, Direct, Artifact, Source, Evict

object LockField:
  def from(raw: String): Option[LockField] =
    raw match
      case "tool"     => Some(LockField.Tool)
      case "root"     => Some(LockField.Root)
      case "resolver" => Some(LockField.Resolver)
      case "cell"     => Some(LockField.Cell)
      case "test"     => Some(LockField.Test)
      case "plugin"   => Some(LockField.Plugin)
      case "direct"   => Some(LockField.Direct)
      case "artifact" => Some(LockField.Artifact)
      case "source"   => Some(LockField.Source)
      case "evict"    => Some(LockField.Evict)
      case _          => None

  def token(field: LockField): String =
    field match
      case LockField.Tool     => "tool"
      case LockField.Root     => "root"
      case LockField.Resolver => "resolver"
      case LockField.Cell     => "cell"
      case LockField.Test     => "test"
      case LockField.Plugin   => "plugin"
      case LockField.Direct   => "direct"
      case LockField.Artifact => "artifact"
      case LockField.Source   => "source"
      case LockField.Evict    => "evict"
end LockField

enum LockError:
  case Empty
  case BadLine(line: String)
  case DuplicateCell(
      module: ProjectName,
      scalaVersion: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope,
  )
  case DuplicateTool(id: ToolId)
  case DuplicatePlugin(organization: String, name: String, revision: String)
  case Missing(field: LockField)
  case Classifier(error: ClassifierError)
  case BadSha256(raw: String)
  case BadRepository(raw: String)

  def message: String = this match
    case Empty                                           => "dependency lock is empty"
    case BadLine(line)                                   => s"dependency lock line is not a field: $line"
    case DuplicateCell(module, version, platform, scope) =>
      s"dependency lock repeats ${scope.header} ${module.value} ${version.value} ${PlatformToken.render(platform)}"
    case DuplicateTool(id)                             => s"dependency lock repeats tool ${id.token}"
    case DuplicatePlugin(organization, name, revision) =>
      s"dependency lock repeats plugin $organization $name $revision"
    case Missing(field) =>
      s"dependency lock is missing ${LockField.token(field)}"
    case Classifier(error)  => error.message
    case BadSha256(raw)     => s"dependency lock sha256 is not 64 hex digits: $raw"
    case BadRepository(raw) =>
      s"dependency lock repository is not central, ivy-local, or a maven url: $raw"
end LockError
