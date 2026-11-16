package histos.load

import histos.jdk.{JdkVersion, Sha256, Sha256Digest}
import histos.plugin.{
  ArtifactIdentity,
  CompilerConstraint,
  ContentHash,
  FrozenDirectory,
  FrozenEnv,
  FrozenFile,
  SelectedInput,
  SemanticSetting,
  StageBinding,
  StageContract,
  StageRestrictions,
}
import histos.ScalaVersion

import java.nio.charset.StandardCharsets

import zio.Chunk

/** Versioned compile identity. Absolute checkout and temp paths are not inputs. */
object PluginCompileKey:
  val Format: Int = 3

  val Bootstrap: String = "HISTOS_WORKER=1"

  def of(
      version: ScalaVersion,
      jdk: JdkVersion,
      flags: String,
      sources: List[SelectedInput],
      settings: List[SemanticSetting],
      stages: List[StageContract],
      helpers: List[ArtifactIdentity],
      classpath: List[Sha256],
      api: List[Sha256],
      compiler: List[ContentHash],
      fingerprint: ContentHash,
  ): Sha256 =
    val lines =
      List(
        s"plugin-compile/$Format",
        s"scala ${version.value}",
        s"jdk ${jdk.value}",
        s"flags $flags",
        s"bootstrap $Bootstrap",
        s"compiler-fingerprint ${fingerprint.value}",
      ) ++ sources.map(sourceLine) ++
        settings.map(setting => s"setting ${setting.key.value}=${setting.value}") ++
        stages.map(stageLine) ++
        helpers.map(identity => s"helper ${identity.digest.value}") ++
        classpath.map(hash => s"classpath ${hash.value}") ++
        compiler.map(hash => s"tool ${hash.value}") ++
        apiLines(api)
    Sha256Digest.of(Chunk.fromArray(lines.mkString("\n").getBytes(StandardCharsets.UTF_8)))

  private def apiLines(api: List[Sha256]): List[String] =
    api.map(hash => s"api ${hash.value}")

  private def sourceLine(source: SelectedInput): String =
    s"source ${source.path.value} ${source.kind} ${source.digest.value} ${source.length}"

  private def stageLine(stage: StageContract): String =
    val entries     = stage.entrypoints.map(_.value).mkString(",")
    val helpers     = stage.helpers.map(_.value).mkString(",")
    val required    = stage.required.map(bindingLine).mkString(",")
    val files       = stage.inputs.files.map(fileLine).mkString(",")
    val directories = stage.inputs.directories.map(directoryLine).mkString(",")
    val env         = stage.inputs.env.map(envLine).mkString(",")
    s"stage ${stage.assurance} entries[$entries] helpers[$helpers] ${compilerLine(stage.compiler)} required[$required] files[$files] dirs[$directories] env[$env] ${restrictionLine(stage.restrictions)}"

  private def compilerLine(constraint: CompilerConstraint): String =
    val scala = constraint.scalaVersion match
      case Some(version) => version.value
      case None          => "any"
    val jdk = constraint.minJdk match
      case Some(major) => major.toString
      case None        => "any"
    s"compiler[$scala,$jdk]"

  private def bindingLine(binding: StageBinding): String =
    s"${binding.kind}:${binding.classification}:${binding.key.value}"

  private def fileLine(file: FrozenFile): String =
    s"${file.path.value}@${file.digest.value}:${file.length}"

  private def directoryLine(directory: FrozenDirectory): String =
    val files = directory.files.map(fileLine).mkString(",")
    s"${directory.path.value}[$files]"

  private def restrictionLine(restrictions: StageRestrictions): String =
    s"restrict[${restrictions.cwd},${restrictions.network},${restrictions.clock},${restrictions.random},${restrictions.process}]"

  private def envLine(env: FrozenEnv): String =
    env match
      case FrozenEnv.Present(key, value) => s"${key.value}=$value"
      case FrozenEnv.Absent(key)         => s"${key.value}<absent>"
end PluginCompileKey
