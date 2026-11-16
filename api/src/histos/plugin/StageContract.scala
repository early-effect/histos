package histos.plugin

import histos.ScalaVersion

/** Who asserts that a compiler stage is determined by its declared inputs. */
enum StageAssurance:
  case HostKnown
  case ProducerDeclared

/** Public values are frozen into the recipe. Secret and unclassified values are not a v1 input. */
enum BindingClass:
  case Public
  case Secret
  case Unclassified

/** The only cwd v1 admits: the root of the declared relative inputs. */
enum StageCwd:
  case DeclaredRoot

enum StageNetwork:
  case Prohibited

enum StageClock:
  case Prohibited

enum StageRandom:
  case Prohibited

enum StageProcess:
  case Prohibited

/** Every restriction is named. There is no v1 case that permits network, time, randomness, or a subprocess. */
final case class StageRestrictions(
    cwd: StageCwd,
    network: StageNetwork,
    clock: StageClock,
    random: StageRandom,
    process: StageProcess,
)

object StageRestrictions:
  val v1: StageRestrictions = StageRestrictions(
    StageCwd.DeclaredRoot,
    StageNetwork.Prohibited,
    StageClock.Prohibited,
    StageRandom.Prohibited,
    StageProcess.Prohibited,
  )

final case class StageBinding(key: InputKey, classification: BindingClass)

/** One frozen file. The path is relative to the recipe root. */
final case class FrozenFile(path: RepoPath, digest: ContentHash, length: Long)

/** A directory input is the complete file list, not a path the consumer re-reads. */
final case class FrozenDirectory(path: RepoPath, files: List[FrozenFile])

/** Explicit absence is a value. A missing key is not absence. */
enum FrozenEnv:
  case Present(override val key: EnvName, value: String)
  case Absent(override val key: EnvName)

  def key: EnvName
end FrozenEnv

final case class StageInputs(
    files: List[FrozenFile],
    directories: List[FrozenDirectory],
    env: List[FrozenEnv],
)

final case class CompilerConstraint(scalaVersion: Option[ScalaVersion], minJdk: Option[Int])

object CompilerConstraint:
  val unconstrained: CompilerConstraint = CompilerConstraint(None, None)

/** Producer assertion, or a host-known stage. Not a proof from bytecode. */
final case class StageContract(
    assurance: StageAssurance,
    entrypoints: List[EntryName],
    helpers: List[RecipeNodeId],
    compiler: CompilerConstraint,
    required: List[StageBinding],
    inputs: StageInputs,
    restrictions: StageRestrictions,
)

object StageContract:
  /** Structural refusal. Offline admission is [[admitOffline]]. */
  def validate(contract: StageContract): Either[StageContractError, StageContract] =
    secret(contract) match
      case Some(error) => Left(error)
      case None        =>
        missing(contract) match
          case Some(key) => Left(StageContractError.MissingBinding(key))
          case None      =>
            contract.compiler.minJdk match
              case Some(major) if major <= 0 => Left(StageContractError.BadJdk(major))
              case _                         => Right(contract)

  /** Offline compilation admits host-known stages only. A declared stage is refused before a worker starts. */
  def admitOffline(contract: StageContract): Either[StageContractError, Unit] =
    contract.assurance match
      case StageAssurance.HostKnown        => Right(())
      case StageAssurance.ProducerDeclared => Left(StageContractError.UnsupportedOffline)

  private def secret(contract: StageContract): Option[StageContractError] =
    contract.required.collectFirst:
      case StageBinding(_, BindingClass.Secret)       => StageContractError.SecretInput
      case StageBinding(_, BindingClass.Unclassified) => StageContractError.UnclassifiedInput

  private def missing(contract: StageContract): Option[InputKey] =
    val present = contract.inputs.env.map(_.key.value).toSet ++
      contract.inputs.files.map(_.path.value) ++
      contract.inputs.directories.map(_.path.value)
    contract.required.collectFirst:
      case StageBinding(key, BindingClass.Public) if !present.contains(key.value) => key
end StageContract

enum StageContractError:
  case SecretInput
  case UnclassifiedInput
  case MissingBinding(key: InputKey)
  case UnsupportedOffline
  case BadJdk(major: Int)

  def message: String = this match
    case SecretInput         => "a stage binding is secret"
    case UnclassifiedInput   => "a stage binding is unclassified"
    case MissingBinding(key) => s"stage binding ${key.value} has no frozen value"
    case UnsupportedOffline  => "a producer-declared stage is not admitted offline"
    case BadJdk(major)       => s"$major is not a JDK major"
end StageContractError
