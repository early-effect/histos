package histos.load

import histos.plugin.{
  BindingClass,
  CompilerConstraint,
  EntryName,
  StageAssurance,
  StageBinding,
  StageContract,
  StageContractError,
  StageInputs,
  StageRestrictions,
}

/** Host-owned stages. A new library is not admitted by package prefix. */
object KnownStageRegistry:
  val Adapter: Int = 1

  val ScalaQuotes: EntryName    = EntryName.trusted("scala.quoted.Quotes")
  val ZioTrace: EntryName       = EntryName.trusted("zio.internal.stacktracer.Macros")
  val IzumiTag: EntryName       = EntryName.trusted("izumi.reflect.TagMacro")
  val HistosLiterals: EntryName = EntryName.trusted("histos.Literals")

  def names: List[EntryName] = List(ScalaQuotes, ZioTrace, IzumiTag, HistosLiterals)

  def contract(name: EntryName): Option[StageContract] =
    if names.contains(name) then Some(known(name)) else None

  /** Structural checks, then offline admission, then host-known membership. */
  def admit(stages: List[StageContract], offline: Boolean): Either[PluginCompileError, Unit] =
    stages.foldLeft[Either[PluginCompileError, Unit]](Right(())):
      case (Left(error), _)  => Left(error)
      case (Right(_), stage) =>
        StageContract.validate(stage) match
          case Left(error) => Left(PluginCompileError.Stage(error))
          case Right(_)    =>
            stage.assurance match
              case StageAssurance.ProducerDeclared if offline =>
                Left(PluginCompileError.Stage(StageContractError.UnsupportedOffline))
              case StageAssurance.HostKnown =>
                stage.entrypoints.find(name => !names.contains(name)) match
                  case Some(name) => Left(PluginCompileError.UnknownStage(name))
                  case None       => Right(())
              case StageAssurance.ProducerDeclared => Right(())

  private def known(name: EntryName): StageContract =
    StageContract(
      StageAssurance.HostKnown,
      List(name),
      Nil,
      CompilerConstraint.unconstrained,
      Nil,
      StageInputs(Nil, Nil, Nil),
      StageRestrictions.v1,
    )

end KnownStageRegistry

enum PluginCompileError:
  case Stage(error: StageContractError)
  case UnknownStage(name: EntryName)
  case Compile(error: histos.zinc.CompileError)
  case Host(error: histos.toolchain.HostCompilerError)
  case Child(error: histos.process.ChildError)
  case Seal(error: histos.plugin.ArtifactManifestError)
  case Io(failure: histos.workspace.IoFailure)
  case InputChanged(path: histos.plugin.RepoPath)
  case Path(error: histos.plugin.RepoPathError)
  case Worker(exit: Int, detail: String)

  def message: String = this match
    case Stage(error)         => error.message
    case UnknownStage(name)   => s"${name.value} is not a host-known stage"
    case Compile(error)       => error.message
    case Host(error)          => error.message
    case Child(error)         => error.message
    case Seal(error)          => error.message
    case Io(failure)          => failure.message
    case InputChanged(path)   => s"${path.value} changed after it was recorded"
    case Path(error)          => error.message
    case Worker(exit, detail) => s"compiler worker exited $exit: $detail"
end PluginCompileError
