package histos.plugin

import histos.ScalaVersion

enum RecipeVersion:
  case V1

enum RecipeRole:
  case Root
  case Helper
  case Stage

enum SourceSet:
  case Main
  case Test

enum SourceKind:
  case Scala
  case Java
  case Resource

enum SourceProvenance:
  case Unmanaged
  case Generated

/** One selected input. An empty list is a recorded empty set, not a missing capture. */
final case class SelectedInput(
    path: RepoPath,
    kind: SourceKind,
    set: SourceSet,
    provenance: SourceProvenance,
    digest: ContentHash,
    length: Long,
)

/** One semantic compiler setting. Output paths and diagnostic paths are not settings. */
final case class SemanticSetting(key: InputKey, value: String)

enum ClosureRole:
  case Compile
  case CompileOnly
  case Provided
  case Runtime

enum HostRole:
  case Api
  case ScalaRuntime
  case Zio
  case Izumi

/** A host-owned coordinate substituted by role. The match is the full artifact, not a name prefix. */
final case class HostSubstitution(role: HostRole, artifact: ArtifactIdentity)

/** One closure member and the role it plays in a rebuild. Runtime is not compile-only. */
final case class ClosureArtifact(artifact: ArtifactIdentity, role: ClosureRole)

/** Producer compiler, Scala, runtime, and JDK. The JDK major is the release, not a machine path. */
final case class ProducerBinding(
    scala: ScalaVersion,
    jdkMajor: Int,
    compiler: List[ArtifactIdentity],
    runtime: List[ArtifactIdentity],
)

/** The recorded body of one node. `Unrecorded` is not an empty copy of this value. */
final case class RecordedInputs(
    sources: List[SelectedInput],
    settings: List[SemanticSetting],
    closure: List[ClosureArtifact],
    stages: List[ArtifactIdentity],
    host: List[HostSubstitution],
    producer: ProducerBinding,
)

/** `Unrecorded` is absence. It is not [[InputRecord.Frozen]] of an empty source list. */
enum InputRecord:
  case Unrecorded
  case Frozen(inputs: RecordedInputs)

final case class RecipeNode(
    id: RecipeNodeId,
    role: RecipeRole,
    prerequisites: List[RecipeNodeId],
    sources: InputRecord,
    stage: Option[StageContract],
)

/** A finite recipe. Construction validates the version, the graph, and the stage contracts. It does not upload. */
final case class RebuildRecipe private (version: RecipeVersion, root: RecipeNodeId, nodes: List[RecipeNode])

object RebuildRecipe:
  def version(raw: Int, node: RecipeNodeId): Either[RecipeError, RecipeVersion] =
    if raw == 1 then Right(RecipeVersion.V1)
    else Left(RecipeError.Unsupported(RecipeFeature("version"), node))

  /** Sources of a node. Unrecorded fails. A recorded empty source list is returned empty, not as unrecorded. */
  def sourcesOf(node: RecipeNodeId, record: InputRecord): Either[RecipeError, RecordedInputs] =
    record match
      case InputRecord.Unrecorded   => Left(RecipeError.Unrecorded(node))
      case InputRecord.Frozen(body) => Right(body)

  def from(
      version: RecipeVersion,
      root: RecipeNodeId,
      nodes: List[RecipeNode],
  ): Either[RecipeError, RebuildRecipe] =
    index(nodes).flatMap: byId =>
      if byId.contains(root) then
        dangling(nodes, byId) match
          case Some(missing) => Left(RecipeError.Incomplete(RecipeInput("prerequisite"), missing))
          case None          =>
            cycle(nodes, byId) match
              case Some(path) => Left(RecipeError.Cycle(path))
              case None       =>
                nodes
                  .foldLeft[Either[RecipeError, Unit]](Right(())): (acc, node) =>
                    acc.flatMap(_ => nodeError(node))
                  .map(_ => RebuildRecipe(version, root, nodes))
      else Left(RecipeError.Incomplete(RecipeInput("root"), root))

  private def index(nodes: List[RecipeNode]): Either[RecipeError, Map[RecipeNodeId, RecipeNode]] =
    nodes.foldLeft[Either[RecipeError, Map[RecipeNodeId, RecipeNode]]](Right(Map.empty)):
      case (Left(error), _)    => Left(error)
      case (Right(seen), node) =>
        if seen.contains(node.id) then Left(RecipeError.Duplicate(node.id))
        else Right(seen.updated(node.id, node))

  private def dangling(
      nodes: List[RecipeNode],
      byId: Map[RecipeNodeId, RecipeNode],
  ): Option[RecipeNodeId] =
    nodes.foldLeft[Option[RecipeNodeId]](None):
      case (found @ Some(_), _) => found
      case (None, node)         => node.prerequisites.find(next => !byId.contains(next))

  private def nodeError(node: RecipeNode): Either[RecipeError, Unit] =
    val recorded: Either[RecipeError, Unit] = node.sources match
      case InputRecord.Unrecorded   => Right(())
      case InputRecord.Frozen(body) =>
        if body.producer.jdkMajor <= 0 then Left(RecipeError.Incomplete(RecipeInput("jdk"), node.id))
        else Right(())
    recorded.flatMap: _ =>
      (node.role, node.stage) match
        case (RecipeRole.Stage, None) =>
          Left(RecipeError.Incomplete(RecipeInput("stage"), node.id))
        case (_, Some(contract)) =>
          StageContract.validate(contract).left.map(RecipeError.Stage(_)).map(_ => ())
        case _ => Right(())

  private def cycle(
      nodes: List[RecipeNode],
      byId: Map[RecipeNodeId, RecipeNode],
  ): Option[List[RecipeNodeId]] =
    def walk(
        id: RecipeNodeId,
        stack: List[RecipeNodeId],
        seen: Set[RecipeNodeId],
    ): Option[List[RecipeNodeId]] =
      if stack.contains(id) then Some((id :: stack).reverse)
      else if seen.contains(id) then None
      else
        byId.get(id) match
          case None       => None
          case Some(node) =>
            node.prerequisites.foldLeft[Option[List[RecipeNodeId]]](None): (found, next) =>
              found.orElse(walk(next, id :: stack, seen + id))
    nodes.foldLeft[Option[List[RecipeNodeId]]](None): (found, node) =>
      found.orElse(walk(node.id, Nil, Set.empty))
end RebuildRecipe

enum RecipeError:
  case Unsupported(feature: RecipeFeature, node: RecipeNodeId)
  case Incomplete(input: RecipeInput, node: RecipeNodeId)
  case Duplicate(id: RecipeNodeId)
  case Cycle(path: List[RecipeNodeId])
  case Unrecorded(node: RecipeNodeId)
  case Stage(error: StageContractError)
  case Seal(error: ArtifactManifestError)

  def message: String = this match
    case Unsupported(feature, node) =>
      s"recipe node ${node.value} does not support ${feature.value}"
    case Incomplete(input, node) =>
      s"recipe node ${node.value} is missing ${input.value}"
    case Duplicate(id) =>
      s"recipe node ${id.value} is declared twice"
    case Cycle(path) =>
      s"recipe cycle ${path.map(_.value).mkString(" -> ")}"
    case Unrecorded(node) =>
      s"recipe node ${node.value} has no compilation snapshot"
    case Stage(error) => error.message
    case Seal(error)  => error.message
end RecipeError
