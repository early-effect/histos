package histos.plugin

enum RecipeVersion:
  case V1

enum RecipeRole:
  case Root
  case Helper
  case Stage

/** One frozen source or resource selected for a node. An empty list is a recorded empty set, not a missing capture. */
final case class FrozenInput(path: RepoPath, digest: ContentHash, length: Long)

/** `Unrecorded` is absence. It is not [[Frozen]] of an empty list. */
enum InputRecord:
  case Unrecorded
  case Frozen(sources: List[FrozenInput])

final case class RecipeNode(
    id: RecipeNodeId,
    role: RecipeRole,
    prerequisites: List[RecipeNodeId],
    sources: InputRecord,
    stage: Option[StageContract],
)

/** A finite recipe. Construction validates the version, the capture, and the stage contracts. It does not upload. */
final case class RebuildRecipe private (version: RecipeVersion, root: RecipeNodeId, nodes: List[RecipeNode])

object RebuildRecipe:
  def version(raw: Int, node: RecipeNodeId): Either[RecipeError, RecipeVersion] =
    if raw == 1 then Right(RecipeVersion.V1)
    else Left(RecipeError.Unsupported(RecipeFeature("version"), node))

  /** Sources of a node. Unrecorded fails. A recorded empty list is returned as empty, not as unrecorded. */
  def sourcesOf(node: RecipeNodeId, record: InputRecord): Either[RecipeError, List[FrozenInput]] =
    record match
      case InputRecord.Unrecorded     => Left(RecipeError.Unrecorded(node))
      case InputRecord.Frozen(inputs) => Right(inputs)

  def from(
      version: RecipeVersion,
      root: RecipeNodeId,
      nodes: List[RecipeNode],
  ): Either[RecipeError, RebuildRecipe] =
    nodes.find(_.id == root) match
      case None    => Left(RecipeError.Incomplete(RecipeInput("root"), root))
      case Some(_) =>
        cycle(nodes) match
          case Some(path) => Left(RecipeError.Cycle(path))
          case None       =>
            nodes
              .foldLeft[Either[RecipeError, Unit]](Right(())): (acc, node) =>
                acc.flatMap(_ => nodeError(node))
              .map(_ => RebuildRecipe(version, root, nodes))

  private def nodeError(node: RecipeNode): Either[RecipeError, Unit] =
    sourcesOf(node.id, node.sources).flatMap: _ =>
      (node.role, node.stage) match
        case (RecipeRole.Stage, None) =>
          Left(RecipeError.Incomplete(RecipeInput("stage"), node.id))
        case (_, Some(contract)) =>
          StageContract.validate(contract).left.map(RecipeError.Stage(_)).map(_ => ())
        case _ => Right(())

  private def cycle(nodes: List[RecipeNode]): Option[List[RecipeNodeId]] =
    val byId = nodes.map(node => node.id -> node).toMap
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
  case Cycle(path: List[RecipeNodeId])
  case Unrecorded(node: RecipeNodeId)
  case Stage(error: StageContractError)
  case Seal(error: ArtifactManifestError)

  def message: String = this match
    case Unsupported(feature, node) =>
      s"recipe node ${node.value} does not support ${feature.value}"
    case Incomplete(input, node) =>
      s"recipe node ${node.value} is missing ${input.value}"
    case Cycle(path) =>
      s"recipe cycle ${path.map(_.value).mkString(" -> ")}"
    case Unrecorded(node) =>
      s"recipe node ${node.value} has no compilation snapshot"
    case Stage(error) => error.message
    case Seal(error)  => error.message
end RecipeError
