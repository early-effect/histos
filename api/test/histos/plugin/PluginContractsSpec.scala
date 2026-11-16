package histos.plugin

import histos.doctor.Noted
import histos.lock.{ExecutionKind, LockContext, LockDecision, LockStrategy}
import histos.{Platform, PluginPublication}

import java.nio.charset.StandardCharsets

import _root_.scala.compiletime.testing.typeChecks

import zio.{Ref, Scope, ZIO, ZLayer}
import zio.test.*

trait ReviewLabel:
  def value: String

final class LiveReviewLabel(val value: String) extends ReviewLabel

object ReviewLabel:
  def layer(value: String): ZLayer[Any, Nothing, ReviewLabel] =
    ZLayer.succeed[ReviewLabel](LiveReviewLabel(value))

final class ReviewTasks(label: ReviewLabel) extends HistosPlugin:
  val id = PluginId("reviewed-locks")

  def contribute: Contribution = Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput(label.value, Nil))
end ReviewTasks

object ReviewTasks:
  val layer: ZLayer[ReviewLabel, Nothing, ReviewTasks] =
    ZLayer.fromFunction(ReviewTasks(_))

enum LabelSetupError extends ServiceSetupError:
  case Empty

  def diagnosis: Noted = this match
    case Empty =>
      Noted.error(
        "review.config",
        "Review label is empty",
        "A nonempty label is required.",
        Noted.steps("Set the label in histos.scala."),
      )
end LabelSetupError

object CheckedReviewLabel:
  def layer(value: String): ZLayer[Any, LabelSetupError, ReviewLabel] =
    ZLayer.fromZIO:
      if value.isBlank then ZIO.fail(LabelSetupError.Empty)
      else ZIO.succeed[ReviewLabel](LiveReviewLabel(value))

object PluginContractsSpec extends ZIOSpecDefault:
  private inline val digest = "0000000000000000000000000000000000000000000000000000000000000000"
  private val node          = RecipeNodeId("root")

  private def exactNothing(plan: Plan[NoExtraServices, Nothing]): Plan[NoExtraServices, Nothing] = plan

  private def exactLabel(
      plan: Plan[NoExtraServices & ReviewLabel, LabelSetupError]
  ): Plan[NoExtraServices & ReviewLabel, LabelSetupError] = plan

  private def exactTasks(
      plan: Plan[NoExtraServices & ReviewLabel & ReviewTasks, Nothing]
  ): Plan[NoExtraServices & ReviewLabel & ReviewTasks, Nothing] = plan

  private val locks = new LockStrategy:
    def decide(context: LockContext): zio.IO[histos.lock.LockStrategyError, LockDecision] =
      ZIO.succeed:
        context.execution match
          case ExecutionKind.Local => LockDecision.OfferRepair
          case ExecutionKind.Ci    => LockDecision.Frozen

  private inline val withLabel =
    """
    import histos.plugin.*
    import zio.ZLayer
    trait ReviewLabel:
      def value: String
    final class LiveReviewLabel(val value: String) extends ReviewLabel
    object ReviewLabel:
      def layer(value: String): ZLayer[Any, Nothing, ReviewLabel] =
        ZLayer.succeed[ReviewLabel](LiveReviewLabel(value))
    final class Tasks(label: ReviewLabel) extends HistosPlugin:
      val id = PluginId("reviewed-locks")
      def contribute = Contribution(Nil, Nil, Nil)
      def run(request: RunRequest) = zio.ZIO.succeed(PluginOutput(label.value, Nil))
    object Tasks:
      val layer: ZLayer[ReviewLabel, Nothing, Tasks] = ZLayer.fromFunction(Tasks(_))
    val wired = BuildServices.default.add[ReviewLabel](ReviewLabel.layer("reviewed")).tasks[Tasks](Tasks.layer)
    """

  private inline val withoutLabel =
    """
    import histos.plugin.*
    import zio.ZLayer
    trait ReviewLabel:
      def value: String
    final class Tasks(label: ReviewLabel) extends HistosPlugin:
      val id = PluginId("reviewed-locks")
      def contribute = Contribution(Nil, Nil, Nil)
      def run(request: RunRequest) = zio.ZIO.succeed(PluginOutput(label.value, Nil))
    object Tasks:
      val layer: ZLayer[ReviewLabel, Nothing, Tasks] = ZLayer.fromFunction(Tasks(_))
    val missing = BuildServices.default.tasks[Tasks](Tasks.layer)
    """

  private inline val publication =
    """
    import histos.*
    val rebuildable = module("reviewed-locks", Jvm)(scala"3.10.1").pluginLibrary
    val binary = module("reviewed-locks", Jvm)(scala"3.10.1").pluginLibrary(PluginPublication.BinaryOnly)
    """

  def spec = suite("plugin contracts")(
    test("sha256 matches the published vectors") {
      val long = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
      assertTrue(
        Digests.sha256(Array.emptyByteArray) ==
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        Digests.sha256("abc".getBytes(StandardCharsets.UTF_8)) ==
          "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
        Digests.sha256(long.getBytes(StandardCharsets.UTF_8)) ==
          "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1",
      )
    },
    test("a repeated artifact path is rejected and an empty seal is not that rejection") {
      val entry    = ArtifactEntry(ArtifactPath("A.class"), 1L, ContentHash(digest))
      val again    = ArtifactEntry(ArtifactPath("A.class"), 2L, ContentHash(digest))
      val first    = ArtifactEntry(ArtifactPath("A.class"), 1L, ContentHash(digest))
      val later    = ArtifactEntry(ArtifactPath("B.class"), 1L, ContentHash(digest))
      val forward  = SealedArtifactManifest.from(List(first, later))
      val reversed = SealedArtifactManifest.from(List(later, first))
      assertTrue(
        SealedArtifactManifest.from(List(entry, again)) == Left(
          ArtifactManifestError.Duplicate(ArtifactPath("A.class"))
        ),
        SealedArtifactManifest.from(Nil).isRight,
        forward.map(_.identity) == reversed.map(_.identity),
      )
    },
    test("secret, unclassified, and missing public bindings are refused") {
      val secret = contract(BindingClass.Secret, present = true)
      val open   = contract(BindingClass.Unclassified, present = true)
      val absent = contract(BindingClass.Public, present = false)
      assertTrue(
        StageContract.validate(secret) == Left(StageContractError.SecretInput),
        StageContract.validate(open) == Left(StageContractError.UnclassifiedInput),
        StageContract.validate(absent) == Left(StageContractError.MissingBinding(InputKey("TOKEN"))),
        StageContract.validate(fileNamedToken) == Left(StageContractError.MissingBinding(InputKey("TOKEN"))),
        StageContract.admitOffline(secret) == Left(StageContractError.UnsupportedOffline),
        StageContract.admitOffline(secret.copy(assurance = StageAssurance.HostKnown)) == Right(()),
      )
    },
    test("an unrecorded recipe input is not an empty frozen list") {
      val unrecorded = RebuildRecipe.sourcesOf(node, InputRecord.Unrecorded)
      val emptyBody  = recorded()
      val empty      = RebuildRecipe.sourcesOf(node, InputRecord.Frozen(emptyBody))
      assertTrue(
        unrecorded == Left(RecipeError.Unrecorded(node)),
        empty == Right(emptyBody),
        emptyBody.sources.isEmpty,
        unrecorded != empty,
      )
    },
    test("a duplicate id and a missing prerequisite are refused") {
      val root     = RecipeNode(node, RecipeRole.Root, Nil, InputRecord.Frozen(recorded()), None)
      val again    = root
      val dangling = RecipeNode(
        node,
        RecipeRole.Root,
        List(RecipeNodeId("missing")),
        InputRecord.Frozen(recorded()),
        None,
      )
      val absent = RecipeNode(node, RecipeRole.Root, Nil, InputRecord.Unrecorded, None)
      val badJdk = root.copy(sources =
        InputRecord.Frozen(recorded().copy(producer = ProducerBinding(histos.ScalaVersion("3.10.1"), 0, Nil, Nil)))
      )
      assertTrue(
        RebuildRecipe.from(RecipeVersion.V1, node, List(root, again)) ==
          Left(RecipeError.Duplicate(node)),
        RebuildRecipe.from(RecipeVersion.V1, node, List(dangling)) ==
          Left(RecipeError.Incomplete(RecipeInput("prerequisite"), RecipeNodeId("missing"))),
        RebuildRecipe.from(RecipeVersion.V1, node, List(absent)).isRight,
        RebuildRecipe.from(RecipeVersion.V1, node, List(badJdk)) ==
          Left(RecipeError.Incomplete(RecipeInput("jdk"), node)),
      )
    },
    test("the builder infers the setup error and a missing service does not typecheck") {
      val checked = exactLabel(BuildServices.default.add[ReviewLabel](CheckedReviewLabel.layer("reviewed")))
      val tasks   = exactTasks(
        BuildServices.default.add[ReviewLabel](ReviewLabel.layer("reviewed")).tasks[ReviewTasks](ReviewTasks.layer)
      )
      val policy = exactNothing(BuildServices.default.withLocks(Reviewed.layer))
      assertTrue(
        (exactLabel(checked) eq checked),
        (exactTasks(tasks) eq tasks),
        (exactNothing(policy) eq policy),
        typeChecks(withLabel),
        !typeChecks(withoutLabel),
        typeChecks(publication),
      )
    },
    test("a duplicate service is refused before its layer runs") {
      ZIO.scoped:
        for
          scope   <- ZIO.service[Scope]
          counter <- Ref.make(0)
          layer = ZLayer.fromZIO:
            counter.update(_ + 1) *> ZIO.succeed[ReviewLabel](LiveReviewLabel("reviewed"))
          plan = BuildServices.default.add[ReviewLabel](layer).add[ReviewLabel](layer)
          exit <- plan.activate
            .provide(
              ZLayer.succeed[BuildContext](Ambient.context),
              ZLayer.succeed[PluginEvents.Generation](Ambient.events),
              ZLayer.succeed[LockStrategy](locks),
              PluginLoading.exactOrRebuild,
              ZLayer.succeed(scope),
            )
            .exit
          seen <- counter.get
        yield assertTrue(exit.isFailure, seen == 0)
    },
    test("omitted locks keep the core strategy") {
      ZIO.scoped:
        for
          scope <- ZIO.service[Scope]
          built <- BuildServices.default.activate.provide(
            ZLayer.succeed[BuildContext](Ambient.context),
            ZLayer.succeed[PluginEvents.Generation](Ambient.events),
            ZLayer.succeed[LockStrategy](locks),
            PluginLoading.exactOrRebuild,
            ZLayer.succeed(scope),
          )
        yield assertTrue(built.locks eq locks)
    },
    test("withLocks replaces the core strategy") {
      val replacement = new LockStrategy:
        def decide(context: LockContext): zio.IO[histos.lock.LockStrategyError, LockDecision] =
          ZIO.succeed(LockDecision.Update)
      ZIO.scoped:
        for
          scope <- ZIO.service[Scope]
          built <- BuildServices.default
            .withLocks(ZLayer.succeed[LockStrategy](replacement))
            .activate
            .provide(
              ZLayer.succeed[BuildContext](Ambient.context),
              ZLayer.succeed[PluginEvents.Generation](Ambient.events),
              ZLayer.succeed[LockStrategy](locks),
              PluginLoading.exactOrRebuild,
              ZLayer.succeed(scope),
            )
        yield assertTrue((built.locks eq replacement), !(built.locks eq locks))
    },
    test("adding the core lock slot is refused before that layer runs") {
      ZIO.scoped:
        for
          scope   <- ZIO.service[Scope]
          counter <- Ref.make(0)
          layer = ZLayer.fromZIO:
            counter.update(_ + 1) *> ZIO.succeed[LockStrategy](locks)
          exit <- BuildServices.default
            .add[LockStrategy](layer)
            .activate
            .provide(
              ZLayer.succeed[BuildContext](Ambient.context),
              ZLayer.succeed[PluginEvents.Generation](Ambient.events),
              ZLayer.succeed[LockStrategy](locks),
              PluginLoading.exactOrRebuild,
              ZLayer.succeed(scope),
            )
            .exit
          seen <- counter.get
          refused = exit.causeOption.flatMap(_.failureOption) match
            case Some(ServiceActivationError.DuplicateReplacement(CoreSlot.Locks)) => true
            case _                                                                 => false
        yield assertTrue(refused, seen == 0)
    },
  )

  private def contract(classification: BindingClass, present: Boolean): StageContract =
    val env =
      if present then List(FrozenEnv.Present(EnvName("TOKEN"), "a"))
      else Nil
    StageContract(
      StageAssurance.ProducerDeclared,
      List(EntryName("stage.Main")),
      Nil,
      CompilerConstraint.unconstrained,
      List(StageBinding(InputKey("TOKEN"), classification, BindingKind.Env)),
      StageInputs(Nil, Nil, env),
      StageRestrictions.v1,
    )

  private def fileNamedToken: StageContract =
    contract(BindingClass.Public, present = false).copy(
      inputs = StageInputs(
        List(FrozenFile(RepoPath("TOKEN"), ContentHash(digest), 1L)),
        Nil,
        Nil,
      )
    )

  private def recorded(): RecordedInputs =
    RecordedInputs(
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      ProducerBinding(histos.ScalaVersion("3.10.1"), 21, Nil, Nil),
    )

  private object Reviewed:
    val layer: ZLayer[Any, Nothing, LockStrategy] =
      ZLayer.succeed[LockStrategy](locks)

  private object Ambient:
    val context: BuildContext = new BuildContext:
      def generation: GenerationId  = GenerationId(digest)
      def repository: String        = "repo"
      def declarations: ContentHash = ContentHash(digest)
      def host: ApiDigest           = ApiDigest(digest)

    val events: PluginEvents.Generation = new PluginEvents.Generation:
      def emit(event: GenerationPluginEvent): zio.IO[PluginEventError, Unit] = ZIO.unit
  end Ambient
end PluginContractsSpec
