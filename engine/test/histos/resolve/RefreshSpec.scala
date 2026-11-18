package histos.resolve

import histos.lock.{
  ExecutionKind,
  LockDecision,
  LockMode,
  LockReply,
  LockStrategy,
  LockStrategyError,
  LockSubject,
  LockTransactionError,
}
import histos.plugin.{
  ClientId,
  CommandIdentity,
  ContentHash,
  Digests,
  Interaction,
  InvocationContext,
  InvocationId,
  OfflineSelection,
  PluginCoordinate,
  PluginDetail,
  PluginEvents,
  ResolutionIntent,
}
import histos.workspace.Workspace
import histos.ProjectName

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Exit, IO, Ref, Scope, UIO, ZIO}
import zio.test.*

object RefreshSpec extends ZIOSpecDefault:
  private val utf8  = StandardCharsets.UTF_8
  private val app   = PluginCoordinate.Module(ProjectName("app"))
  private val shots = LocatorName("shots")
  private val tools = LocatorName("tools")

  def spec = suite("snapshot refresh")(
    test("no selector revalidates every retained locator") {
      ZIO.scoped:
        temp.flatMap: root =>
          val first  = bytes("snap-1")
          val second = bytes("tools-1")
          val nextA  = bytes("snap-2")
          val nextB  = bytes("tools-2")
          val before = document(hashBytes(first), hashBytes(second))
          val after  = document(hashBytes(nextA), hashBytes(nextB))
          for
            _       <- writeBytes(root.resolve(shots.value), nextA)
            _       <- writeBytes(root.resolve(tools.value), nextB)
            harness <- harness(root, before)
            outcome <- harness.coordinator.prepare(request("all", Nil, LockMode.Update))
            text    <- lockText(root)
            keptA   <- readBytes(root.resolve(shots.value))
            keptB   <- readBytes(root.resolve(tools.value))
            renames <- harness.renames.get
          yield assertTrue(
            committed(outcome),
            text == LockDocument.render(after),
            keptA.toList == nextA.toList,
            keptB.toList == nextB.toList,
            renames == 1,
          )
    },
    test("one selector leaves the other locator's bytes and lock record unchanged") {
      ZIO.scoped:
        temp.flatMap: root =>
          val first  = bytes("snap-1")
          val other  = bytes("tools-1")
          val next   = bytes("snap-2")
          val before = document(hashBytes(first), hashBytes(other))
          val after  = document(hashBytes(next), hashBytes(other))
          for
            _       <- writeBytes(root.resolve(shots.value), next)
            _       <- writeBytes(root.resolve(tools.value), other)
            harness <- harness(root, before)
            outcome <- harness.coordinator.prepare(request("one", List(ProjectName("shots")), LockMode.Update))
            text    <- lockText(root)
            kept    <- readBytes(root.resolve(tools.value))
            shot    <- readBytes(root.resolve(shots.value))
          yield assertTrue(
            committed(outcome),
            text == LockDocument.render(after),
            kept.toList == other.toList,
            shot.toList == next.toList,
          )
    },
    test("an unknown selector is a lock transaction error and writes nothing") {
      ZIO.scoped:
        temp.flatMap: root =>
          val first    = bytes("snap-1")
          val before   = single(hashBytes(first))
          val rendered = LockDocument.render(before)
          for
            _       <- writeBytes(root.resolve(shots.value), bytes("snap-2"))
            harness <- harness(root, before)
            exit <- harness.coordinator.prepare(request("missing", List(ProjectName("missing")), LockMode.Update)).exit
            text <- lockText(root)
            renames <- harness.renames.get
          yield assertTrue(
            failure(exit) == Some(
              LockRepairError.Transaction(LockTransactionError.UnknownSelector(ProjectName("missing")))
            ),
            text == rendered,
            renames == 0,
          )
    },
    test("unchanged bytes do not write the lock") {
      ZIO.scoped:
        temp.flatMap: root =>
          val first    = bytes("snap-1")
          val before   = single(hashBytes(first))
          val rendered = LockDocument.render(before)
          for
            _       <- writeBytes(root.resolve(shots.value), first)
            harness <- harness(root, before)
            offered <- harness.coordinator.prepare(request("same-offer", Nil, LockMode.Offer))
            updated <- harness.coordinator.prepare(request("same-update", Nil, LockMode.Update))
            text    <- lockText(root)
            renames <- harness.renames.get
          yield assertTrue(
            selected(offered),
            selected(updated),
            text == rendered,
            renames == 0,
          )
    },
    test("changed bytes with offer wait for accept") {
      ZIO.scoped:
        temp.flatMap: root =>
          val first    = bytes("snap-1")
          val next     = bytes("snap-2")
          val before   = single(hashBytes(first))
          val rendered = LockDocument.render(before)
          for
            _       <- writeBytes(root.resolve(shots.value), next)
            harness <- harness(root, before)
            outcome <- harness.coordinator.prepare(request("offer", Nil, LockMode.Offer))
            held    <- lockText(root)
            quiet   <- harness.renames.get
            checked <- accept(harness.coordinator, outcome, root)
            renames <- harness.renames.get
          yield checked && assertTrue(held == rendered, quiet == 0, renames == 1)
    },
    test("changed bytes with update commit only that locator") {
      ZIO.scoped:
        temp.flatMap: root =>
          val first  = bytes("snap-1")
          val other  = bytes("tools-1")
          val next   = bytes("snap-2")
          val before = document(hashBytes(first), hashBytes(other))
          val after  = document(hashBytes(next), hashBytes(other))
          for
            _       <- writeBytes(root.resolve(shots.value), next)
            _       <- writeBytes(root.resolve(tools.value), other)
            harness <- harness(root, before)
            outcome <- harness.coordinator.prepare(request("update", List(ProjectName("shots")), LockMode.Update))
            text    <- lockText(root)
            renames <- harness.renames.get
          yield assertTrue(committed(outcome), text == LockDocument.render(after), renames == 1)
    },
    test("a dependency lock is corrupt and the file is unchanged") {
      ZIO.scoped:
        temp.flatMap: root =>
          val cell = "cell app 3.8.1 jvm\nartifact com.example demo 1.0.0\n"
          for
            renames     <- Ref.make(0)
            _           <- writeText(root.resolve("dependencies.lock"), cell)
            coordinator <- open(root, renames)
            exit        <- coordinator
              .prepare(request("cell", Nil, LockMode.Update))
              .exit
            after <- readText(root.resolve("dependencies.lock"))
            count <- renames.get
          yield
            val corrupt = failure(exit) match
              case Some(LockRepairError.Store(LockStoreFault.Corrupt(detail))) =>
                detail.value == "line 1 is not a lock record" &&
                detail.value != LockStoreFault.Corrupt(detail).toString
              case Some(_) => false
              case None    => false
            assertTrue(corrupt, after == cell, count == 0)
    },
    test("refresh false does not replace a retained locator") {
      ZIO.scoped:
        temp.flatMap: root =>
          val first    = bytes("snap-1")
          val next     = bytes("snap-2")
          val before   = single(hashBytes(first))
          val rendered = LockDocument.render(before)
          val refused  = Solved(List(app), Nil, List(shots), List(SnapshotLocator(shots, hashBytes(next))))
          for
            _       <- writeBytes(root.resolve(shots.value), next)
            harness <- harness(root, before)
            outcome <- harness.coordinator.prepare(request("keep", Nil, LockMode.Update, refresh = false))
            text    <- lockText(root)
            renames <- harness.renames.get
          yield assertTrue(
            LockDocument.merge(before, refused, refresh = false) == Left(MergeFault.RefreshRequired),
            selected(outcome),
            text == rendered,
            renames == 0,
          )
    },
    test("a missing selected locator is a solve fault and writes nothing") {
      ZIO.scoped:
        temp.flatMap: root =>
          val before   = single(hashBytes(bytes("snap-1")))
          val rendered = LockDocument.render(before)
          for
            harness <- harness(root, before)
            exit    <- harness.coordinator.prepare(request("absent-file", Nil, LockMode.Update)).exit
            text    <- lockText(root)
            renames <- harness.renames.get
          yield assertTrue(
            failure(exit) == Some(LockRepairError.Solve(PluginDetail("a retained snapshot is not a file"))),
            text == rendered,
            renames == 0,
          )
    },
  )

  private def accept(coordinator: LockCoordinator, outcome: PrepareOutcome, root: Path): UIO[TestResult] =
    outcome match
      case PrepareOutcome.Offered(offer) =>
        for
          _ <- coordinator
            .accept(LockReply.Accept(offer.proposal, offer.base))
            .foldZIO(error => ZIO.dieMessage(error.message), ZIO.succeed(_))
          text <- lockText(root)
          digest = hashBytes(bytes("snap-2"))
        yield assertTrue(
          offer.proposal.value.nonEmpty,
          text == LockDocument.render(single(digest)),
        )
      case PrepareOutcome.Committed(_, _) | PrepareOutcome.Blocked(_) | PrepareOutcome.Selected(_) =>
        ZIO.succeed(assertTrue(false))

  private def committed(outcome: PrepareOutcome): Boolean =
    outcome match
      case PrepareOutcome.Committed(_, _)                                                     => true
      case PrepareOutcome.Offered(_) | PrepareOutcome.Blocked(_) | PrepareOutcome.Selected(_) => false

  private def selected(outcome: PrepareOutcome): Boolean =
    outcome match
      case PrepareOutcome.Selected(_)                                                             => true
      case PrepareOutcome.Offered(_) | PrepareOutcome.Committed(_, _) | PrepareOutcome.Blocked(_) => false

  private def request(
      token: String,
      selectors: List[ProjectName],
      mode: LockMode,
      refresh: Boolean = true,
  ): PrepareRequest =
    PrepareRequest(
      InvocationContext(
        InvocationId.derive(token),
        ClientId("cli"),
        CommandIdentity.Refresh(selectors),
        ExecutionKind.Local,
        Interaction.Offers,
        OfflineSelection.Online,
      ),
      ResolutionIntent.Inherit,
      PluginEvents.closedInvocation,
      Some(mode),
      refresh,
    )

  private def document(shotsDigest: ContentHash, toolsDigest: ContentHash): LockDocument =
    LockDocument(
      List(VersionedPin(app, LockSubject.Artifact, hashBytes(bytes("app-1")), None)),
      List(SnapshotLocator(shots, shotsDigest), SnapshotLocator(tools, toolsDigest)),
    )

  private def single(digest: ContentHash): LockDocument =
    LockDocument(
      List(VersionedPin(app, LockSubject.Artifact, hashBytes(bytes("app-1")), None)),
      List(SnapshotLocator(shots, digest)),
    )

  private final case class Open(coordinator: LockCoordinator, renames: Ref[Int])

  private def harness(root: Path, document: LockDocument): UIO[Open] =
    for
      renames <- Ref.make(0)
      _       <- writeText(root.resolve("dependencies.lock"), LockDocument.render(document))
      found   <- open(root, renames)
    yield Open(found, renames)

  private def open(root: Path, renames: Ref[Int]): UIO[LockCoordinator] =
    val strategy = new LockStrategy:
      def decide(context: histos.lock.LockContext): IO[LockStrategyError, LockDecision] =
        ZIO.fail(LockStrategyError.Policy(PluginDetail("strategy was called")))
    val command = new SuspendedCommand:
      def run(snapshot: ResolutionSnapshot) = ZIO.unit
    for
      store       <- LockStore.open(root, (temp, target) => renames.update(_ + 1) *> AtomicReplace.rename(temp, target))
      generations <- GenerationOwner.memory(root)
      coordinator <- LockCoordinator.open(DocumentPreparer(root), strategy, store, generations, command)
    yield coordinator

  private def hashBytes(payload: Array[Byte]): ContentHash =
    Digests.content(payload)

  private def bytes(text: String): Array[Byte] =
    text.getBytes(utf8)

  private def writeBytes(path: Path, payload: Array[Byte]): UIO[Unit] =
    ZIO.attemptBlocking(Files.write(path, payload)).orDie.unit

  private def readBytes(path: Path): UIO[Array[Byte]] =
    ZIO.attemptBlocking(Files.readAllBytes(path)).orDie

  private def writeText(path: Path, text: String): UIO[Unit] =
    AtomicReplace.write(path, text).foldZIO(fault => ZIO.dieMessage(fault.toString), ZIO.succeed(_))

  private def readText(path: Path): UIO[String] =
    ZIO.attemptBlocking(Files.readString(path, utf8)).orDie

  private def lockText(root: Path): UIO[String] =
    readText(root.resolve("dependencies.lock"))

  private def failure[A](exit: Exit[LockRepairError, A]): Option[LockRepairError] =
    exit match
      case Exit.Success(_)     => None
      case Exit.Failure(cause) => cause.failureOption

  private def temp: ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-refresh")).orDie): path =>
      ZIO.attemptBlocking(Workspace.deleteSync(path)).orDie
end RefreshSpec
