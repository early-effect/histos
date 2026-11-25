package histos.server

import histos.bsp.Frame
import histos.build.BuildEvent
import histos.cli.{ClientSurface, ConsentDecision, ConsentRoute}
import histos.lock.{
  ExecutionKind,
  LockDecision,
  LockEvent,
  LockReply,
  LockRevisionState,
  LockStrategy,
  LockStrategyError,
  LockSubject,
  LockTransactionError,
}
import histos.mcp.McpConsent
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
  PluginEvents,
  ResolutionIntent,
}
import histos.resolve.{
  Inspection,
  LockDocument,
  LockPreparer,
  LockRepairError,
  LockStoreFault,
  PhaseInspection,
  PrepareFault,
  PrepareOutcome,
  PrepareRequest,
  ResolutionSnapshot,
  ResolveError,
  SnapshotLocator,
  SnapshotSelection,
  Solved,
  SuspendedCommand,
  VersionedPin,
}
import histos.process.RelayCommand
import histos.report.WireJson
import histos.workspace.Workspace
import histos.ProjectName
import histos.bsp.BspConsent
import histos.suite.HistosSpec

import WireJson.given

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.json.*
import zio.{Chunk, Exit, IO, Queue, Ref, UIO, ZIO}
import zio.test.*

object DaemonConsentSpec extends HistosSpec:
  private val utf8  = StandardCharsets.UTF_8
  private val app   = PluginCoordinate.Module(ProjectName("app"))
  private val tools = PluginCoordinate.Module(ProjectName("tools"))
  private val shots = histos.resolve.LocatorName("shots")

  private val original =
    LockDocument(List(pin(app, "app-1"), pin(tools, "tools-1")), List(SnapshotLocator(shots, hash("snap-1"))))

  private val merged =
    LockDocument(List(pin(app, "app-2"), pin(tools, "tools-1")), List(SnapshotLocator(shots, hash("snap-1"))))

  def spec = suite("daemon consent")(
    test("one accept commits the fixture, adopts it, and the watch resumes once") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, events) = heard
          desk    <- open(root, runs, say)
          _       <- write(root.resolve("dependencies.lock"), LockDocument.render(original))
          outcome <- desk.prepare(request("once"))
          offer   <- offered(outcome)
          stdin  = (RunInput.Stdin(RunBytes.encode(Chunk.fromArray("y".getBytes(utf8)))): RunInput).toJson
          accept = (LockReply.Accept(offer.proposal, offer.base): LockReply).toJson
          delivered <- deliver(desk, say, List(stdin, accept, accept))
          (commands, answers) = delivered
          text    <- lockText(root)
          adopted <- desk.adopted
          count   <- runs.get
          resumes <- desk.resumes
          feed    <- events
        yield
          val stdinKept = commands.toList match
            case RelayCommand.Stdin(bytes) :: Nil => bytes == Chunk.fromArray("y".getBytes(utf8))
            case _                                => false
          val watched = answers match
            case Some(WatchAnswer.Resumed) :: Some(WatchAnswer.Repeated) :: Nil => true
            case _                                                              => false
          val committed = adopted match
            case Some(snapshot) =>
              snapshot.selection == SnapshotSelection.Committed &&
              snapshot.lockRevision == LockRevisionState.Present(LockDocument.revision(merged))
            case None => false
          val published = feed.toList match
            case BuildEvent.Lock(LockEvent.Offered(found)) :: Nil => found.proposal == offer.proposal
            case _                                                => false
          assertTrue(
            stdinKept,
            watched,
            committed,
            published,
            text == LockDocument.render(merged),
            text.contains(s"retain ${shots.value}"),
            count == 1,
            resumes == 1,
          )
      }
    },
    test("disconnect before accept discards and a later reply does not run") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, events) = heard
          desk    <- open(root, runs, say)
          _       <- write(root.resolve("dependencies.lock"), LockDocument.render(original))
          outcome <- desk.prepare(request("disconnect"))
          offer   <- offered(outcome)
          _       <- desk.discard
          answers <- deliver(desk, say, List((LockReply.Accept(offer.proposal, offer.base): LockReply).toJson))
          text    <- lockText(root)
          adopted <- desk.adopted
          count   <- runs.get
          resumes <- desk.resumes
          feed    <- events
        yield
          val late = answers match
            case (_, None :: Nil) => true
            case _                => false
          val reported = feed.toList match
            case BuildEvent.Lock(LockEvent.Offered(found)) :: BuildEvent.Err(text) :: Nil =>
              found.proposal == offer.proposal &&
              text == ResolveError.Transaction(LockTransactionError.StaleProposal(offer.proposal)).message
            case _ => false
          assertTrue(
            late,
            reported,
            text == LockDocument.render(original),
            adopted.isEmpty,
            count == 0,
            resumes == 0,
          )
      }
    },
    test("program stdin reaches the program and does not enter answer") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, _) = heard
          desk    <- open(root, runs, say)
          _       <- write(root.resolve("dependencies.lock"), LockDocument.render(original))
          outcome <- desk.prepare(request("stdin"))
          _       <- offered(outcome)
          stdin = (RunInput.Stdin(RunBytes.encode(Chunk.fromArray("y".getBytes(utf8)))): RunInput).toJson
          delivered <- deliver(desk, say, List(stdin))
          (commands, answers) = delivered
          text  <- lockText(root)
          count <- runs.get
          held  <- desk.iteration
        yield
          val reached = commands.toList match
            case RelayCommand.Stdin(bytes) :: Nil => bytes == Chunk.fromArray("y".getBytes(utf8))
            case _                                => false
          assertTrue(reached, answers.isEmpty, held.isEmpty, count == 0, text == LockDocument.render(original))
      }
    },
    test("redirected and unnegotiated clients stay diagnoses") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, _) = heard
          desk    <- open(root, runs, say)
          _       <- write(root.resolve("dependencies.lock"), LockDocument.render(original))
          outcome <- desk.prepare(request("surface"))
          offer   <- offered(outcome)
          count   <- runs.get
          _       <- desk.discard
        yield
          val call       = Call.Compile(List(ProjectName("app")))
          val redirected = ConsentRoute.decide(ClientSurface.Cli(false), offer, call)
          val mcp        = McpConsent.unnegotiated(offer, call)
          val bsp        = BspConsent.unnegotiated(offer, call)
          val required   = redirected match
            case ConsentDecision.Required(error, _, update, local) =>
              error == LockTransactionError.ConsentRequired(offer.proposal) &&
              update.contains("--lock=update") &&
              local.contains("--lock=local")
            case ConsentDecision.Prompt | ConsentDecision.Structured | ConsentDecision.Diagnosis(_) =>
              false
          val diagnosed = List(mcp, bsp).forall:
            case ConsentDecision.Diagnosis(noted) =>
              noted.explain.nonEmpty && !noted.explain.contains("[y/N]")
            case ConsentDecision.Prompt | ConsentDecision.Structured | ConsentDecision.Required(_, _, _, _) =>
              false
          assertTrue(required, diagnosed, count == 0)
      }
    },
    test("a corrupt lock is a typed refusal before an offer") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, events) = heard
          desk      <- open(root, runs, say)
          _         <- write(root.resolve("dependencies.lock"), "")
          prepared  <- desk.prepare(request("corrupt")).exit
          feed      <- events
          count     <- runs.get
          iteration <- desk.iteration
        yield
          val detail = prepared match
            case Exit.Success(_)     => None
            case Exit.Failure(cause) =>
              cause.failureOption.flatMap: error =>
                ConsentDesk.reportOf(error) match
                  case ConsentReport.Corrupt(found)                               => Some(found)
                  case ConsentReport.Transaction(_) | ConsentReport.Unreported(_) => None
          val refusal = detail match
            case Some(found) =>
              found.value == "the lock document is empty" && found.value != LockStoreFault.Corrupt(found).toString
            case None => false
          assertTrue(refusal, feed.isEmpty, count == 0, iteration.isEmpty)
      }
    },
    test("a replacement declines the cancelled proposal before publishing the new offer") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, events) = heard
          desk    <- open(root, runs, say)
          _       <- write(root.resolve("dependencies.lock"), LockDocument.render(original))
          first   <- desk.prepare(request("replace-a")).flatMap(offered)
          second  <- desk.prepare(request("replace-b")).flatMap(offered)
          late    <- deliver(desk, say, List(accept(first)))
          held    <- lockText(root)
          before  <- runs.get
          kept    <- deliver(desk, say, List(accept(second)))
          written <- lockText(root)
          adopted <- desk.adopted
          after   <- runs.get
          feed    <- events
        yield
          val ignored = late match
            case (_, None :: Nil) => true
            case _                => false
          val resumed = kept match
            case (_, Some(WatchAnswer.Resumed) :: Nil) => true
            case _                                     => false
          val published = feed.toList match
            case BuildEvent.Lock(LockEvent.Offered(cancelled)) :: BuildEvent.Lock(LockEvent.Offered(next)) ::
                BuildEvent.Err(message) :: Nil =>
              cancelled.proposal == first.proposal &&
              next.proposal == second.proposal &&
              message == ResolveError.Transaction(LockTransactionError.StaleProposal(first.proposal)).message
            case _ => false
          val committed = adopted match
            case Some(snapshot) => snapshot.selection == SnapshotSelection.Committed
            case None           => false
          assertTrue(
            first.proposal != second.proposal,
            ignored,
            resumed,
            published,
            held == LockDocument.render(original),
            written == LockDocument.render(merged),
            before == 0,
            after == 1,
            committed,
          )
      }
    },
    test("an unattended repeat does not publish another offer") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, events) = heard
          desk    <- open(root, runs, say)
          _       <- write(root.resolve("dependencies.lock"), LockDocument.render(original))
          first   <- desk.prepare(request("quiet-a", Interaction.Unattended)).flatMap(offered)
          second  <- desk.prepare(request("quiet-b", Interaction.Unattended)).flatMap(offered)
          late    <- deliver(desk, say, List(accept(second)))
          held    <- lockText(root)
          before  <- runs.get
          kept    <- deliver(desk, say, List(accept(first)))
          written <- lockText(root)
          after   <- runs.get
          feed    <- events
        yield
          val ignored = late match
            case (_, None :: Nil) => true
            case _                => false
          val resumed = kept match
            case (_, Some(WatchAnswer.Resumed) :: Nil) => true
            case _                                     => false
          val published = feed.toList match
            case BuildEvent.Lock(LockEvent.Offered(found)) :: BuildEvent.Err(message) :: Nil =>
              found.proposal == first.proposal &&
              message == ResolveError.Transaction(LockTransactionError.StaleProposal(second.proposal)).message
            case _ => false
          assertTrue(
            first.proposal != second.proposal,
            ignored,
            resumed,
            published,
            held == LockDocument.render(original),
            written == LockDocument.render(merged),
            before == 0,
            after == 1,
          )
      }
    },
    test("the production preparer accepts a lock document and refuses a dependency lock") {
      rooted { root =>
        val lock = root.resolve("dependencies.lock")
        val text = LockDocument.render(original)
        val cell = "cell app 3.8.1 jvm\nartifact com.example demo 1.0.0\n"
        for
          heard <- recording
          (say, _) = heard
          desk <- ConsentDesk.open(
            root,
            ConsentDesk.strategy(ExecutionKind.Local, None),
            ConsentDesk.command,
            ConsentDesk.preparer(root),
            ConsentDesk.feed(say),
          )
          _        <- write(lock, text)
          prepared <- desk.prepare(request("production")).exit
          kept     <- lockText(root)
          adopted  <- desk.adopted
          _        <- write(lock, cell)
          refused  <- desk.prepare(request("cell")).exit
          after    <- lockText(root)
        yield
          val selected = prepared match
            case Exit.Success(PrepareOutcome.Selected(_)) => true
            case Exit.Success(_)                          => false
            case Exit.Failure(_)                          => false
          val corrupt = refused match
            case Exit.Success(_)     => false
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(error) =>
                  ConsentDesk.reportOf(error) match
                    case ConsentReport.Corrupt(found) =>
                      found.value == "line 1 is not a lock record" && found.value != LockStoreFault
                        .Corrupt(found)
                        .toString
                    case ConsentReport.Transaction(_) | ConsentReport.Unreported(_) => false
                case None => false
          assertTrue(selected, adopted.isDefined, kept == text, corrupt, after == cell)
      }
    },
    test("a source change declines the pending proposal before a later accept") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, events) = heard
          desk    <- open(root, runs, say)
          _       <- write(root.resolve("dependencies.lock"), LockDocument.render(original))
          outcome <- desk.prepare(request("wake-decline"))
          offer   <- offered(outcome)
          _       <- desk.onChange(List(root.resolve("src")))
          late    <- deliver(desk, say, List(accept(offer)))
          text    <- lockText(root)
          count   <- runs.get
          held    <- desk.iteration
          feed    <- events
        yield
          val ignored = late match
            case (_, None :: Nil) => true
            case _                => false
          val reported = feed.toList match
            case BuildEvent.Lock(LockEvent.Offered(found)) :: BuildEvent.Err(message) :: Nil =>
              found.proposal == offer.proposal &&
              message == ResolveError.Transaction(LockTransactionError.StaleProposal(offer.proposal)).message
            case _ => false
          assertTrue(ignored, reported, text == LockDocument.render(original), count == 0, held.isEmpty)
      }
    },
    test("a source change clears the retained resume and an empty wake does not") {
      rooted { root =>
        for
          runs  <- Ref.make(0)
          heard <- recording
          (say, _) = heard
          desk    <- open(root, runs, say)
          _       <- write(root.resolve("dependencies.lock"), LockDocument.render(original))
          outcome <- desk.prepare(request("wake"))
          offer   <- offered(outcome)
          _       <- deliver(desk, say, List(accept(offer)))
          held    <- desk.iteration
          _       <- desk.onChange(Nil)
          still   <- desk.iteration
          _       <- desk.onChange(List(root.resolve("src")))
          cleared <- desk.iteration
          resumes <- desk.resumes
        yield assertTrue(
          held.contains(WatchAnswer.Resumed),
          still.contains(WatchAnswer.Resumed),
          cleared.isEmpty,
          resumes == 1,
        )
      }
    },
  )

  private def accept(offer: histos.lock.LockOffer): String =
    (LockReply.Accept(offer.proposal, offer.base): LockReply).toJson

  private def open(root: Path, runs: Ref[Int], say: Say): UIO[ConsentDesk] =
    val command = new SuspendedCommand:
      def run(snapshot: ResolutionSnapshot) =
        runs.update(_ + 1)
    ConsentDesk.open(root, fixed(LockDecision.OfferRepair), command, preparer, ConsentDesk.feed(say))

  private def request(token: String, interaction: Interaction = Interaction.Offers): PrepareRequest =
    PrepareRequest(
      InvocationContext(
        InvocationId.derive(token),
        ClientId("cli"),
        CommandIdentity.Compile(List(ProjectName("app"))),
        ExecutionKind.Local,
        interaction,
        OfflineSelection.Online,
      ),
      ResolutionIntent.Inherit,
      PluginEvents.closedInvocation,
      None,
      refresh = false,
    )

  private val preparer: LockPreparer = new LockPreparer:
    def inspect(
        request: PrepareRequest,
        revision: LockRevisionState,
        document: LockDocument,
    ): IO[PrepareFault, Inspection] =
      ZIO.succeed(
        Inspection(
          hash("declarations"),
          List(PhaseInspection(histos.lock.LockPhase.Seed, histos.lock.DeclarationDrift.Changed(hash("declarations")))),
        )
      )

    def solve(request: PrepareRequest, inspection: Inspection): IO[PrepareFault, Solved] =
      ZIO.succeed(Solved(List(app), List(pin(app, "app-2")), Nil, Nil))

  private def fixed(decision: LockDecision): LockStrategy =
    new LockStrategy:
      def decide(context: histos.lock.LockContext): IO[LockStrategyError, LockDecision] =
        ZIO.succeed(decision)

  private def offered(outcome: PrepareOutcome): UIO[histos.lock.LockOffer] =
    outcome match
      case PrepareOutcome.Offered(offer)                                                           => ZIO.succeed(offer)
      case PrepareOutcome.Committed(_, _) | PrepareOutcome.Blocked(_) | PrepareOutcome.Selected(_) =>
        ZIO.dieMessage("the fixture offered a repair")

  private def deliver(
      desk: ConsentDesk,
      say: Say,
      payloads: List[String],
  ): UIO[(Chunk[histos.process.RelayCommand], List[Option[WatchAnswer]])] =
    for
      program <- Queue.unbounded[histos.process.RelayCommand]
      replies <- Queue.unbounded[LockReply]
      buffer = new ByteArrayOutputStream
      _ <- ZIO.foreachDiscard(payloads): payload =>
        Frame.write(buffer, payload).catchAll(error => ZIO.dieMessage(error.message))
      _        <- Daemon.readFrames(new ByteArrayInputStream(buffer.toByteArray), Some(program), Some(replies))
      found    <- replies.takeAll
      answers  <- ZIO.foreach(found.toList)(reply => Daemon.hear(desk, say, reply))
      commands <- program.takeAll
    yield (commands, answers)

  private def recording: UIO[(Say, UIO[Chunk[BuildEvent]])] =
    Ref.make(Chunk.empty[BuildEvent]).map { found =>
      val say = new Say({
        case Message.Event(event)                                                    => found.update(_ :+ event)
        case Message.Ready | Message.Output(_) | Message.Done(_) | Message.Marker(_) => ZIO.unit
      })
      (say, found.get)
    }

  private def pin(coordinate: PluginCoordinate, label: String): VersionedPin =
    VersionedPin(coordinate, LockSubject.Artifact, hash(label), None)

  private def hash(text: String): ContentHash =
    Digests.content(text.getBytes(utf8))

  private def write(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking(Files.writeString(path, text, utf8)).orDie.unit

  private def lockText(root: Path): UIO[String] =
    ZIO.attemptBlocking(Files.readString(root.resolve("dependencies.lock"), utf8)).orDie

  private def rooted[A](use: Path => IO[LockRepairError, A]): IO[LockRepairError, A] =
    ZIO.scoped(
      ZIO
        .acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-consent")).orDie)(path =>
          ZIO.attemptBlocking(Workspace.deleteSync(path)).orDie
        )
        .flatMap(use)
    )
end DaemonConsentSpec
