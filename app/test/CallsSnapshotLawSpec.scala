package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.plugin.ApiDigest
import histos.report.{BuildFault, CallResult}
import histos.resolve.Resolve
import histos.workspace.Workspace
import histos.{HistosBuild, Jvm, LawMachine, Projects, Ship, module, organization, ships}
import histos.`scala`
import slayer.stubbed

import zio.{Chunk, Ref, Scope, ZIO}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Snapshots ask for credentials before anything is staged. A ship with no asset is quiet unless it is named. */
object CallsSnapshotLawSpec extends ZIOSpecDefault:
  private val group = organization"example.com"
  private val named = module("snaplaw", Jvm)(scala"3.10.0")

  def spec = suite("calls snapshot laws")(
    test("snapshots name the missing credentials, and assets upload nothing unless a ship with none is named"):
      Workspace
        .work("histos-calls-snap")
        .flatMap: dir =>
          val program =
            for
              graph <- ZIO
                .fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(named)), named))
                .orDieWith(error => IllegalStateException(error.message))
              machine              <- LawMachine.at(dir)
              (snapped, snapHeard) <- speak(say =>
                Calls.publishSnapshots(
                  dir,
                  graph,
                  HistosBuild.fixed(graph),
                  machine,
                  LawMachine.fixtureJdk,
                  Nil,
                  say,
                  ApiDigest.trusted("ab" * 32),
                )
              )
              (quiet, quietHeard) <- speak(say => Calls.publishAssets(dir, graph, machine, Nil, say))
              (asked, askedHeard) <- speak(say => Calls.publishAssets(dir, graph, machine, List("demo"), say))
            yield assertTrue(
              snapped == Right(()),
              done(snapHeard)
                .collect { case CallResult.Fault(found) => found.message }
                .exists(_.startsWith("SONATYPE_USERNAME and SONATYPE_PASSWORD are required")),
              quiet == Right(()),
              outs(quietHeard) == "",
              done(quietHeard) == Some(CallResult.reported()),
              asked == Right(()),
              done(askedHeard) == Some(CallResult.Fault(BuildFault.Release("demo has no asset"))),
            )
          program.provideSomeLayer[Scope](stubbed[Resolve] ++ stubbed[ContentStore])
  )

  private def done(heard: Chunk[Message]): Option[CallResult] =
    heard.collect { case Message.Done(result) => result }.lastOption

  private def outs(heard: Chunk[Message]): String =
    heard.collect { case Message.Event(BuildEvent.Out(text)) => text }.mkString("\n")

  private def speak[R, E](use: Say => ZIO[R, E, Unit]): ZIO[R, Nothing, (Either[E, Unit], Chunk[Message])] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      result <- use(say).either
      heard  <- messages.get
    yield (result, heard)
end CallsSnapshotLawSpec
