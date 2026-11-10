package histos.server

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.plugin.ApiDigest
import histos.report.CallResult
import histos.resolve.Resolve
import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{HistosBuild, Jvm, Projects, Ship, module, organization, ships}
import histos.`scala`

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** Snapshots and a promotion ask for credentials before a socket. A ship with no asset does not. */
object CallsSnapshotLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val group  = organization"example.com"
  private val named  = module("snaplaw", Jvm)(scala3)

  def spec = suite("calls snapshot laws")(
    test("snapshots and promote name the missing credentials, and assets with nothing to upload do not"):
      Workspace
        .work("histos-calls-snap")
        .flatMap: dir =>
          val program =
            for
              graph <- ZIO
                .fromEither(Projects.organization(group)(ships(Ship("demo").next(1, 2, 3)(named)), named))
                .catchAll(error => ZIO.dieMessage(error.message))
              loaded  <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value.copy(env = Map.empty))
              found <- GraphCompile.jdk(machine).either
              jdk   <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              (snapped, snapHeard) <- speak(say =>
                Calls.publishSnapshots(
                  dir,
                  graph,
                  HistosBuild.fixed(graph),
                  machine,
                  jdk,
                  Nil,
                  say,
                  ApiDigest.trusted("ab" * 32),
                )
              )
              (promoted, promoteHeard) <- speak(say => Calls.promote(dir, machine, "nope", say))
              (assets, assetHeard)     <- speak(say => Calls.publishAssets(dir, graph, machine, Nil, say))
            yield assertTrue(
              snapped == Right(()),
              fault(snapHeard, "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"),
              promoted == Right(()),
              fault(promoteHeard, "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"),
              assets == Right(()),
              done(assetHeard).exists {
                case CallResult.Reported(report) =>
                  report.err.isEmpty && !report.out.exists(_.contains("SONATYPE"))
                case CallResult.Fault(_) => false
              },
            )
          program.provideSomeLayer[Resolve & Scope](ZLayer.succeed(ContentStore.in(dir.resolve("store"))))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def fault(heard: Chunk[Message], text: String): Boolean =
    done(heard) match
      case Some(CallResult.Fault(found)) => found.message.contains(text)
      case _                             => false

  private def done(heard: Chunk[Message]): Option[CallResult] =
    heard.collect { case Message.Done(result) => result }.lastOption

  private def speak[R, E](use: Say => ZIO[R, E, Unit]): ZIO[R, Nothing, (Either[E, Unit], Chunk[Message])] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      result <- use(say).either
      heard  <- messages.get
    yield (result, heard)

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath)
end CallsSnapshotLawSpec
