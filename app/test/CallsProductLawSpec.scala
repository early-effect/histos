package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.clean.Products
import histos.report.CallResult
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers}
import histos.{
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Named clean removes that module's products and leaves the shared root. A full clean forgets an empty store. */
object CallsProductLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("productlaw")

  def spec = suite("calls product laws")(
    test("an unknown name deletes nothing, a named clean leaves the shared root, and a full clean forgets nothing"):
      Workspace
        .work("histos-calls-product")
        .flatMap: dir =>
          val clazz   = dir.resolve(".histos").resolve("classes").resolve(lawName.value).resolve("Kept.class")
          val shared  = dir.resolve(".histos").resolve("provider").resolve("stay.txt")
          val tally   = Products.observation(dir)
          val program =
            for
              _                     <- write(clazz)
              _                     <- write(shared)
              _                     <- write(tally)
              (missed, missedHeard) <- speak(say => Calls.clean(dir, graph, List(ProjectName.trusted("missing")), say))
              missedKept            <- exists(clazz)
              missedShared          <- exists(shared)
              (named, namedHeard)   <- speak(say => Calls.clean(dir, graph, List(lawName), say))
              namedGone             <- exists(clazz)
              namedShared           <- exists(shared)
              namedTally            <- exists(tally)
              _                     <- write(tally)
              (wiped, wipedHeard)   <- speak(say => Calls.wipe(dir, say))
              wipedShared           <- exists(shared)
              wipedTally            <- exists(tally)
              (full, fullHeard)     <- speak(say => Calls.cleanFull(dir, say))
            yield assertTrue(
              missed == Right(()),
              done(missedHeard).exists(faultNamesMissing),
              missedKept,
              missedShared,
              named == Right(()),
              outs(namedHeard) == "cleaned productlaw",
              !namedGone,
              namedShared,
              !namedTally,
              wiped == Right(()),
              outs(wipedHeard) == "cleaned",
              !wipedShared,
              !wipedTally,
              full == Right(()),
              outs(fullHeard) == Products.forgotten(Nil),
            )
          program.provideSomeLayer[Scope](session(dir))
  )

  private def faultNamesMissing(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains("'missing' is not a project")
      case _                       => false

  private def exists(path: Path): ZIO[Any, Nothing, Boolean] =
    ZIO.attempt(Files.isRegularFile(path)).orDie

  private def write(path: Path): ZIO[Any, Nothing, Unit] =
    (Workspace.createDirectories(parentOf(path)) *> Workspace.writeString(path, "kept\n"))
      .catchAll(error => ZIO.dieMessage(error.message))

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

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def session(dir: Path) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      Compilers.live(1) ++
      CompileMemo.live

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)
end CallsProductLawSpec
