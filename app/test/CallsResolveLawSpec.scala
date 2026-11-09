package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.load.Loader
import histos.native.NativeOrigin
import histos.pack.ClassJar
import histos.report.CallResult
import histos.resolve.Resolve
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A missing lock is refused. Update writes it. Verify and check then name the cell. */
object CallsResolveLawSpec extends ZIOSpecDefault:
  def spec = suite("calls resolve laws")(
    test("verify needs a lock, and update then check name the cell"):
      Workspace
        .work("histos-calls-resolve")
        .flatMap: dir =>
          val store = ContentStore.in(dir.resolve("store"))
          val layer =
            (Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++ ZLayer.succeed(store)) >+>
              Loader.live
          val program =
            for
              api <- apiJar(dir.resolve("histos-api.jar"))
              _   <- Workspace
                .writeString(dir.resolve("histos.scala"), buildFile)
                .catchAll(error => ZIO.dieMessage(error.message))
              (missing, missingHeard) <- speak(say =>
                Calls.resolve(dir, api, ResolveAction.Verify, say, NativeOrigin.Central)
              )
              (updated, updatedHeard) <- speak(say =>
                Calls.resolve(dir, api, ResolveAction.Update, say, NativeOrigin.Central)
              )
              lockPath = dir.resolve("dependencies.lock")
              lockFile                  <- ZIO.attemptBlocking(Files.isRegularFile(lockPath)).orDie
              lockText                  <- ZIO.attemptBlocking(Files.readString(lockPath)).orDie
              (verified, verifiedHeard) <- speak(say =>
                Calls.resolve(dir, api, ResolveAction.Verify, say, NativeOrigin.Central)
              )
              (checked, checkedHeard) <- speak(say =>
                Calls.resolve(dir, api, ResolveAction.Check, say, NativeOrigin.Central)
              )
            yield assertTrue(
              missing == Right(()),
              done(missingHeard).exists(faultHas("no dependency lock at")),
              updated == Right(()),
              done(updatedHeard).contains(CallResult.reported()),
              outs(updatedHeard).startsWith(lockPath.toString),
              outs(updatedHeard).contains("evict org.scala-lang scala-library 2.12.20 3.10.0"),
              outs(updatedHeard).contains("evict org.scala-lang scala-library 2.13.17 3.10.0"),
              lockFile,
              lockText.contains("resolvelaw"),
              lockText.contains("3.10.0"),
              verified == Right(()),
              done(verifiedHeard).contains(CallResult.reported()),
              outs(verifiedHeard).contains("resolvelaw 3.10.0 jvm"),
              checked == Right(()),
              done(checkedHeard).contains(CallResult.reported()),
              outs(checkedHeard).contains("resolvelaw 3.10.0 jvm"),
            )
          program.provideSomeLayer[Scope](layer)
  )

  private val buildFile =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val resolvelaw = module("resolvelaw", Jvm)(scala"3.10.0")
      |    Projects(resolvelaw)
      |""".stripMargin

  private def apiJar(dest: Path): ZIO[Any, Nothing, Path] =
    val source = Option(classOf[histos.HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(code => Option(code.getLocation))
      .flatMap(url => scala.util.Try(Path.of(url.toURI)).toOption)
    source match
      case None       => ZIO.dieMessage("api classes have no code source")
      case Some(path) =>
        ZIO
          .attemptBlocking(Files.isRegularFile(path))
          .orDie
          .flatMap: file =>
            if file then ZIO.succeed(path)
            else
              ClassJar
                .write(path, dest)
                .map(_.path)
                .catchAll(error => ZIO.dieMessage(error.toString))

  private def faultHas(text: String)(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains(text)
      case _                       => false

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
end CallsResolveLawSpec
