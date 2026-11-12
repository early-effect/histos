package histos.server

import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.load.Loader
import histos.native.NativeOrigin
import histos.pack.ClassJar
import histos.report.{BuildFault, CallResult}
import histos.resolve.{ClasspathScope, DependencyLock, Resolve, ResolveError, ToolRole}
import histos.workspace.Workspace
import histos.{CompilerClosure, Platform, ProjectName}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

/** A missing lock is refused. Update writes it. Verify and check then name the cell. The solve reads the Coursier cache
  * offline through the compiler closure, so a cold cache is its named precondition, and the compiler rows it writes are
  * the ones this checkout pins.
  */
object CallsResolveLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("resolvelaw")

  def spec = suite("calls resolve laws")(
    test("verify needs a lock, and update then check name the cell"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-calls-resolve")
          .flatMap: dir =>
            val lockPath = dir.resolve("dependencies.lock")
            val program  =
              for
                api                     <- apiJar(dir.resolve("histos-api.jar"))
                _                       <- Workspace.writeString(dir.resolve("histos.scala"), buildFile(scala3.value))
                (missing, missingHeard) <- speak(say =>
                  Calls.resolve(dir, api, ResolveAction.Verify, say, NativeOrigin.Central)
                )
                (updated, updatedHeard) <- speak(say =>
                  Calls.resolve(dir, api, ResolveAction.Update, say, NativeOrigin.Central)
                )
                lockText                  <- ZIO.attemptBlocking(Files.readString(lockPath)).orDie
                (verified, verifiedHeard) <- speak(say =>
                  Calls.resolve(dir, api, ResolveAction.Verify, say, NativeOrigin.Central)
                )
                (checked, checkedHeard) <- speak(say =>
                  Calls.resolve(dir, api, ResolveAction.Check, say, NativeOrigin.Central)
                )
              yield
                val written  = DependencyLock.parse(lockText)
                val compiler = (lock: DependencyLock) =>
                  lock.tools.filter(row => row.id.role == ToolRole.Compiler && row.id.scalaVersion == scala3)
                assertTrue(
                  missing == Right(()),
                  done(missingHeard) == Some(CallResult.Fault(BuildFault.Resolve(ResolveError.NoLock(lockPath)))),
                  updated == Right(()),
                  done(updatedHeard).contains(CallResult.reported()),
                  outs(updatedHeard).linesIterator.toList.headOption.contains(lockPath.toString),
                  written.map(_.cells.map(cell => (cell.module, cell.scalaVersion, cell.platform, cell.scope))) ==
                    Right(List((lawName, scala3, Platform.Jvm, ClasspathScope.Compile))),
                  written.map(compiler) == Right(compiler(closure.lock)),
                  compiler(closure.lock).nonEmpty,
                  verified == Right(()),
                  done(verifiedHeard).contains(CallResult.reported()),
                  outs(verifiedHeard).contains(s"resolvelaw ${scala3.value} jvm"),
                  checked == Right(()),
                  done(checkedHeard).contains(CallResult.reported()),
                  outs(checkedHeard).contains(s"resolvelaw ${scala3.value} jvm"),
                )
            program.provideSomeLayer[Resolve & Scope](
              (ZLayer.service[Resolve] ++ ZLayer.succeed(ContentStore.in(dir.resolve("store")))) >+> Loader.live
            )
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer)

  private def buildFile(version: String) =
    s"""import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val resolvelaw = module("resolvelaw", Jvm)(scala"$version")
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
