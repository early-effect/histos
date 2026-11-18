package histos.plugin

import histos.cache.ContentStore
import histos.toolchain.{HostCompilerError, HostCompilerInputs, HostToolRole}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import zio.test.*
import zio.ZIO

/** The installed distribution. `HISTOS_JAR` is the payload and `HISTOS_REPO` is the checkout lock. A missing value
  * fails the assertion. This does not solve.
  */
object InstalledCompilerPayloadSpec extends ZIOSpecDefault:
  def spec = suite("installed compiler payload")(
    test("materialize checks the installed jar and leaves dependencies.lock unchanged") {
      val jar  = named("HISTOS_JAR")
      val repo = named("HISTOS_REPO")
      (jar, repo) match
        case (Some(jarPath), Some(repoPath)) =>
          checked(Path.of(jarPath), Path.of(repoPath))
        case _ =>
          ZIO.succeed(assertTrue(jar.isDefined, repo.isDefined))
    }
  )

  private def named(key: String): Option[String] =
    Option(System.getenv(key)).filter(_.nonEmpty)

  private def checked(jar: Path, repo: Path): ZIO[Any, HostCompilerError, TestResult] =
    ZIO.scoped {
      for
        home <- jdkHome
        lock = repo.resolve("dependencies.lock")
        before <- bytes(lock)
        root   <- Workspace.work("histos-a8-cas").mapError(HostCompilerError.Io(_))
        cas    <- ZIO
          .attemptBlocking(Files.createDirectories(root.resolve("cas")))
          .mapError(_ => HostCompilerError.Io(IoFailure(root.resolve("cas"), IoFault.Temp)))
        session <- HostCompilerInputs.materialize(jar, home, ContentStore.in(cas))
        after   <- bytes(lock)
      yield
        val roles  = session.tools.map(_.entry.role).toSet
        val signed = HostCompilerInputs.sign(
          session.manifest.version,
          session.manifest.scalaVersion,
          session.manifest.tools,
          session.manifest.jdk,
        )
        assertTrue(
          roles.contains(HostToolRole.Compiler),
          roles.contains(HostToolRole.Bridge),
          roles.contains(HostToolRole.Interfaces),
          roles.contains(HostToolRole.Runtime),
          session.fingerprint == signed.fingerprint,
          before.sameElements(after),
        )
    }

  private def jdkHome: ZIO[Any, HostCompilerError, Path] =
    ZIO
      .attempt(Option(System.getProperty("java.home")).filter(_.nonEmpty))
      .mapError(_ => HostCompilerError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))
      .flatMap:
        case Some(home) => ZIO.succeed(Path.of(home))
        case None       => ZIO.fail(HostCompilerError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))

  private def bytes(path: Path): ZIO[Any, HostCompilerError, Array[Byte]] =
    ZIO
      .attemptBlocking(Files.readAllBytes(path))
      .mapError(_ => HostCompilerError.Io(IoFailure(path, IoFault.Read)))
end InstalledCompilerPayloadSpec
