package histos.testrun

import histos.jdk.JdkRuntime
import histos.resolve.LockedTools
import histos.jdk.Sha256
import histos.zinc.{
  CompileRequest,
  CompileToolchain,
  JvmCompile,

  ScalacFlags,
}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, Exit, IO, Promise, Ref, UIO, ZIO}

/** One compiled provider runner. Cells that need the same install share it. One of them writes it.
  */
private[testrun] final class ProviderInstall(
    inflight: Ref[Map[Sha256, Promise[ModuleTestError, Path]]]
):
  def classes(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      jdk: JdkRuntime,
      classpath: List[Path],
  ): ZIO[LockedTools, ModuleTestError, Path] =
    val digest = ProviderInstall.digest(version, classpath)
    val root   = repo
      .resolve(".histos")
      .resolve("provider")
      .resolve(version.value)
      .resolve(digest.value)
    for
      promise <- Promise.make[ModuleTestError, Path]
      winner  <- inflight.modify: have =>
        have.get(digest) match
          case Some(existing) => (existing, have)
          case None           => (promise, have.updated(digest, promise))
      path <-
        if winner eq promise then occupy(promise, compile(module, version, jdk, classpath, root))
        else winner.await
    yield path

  private def occupy(
      promise: Promise[ModuleTestError, Path],
      compile: ZIO[LockedTools, ModuleTestError, Path],
  ): ZIO[LockedTools, ModuleTestError, Path] =
    ZIO.uninterruptibleMask: restore =>
      restore(compile).onExit:
        case Exit.Success(path)  => promise.succeed(path).unit
        case Exit.Failure(cause) => promise.failCause(cause).unit

  private def compile(
      module: ProjectName,
      version: ScalaVersion,
      jdk: JdkRuntime,
      classpath: List[Path],
      root: Path,
  ): ZIO[LockedTools, ModuleTestError, Path] =
    val source =
      root.resolve("src").resolve("subprocess").resolve("Provider.scala")
    val classes = root.resolve("classes")
    for
      _ <- ZIO
        .attemptBlocking {
          Files.createDirectories(source.getParent)
          Files.writeString(source, ProviderRunner.source)
        }
        .mapError(_ => ModuleTestError.Runner(source))
      _ <- JvmCompile
        .compile(
          CompileRequest(
            CompileToolchain(version, Platform.Jvm, jdk, ScalacFlags.empty),
            List(source),
            classpath,
            classes,
            root.resolve("analysis"),
            Nil,
            Nil,
          )
        )
        .mapError(error => ModuleTestError.Sources(module, version, error))
    yield classes
end ProviderInstall

private[testrun] object ProviderInstall:
  def make: UIO[ProviderInstall] =
    Ref
      .make(Map.empty[Sha256, Promise[ModuleTestError, Path]])
      .map(ProviderInstall(_))

  private def digest(version: ScalaVersion, classpath: List[Path]): Sha256 =
    val body =
      version.value + "\n" +
        classpath.map(_.toAbsolutePath.toString).sorted.mkString("\n") + "\n" +
        ProviderRunner.source
    Sha256.of(Chunk.fromArray(body.getBytes(StandardCharsets.UTF_8)))
end ProviderInstall
