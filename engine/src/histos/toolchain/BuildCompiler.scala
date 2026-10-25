package histos.toolchain

import histos.{ScalaVersion, ScalaVersionError}
import histos.resolve.ResolveError
import zio.{IO, ZIO, Chunk}
import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault}
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets

/** The compiler embedded in the launcher, not the compiler a module requests.
  */
object BuildCompiler:
  private val parsed: Either[ScalaVersionError, ScalaVersion] =
    ScalaVersion.from(dotty.tools.dotc.config.Properties.versionNumberString)
  def version: IO[ResolveError, ScalaVersion] =
    ZIO.fromEither(parsed.left.map(ResolveError.CompilerVersion(_)))

  def identity(boundary: List[Path]): IO[ResolveError, Sha256] =
    for
      locations <- ZIO.foreach(
        List(
          classOf[dotty.tools.dotc.Driver],
          classOf[dotty.tools.tasty.TastyReader],
          classOf[dotty.tools.dotc.interfaces.ReporterResult],
        )
      )(locationOf)
      hashes <- ZIO.foreach((locations ++ boundary).distinct)(content)
    yield Sha256.of(
      Chunk.fromArray(
        hashes.map(_.value).mkString("\n").getBytes(StandardCharsets.UTF_8)
      )
    )

  private def locationOf(cls: Class[?]): IO[ResolveError, Path] =
    ZIO
      .attemptBlocking(
        Option(cls.getProtectionDomain)
          .flatMap(domain => Option(domain.getCodeSource))
          .flatMap(source => Option(source.getLocation))
          .map(url => Path.of(url.toURI))
      )
      .mapError(_ => ResolveError.Io(IoFailure(Path.of("compiler"), IoFault.Read)))
      .flatMap(value =>
        ZIO
          .fromOption(value)
          .mapError(_ => ResolveError.Io(IoFailure(Path.of("compiler"), IoFault.Read)))
      )

  private def content(location: Path): IO[ResolveError, Sha256] =
    for
      regular <- ZIO
        .attemptBlocking(Files.isRegularFile(location))
        .mapError(_ => ResolveError.Io(IoFailure(location, IoFault.Stat)))
      hash <-
        if regular then RuntimeFiles.sha256(location).mapError(ResolveError.Io(_))
        else RuntimeFiles.fingerprint(location).mapError(ResolveError.Io(_))
    yield hash
end BuildCompiler

final case class CompilerIdentity(
    version: ScalaVersion,
    embedded: Sha256,
    runtime: RuntimeIdentity,
):
  def fingerprint: Sha256 = Sha256.of(
    Chunk.fromArray(
      s"${version.value}\n${embedded.value}\n${Runtimes.identityToken(runtime)}"
        .getBytes(StandardCharsets.UTF_8)
    )
  )
