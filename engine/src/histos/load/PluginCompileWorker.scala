package histos.load

import histos.cache.ContentStore
import histos.jdk.{JdkVersion, Sha256}
import histos.schedule.RemoteCache
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileReplay, CompileRequest, CompileToolchain, CompilerStarts, Compilers}
import histos.{Platform, ProjectName, ScalaVersion, ScalacFlags}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{ExitCode, Scope, ZIO, ZIOAppDefault, ZLayer}

/** Child compiler. The parent owns the cache, the clock, and the cleared environment. */
object PluginCompileWorker extends ZIOAppDefault:
  private[histos] def requestFile(recipe: Path): Path = recipe.resolve(".histos-compile-request")

  private[histos] def resultFile(request: Path): Path =
    Option(request.getParent) match
      case Some(parent) => parent.resolve(".histos-compile-result")
      case None         => request.resolveSibling(".histos-compile-result")

  private[histos] def write(
      job: PluginCompileJob,
      compiler: List[Path],
      bridge: Path,
      support: List[Path],
      runtime: List[Path],
      apis: List[Sha256],
  ): ZIO[Any, PluginCompileError, Path] =
    val lines =
      List(
        field("module", job.module.value),
        field("scala", job.session.manifest.scalaVersion.value),
        field("jdk", job.session.jdk.value),
        field("jdkHome", job.session.jdkHome.toString),
        field("checkout", job.checkout.toString),
        field("classes", job.classes.toString),
        field("analysis", job.analysis.toString),
        field("bridge", bridge.toString),
      ) ++ compiler.map(path => field("compiler", path.toString))
        ++ support.map(path => field("support", path.toString))
        ++ runtime.map(path => field("runtime", path.toString))
        ++ job.sources.map(path => field("source", path.toString))
        ++ job.classpath.map(path => field("classpath", path.toString))
        ++ job.apiJars.map(path => field("api", path.toString))
        ++ apis.map(hash => field("apiHash", hash.value))
    val path = requestFile(job.recipe)
    ZIO
      .attemptBlocking {
        Files.createDirectories(job.recipe)
        Files.writeString(path, lines.mkString("\n") + "\n", StandardCharsets.UTF_8)
        path
      }
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Write)))

  def run =
    for
      args <- getArgs
      code <- args.toList match
        case path :: Nil => serve(Path.of(path))
        case _           => ZIO.succeed(ExitCode.failure)
    yield code

  private def serve(path: Path): ZIO[Any, Nothing, ExitCode] =
    compile(path).foldZIO(
      error => writeResult(path, s"failed\n${error.message}\n").as(ExitCode.failure),
      _ => writeResult(path, "ok\n").as(ExitCode.success),
    )

  private def compile(path: Path): ZIO[Any, PluginCompileError, Unit] =
    for
      text    <- read(path)
      request <- ZIO.fromEither(decode(text))
      _       <- ZIO.scoped {
        effect(request).provideSomeLayer[Scope](
          Compilers.live ++ CompileReplay.processStore ++ CompilerStarts.live ++ RemoteCache.off
        )
      }
    yield ()

  private def effect(
      request: Decoded
  ): ZIO[Scope & ContentStore & CompilerStarts & Compilers & RemoteCache, PluginCompileError, Unit] =
    val _       = request.module
    val compile = CompileRequest(
      CompileToolchain(request.scala, Platform.Jvm, request.runtime, ScalacFlags.empty),
      request.sources,
      request.classpath ++ request.api,
      request.classes,
      request.analysis,
      Nil,
      Nil,
      checkout = request.checkout,
      apiBytes = request.apiHashes,
    )
    PluginCompiler.supplied(
      compile,
      request.runtimeJars,
      request.compiler,
      request.bridge,
      request.support,
      placeholder,
    )

  private def writeResult(request: Path, body: String): ZIO[Any, Nothing, Unit] =
    val path = resultFile(request)
    ZIO.attemptBlocking(Files.writeString(path, body, StandardCharsets.UTF_8)).ignore

  private def read(path: Path): ZIO[Any, PluginCompileError, String] =
    ZIO
      .attemptBlocking(Files.readString(path, StandardCharsets.UTF_8))
      .mapError(_ => PluginCompileError.Io(IoFailure(path, IoFault.Read)))

  private def field(key: String, value: String): String = s"$key\t$value"

  private def decode(text: String): Either[PluginCompileError, Decoded] =
    val rows                                                 = text.linesIterator.flatMap(parse).toList
    def one(key: String): Either[PluginCompileError, String] =
      rows.collect { case Row.One(`key`, value) => value } match
        case value :: Nil => Right(value)
        case _            => Left(PluginCompileError.Worker(2, s"compiler request has no $key"))
    def many(key: String): List[String] =
      rows.collect { case Row.One(`key`, value) => value }
    for
      module <- one("module").flatMap(name =>
        ProjectName.from(name).left.map(_ => PluginCompileError.Worker(2, "module"))
      )
      scala <- one("scala").flatMap(raw => ScalaVersion.from(raw).left.map(_ => PluginCompileError.Worker(2, "scala")))
      jdk   <- one("jdk").flatMap(raw => JdkVersion.from(raw).left.map(_ => PluginCompileError.Worker(2, "jdk")))
      home  <- one("jdkHome")
      checkout <- one("checkout")
      classes  <- one("classes")
      analysis <- one("analysis")
      bridge   <- one("bridge")
      hashes   <- many("apiHash").foldLeft[Either[PluginCompileError, List[Sha256]]](Right(Nil)):
        case (Left(error), _)  => Left(error)
        case (Right(acc), raw) =>
          Sha256.from(raw) match
            case Right(hash) => Right(acc :+ hash)
            case Left(_)     => Left(PluginCompileError.Worker(2, "api"))
    yield Decoded(
      module,
      scala,
      histos.jdk.JdkRuntime(
        Path.of(home),
        jdk,
        histos.toolchain.RuntimeIdentity.Local(Path.of(home), placeholder),
      ),
      Path.of(checkout),
      Path.of(classes),
      Path.of(analysis),
      Path.of(bridge),
      many("compiler").map(Path.of(_)),
      many("support").map(Path.of(_)),
      many("runtime").map(Path.of(_)),
      many("source").map(Path.of(_)),
      many("classpath").map(Path.of(_)),
      many("api").map(Path.of(_)),
      hashes,
    )

  private def placeholder: Sha256 =
    histos.jdk.Sha256Digest.of(zio.Chunk.fromArray("jdk".getBytes(StandardCharsets.UTF_8)))

  private def parse(line: String): Option[Row] =
    val parts = line.split("\t", -1).toList
    parts match
      case key :: value :: Nil if key.nonEmpty => Some(Row.One(key, value))
      case _                                   => None

  private enum Row:
    case One(key: String, value: String)

  private final case class Decoded(
      module: ProjectName,
      scala: ScalaVersion,
      runtime: histos.jdk.JdkRuntime,
      checkout: Path,
      classes: Path,
      analysis: Path,
      bridge: Path,
      compiler: List[Path],
      support: List[Path],
      runtimeJars: List[Path],
      sources: List[Path],
      classpath: List[Path],
      api: List[Path],
      apiHashes: List[Sha256],
  )

end PluginCompileWorker
