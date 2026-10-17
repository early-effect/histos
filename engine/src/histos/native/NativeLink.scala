package histos.native

import histos.jdk.Sha256
import histos.testrun.MainClass
import histos.zinc.{CompileError, CompileOutput, CompileRequest, CompileToolchain, JvmCompile, ResolvedJdk, ScalacFlags}
import histos.{Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{Chunk, IO, ZIO}

object NativeKey:
  /** Identity of a native link. The binary path is not part of it. The clang version and target are. */
  def of(
      version: ScalaVersion,
      clang: ClangVersion,
      target: ClangTarget,
      sources: Sha256,
      artifacts: List[Sha256],
  ): Sha256 =
    val text =
      s"""${version.value}
         |${clang.value}
         |${target.value}
         |stubs
         |no-optimize
         |${sources.value}
         |${artifacts.map(_.value).mkString("\n")}
         |""".stripMargin
    Sha256.of(Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

enum LinkError:
  case Interrupted
  case Linker(text: String)
  case Unexpected(kind: String)

  def message: String = this match
    case Interrupted     => "the native link was interrupted"
    case Linker(text)    => text
    case Unexpected(kind) => s"the native link failed ($kind)"

final case class Linked(binary: Path, key: Sha256)

enum NativeError:
  case Clang(miss: ClangMiss)
  case Fetch(error: NativeFetch)
  case Compile(error: CompileError)
  case Link(error: LinkError)

  def message: String = this match
    case Clang(miss)   => miss.message
    case Fetch(error)  => error.message
    case Compile(error) => error.message
    case Link(error)   => error.message

enum NativeDeclaration:
  case NotNative
  case Major(major: histos.ClangMajor)
  case Missing(name: ProjectName)
  case Disagree(left: ProjectName, right: ProjectName)

  def message: String = this match
    case NotNative          => ""
    case Major(major)       => s"clang ${major.value}"
    case Missing(name)      => s"project '${name.value}' targets Native and does not name a clang major"
    case Disagree(left, right) =>
      s"projects '${left.value}' and '${right.value}' name different clang majors"

object NativeProject:
  def declaration(graph: ProjectGraph): NativeDeclaration =
    val pins = graph.projects.flatMap:
      case histos.Module(name, platforms, _, _, _, _, clang, _, _, _) if platforms.all.contains(Platform.Native) =>
        clang match
          case histos.ClangPin.Major(major) => List(name -> Some(major))
          case histos.ClangPin.Unused       => List(name -> None)
      case _ => Nil
    pins match
      case Nil => NativeDeclaration.NotNative
      case (name, None) :: _ => NativeDeclaration.Missing(name)
      case (name, Some(major)) :: rest =>
        rest.foldLeft[NativeDeclaration](NativeDeclaration.Major(major)):
          case (NativeDeclaration.Major(chosen), (_, Some(found))) if found == chosen => NativeDeclaration.Major(chosen)
          case (NativeDeclaration.Major(_), (other, Some(_))) => NativeDeclaration.Disagree(name, other)
          case (NativeDeclaration.Major(_), (missing, None)) => NativeDeclaration.Missing(missing)
          case (done, _) => done

object NativeLink:
  def link(
      major: histos.ClangMajor,
      env: Map[String, String],
      sourcesRoot: Path,
      work: Path,
      jdk: ResolvedJdk,
      main: MainClass,
  ): IO[NativeError, Linked] =
    for
      clang <- ClangProbe.resolve(major, env).mapError(NativeError.Clang(_))
      libs  <- NativeClasspath.fetch.mapError(NativeError.Fetch(_))
      sources <- histos.zinc.Sources.select(sourcesRoot, JvmCompile.Scala390, Platform.Native).mapError(NativeError.Compile(_))
      digest <- JvmCompile.sourceDigest(sources).mapError(NativeError.Compile(_))
      compiled <- compile(sources, libs, work, jdk).mapError(NativeError.Compile(_))
      hashes <- NativeClasspath.hashes(libs).mapError(NativeError.Fetch(_))
      binary <- invoke(clang, main, compiled.classes :: libs.libraries, work).mapError(NativeError.Link(_))
    yield Linked(binary, NativeKey.of(JvmCompile.Scala390, clang.version, clang.target, digest, hashes))

  private def compile(
      sources: List[Path],
      libs: NativeClasspath,
      work: Path,
      jdk: ResolvedJdk,
  ): IO[CompileError, CompileOutput] =
    val request = CompileRequest(
      CompileToolchain(JvmCompile.Scala390, Platform.Native, jdk, ScalacFlags.empty),
      sources,
      libs.libraries,
      work.resolve("classes"),
      work.resolve("analysis"),
      Nil,
    )
    JvmCompile.compilePrepared(request, List(libs.plugin))

  private def invoke(clang: Clang, main: MainClass, classpath: List[Path], work: Path): IO[LinkError, Path] =
    ZIO.attemptBlockingInterrupt(run(clang, main, classpath, work)).mapError:
      case _: InterruptedException => LinkError.Interrupted
      case other                   => LinkError.Unexpected(other.getClass.getName)
    .flatMap(ZIO.fromEither)

  private def run(clang: Clang, main: MainClass, classpath: List[Path], work: Path): Either[LinkError, Path] =
    try
      Right(scala.scalanative.util.Scope { scope =>
        val config = scala.scalanative.build.Config.empty
          .withLogger(scala.scalanative.build.Logger.nullLogger)
          .withBaseDir(work)
          .withModuleName("hello")
          .withMainClass(Some(main.binary))
          .withClassPath(classpath)
          .withCompilerConfig(
            scala.scalanative.build.NativeConfig.empty
              .withClang(clang.clang)
              .withClangPP(clang.clangpp)
              .withLinkStubs(true)
              .withOptimize(false)
              .withMultithreading(false)
              .withCompileOptions(Seq("-Qunused-arguments"))
          )
        scala.scalanative.build.Build.buildCachedAwait(config)(using scope)
      })
    catch
      case _: InterruptedException => Left(LinkError.Interrupted)
      case error: scala.scalanative.build.BuildException =>
        Left(LinkError.Linker(Option(error.getMessage).getOrElse("the native linker failed")))
      case error: scala.scalanative.linker.LinkingException =>
        Left(LinkError.Linker(Option(error.getMessage).getOrElse("the native linker failed")))
