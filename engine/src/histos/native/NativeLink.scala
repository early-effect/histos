package histos.native

import histos.jdk.Sha256
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildProcess, ChildRequest}
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileOutput,
  CompileRequest,
  CompileToolchain,
  JvmCompile,
  ResolvedJdk,
  ScalacFlags
}
import histos.{Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO}

object NativeKey:
  /** Identity of a native link. The binary path is not part of it. The clang
    * version and target are.
    */
  def of(
      version: ScalaVersion,
      clang: ClangVersion,
      target: ClangTarget,
      sources: Sha256,
      artifacts: List[Sha256]
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
    case Interrupted      => "the native link was interrupted"
    case Linker(text)     => text
    case Unexpected(kind) => s"the native link failed ($kind)"

final case class Linked(binary: Path, key: Sha256)

enum NativeError:
  case Clang(miss: ClangMiss)
  case Fetch(error: NativeFetch)
  case Compile(error: CompileError)
  case Link(error: LinkError)

  def message: String = this match
    case Clang(miss)    => miss.message
    case Fetch(error)   => error.message
    case Compile(error) => error.message
    case Link(error)    => error.message

enum NativeDeclaration:
  case NotNative
  case Major(major: histos.ClangMajor)
  case Missing(name: ProjectName)
  case Disagree(left: ProjectName, right: ProjectName)

  def message: String = this match
    case NotNative     => ""
    case Major(major)  => s"clang ${major.value}"
    case Missing(name) =>
      s"project '${name.value}' targets Native and does not name a clang major"
    case Disagree(left, right) =>
      s"projects '${left.value}' and '${right.value}' name different clang majors"

object NativeProject:
  def declaration(graph: ProjectGraph): NativeDeclaration =
    val pins = graph.projects.flatMap:
      case histos.Module(
            name,
            platforms,
            _,
            _,
            _,
            _,
            clang,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) if platforms.all.contains(Platform.Native) =>
        clang match
          case histos.ClangPin.Major(major) => List(name -> Some(major))
          case histos.ClangPin.Unused       => List(name -> None)
      case _ => Nil
    pins match
      case Nil                         => NativeDeclaration.NotNative
      case (name, None) :: _           => NativeDeclaration.Missing(name)
      case (name, Some(major)) :: rest =>
        rest.foldLeft[NativeDeclaration](NativeDeclaration.Major(major)):
          case (NativeDeclaration.Major(chosen), (_, Some(found)))
              if found == chosen =>
            NativeDeclaration.Major(chosen)
          case (NativeDeclaration.Major(_), (other, Some(_))) =>
            NativeDeclaration.Disagree(name, other)
          case (NativeDeclaration.Major(_), (missing, None)) =>
            NativeDeclaration.Missing(missing)
          case (done, _) => done

/** Clang, the toolchain, and where the link runs. The sources or the classpath
  * are the other argument.
  */
final case class NativeLinkRequest(
    major: histos.ClangMajor,
    toolchain: NativeToolchain,
    env: Map[String, String],
    work: Path,
    jdk: ResolvedJdk,
    main: MainClass
)

object NativeLink:
  /** `env` is the machine's: clang is found on its `PATH`, and the linker child
    * gets that `PATH`.
    */
  def link(
      request: NativeLinkRequest,
      sourcesRoot: Path
  ): IO[NativeError, Linked] =
    val NativeLinkRequest(major, toolchain, env, work, jdk, main) = request
    for
      libs <- NativeClasspath.fetch(toolchain).mapError(NativeError.Fetch(_))
      clang <- ClangProbe.resolve(major, env).mapError(NativeError.Clang(_))
      sources <- histos.zinc.Sources
        .select(sourcesRoot, JvmCompile.Scala390, Platform.Native)
        .mapError(NativeError.Compile(_))
      digest <- JvmCompile
        .sourceDigest(sources)
        .mapError(NativeError.Compile(_))
      compiled <- compile(sources, libs, work, jdk).mapError(
        NativeError.Compile(_)
      )
      hashes <- NativeClasspath
        .hashes(libs, toolchain.source)
        .mapError(NativeError.Fetch(_))
      binary <- invoke(
        clang,
        main,
        compiled.classes :: libs.libraries,
        work,
        jdk,
        toolchain,
        env
      )
    yield Linked(
      binary,
      NativeKey.of(
        JvmCompile.Scala390,
        clang.version,
        clang.target,
        digest,
        hashes
      )
    )

  /** Link `.nir` that is already compiled. The native libraries are added after
    * `classpath`.
    */
  def linkClasspath(
      request: NativeLinkRequest,
      classpath: List[Path]
  ): IO[NativeError, Path] =
    val NativeLinkRequest(major, toolchain, env, work, jdk, main) = request
    for
      libs <- NativeClasspath.fetch(toolchain).mapError(NativeError.Fetch(_))
      clang <- ClangProbe.resolve(major, env).mapError(NativeError.Clang(_))
      binary <- invoke(
        clang,
        main,
        withoutLibraries(classpath, libs.libraries) ++ libs.libraries,
        work,
        jdk,
        toolchain,
        env
      )
    yield binary

  private def compile(
      sources: List[Path],
      libs: NativeClasspath,
      work: Path,
      jdk: ResolvedJdk
  ): IO[CompileError, CompileOutput] =
    val request = CompileRequest(
      CompileToolchain(
        JvmCompile.Scala390,
        Platform.Native,
        jdk,
        ScalacFlags.empty
      ),
      sources,
      libs.libraries,
      work.resolve("classes"),
      work.resolve("analysis"),
      Nil
    )
    JvmCompile.compilePrepared(request, List(libs.plugin))

  /** The toolchain appends its own native libraries. A resolved test jar of the
    * same file is the same library.
    */
  private def withoutLibraries(
      classpath: List[Path],
      libraries: List[Path]
  ): List[Path] =
    val names =
      libraries.flatMap(path => Option(path.getFileName).map(_.toString)).toSet
    classpath.filterNot(path =>
      Option(path.getFileName).exists(name => names.contains(name.toString))
    )

  /** The linker JVM is a fresh environment. Linux clang finds `ld` on `PATH`.
    * An empty environment makes that exec fail.
    */
  private[histos] def linkerEnv(env: Map[String, String]): Map[String, String] =
    env.get("PATH").filter(_.nonEmpty) match
      case Some(path) => Map("PATH" -> path)
      case None       => Map.empty

  /** Compiles the bridge against this revision's tools, then links in a child
    * JVM.
    */
  private def invoke(
      clang: Clang,
      main: MainClass,
      classpath: List[Path],
      work: Path,
      jdk: ResolvedJdk,
      toolchain: NativeToolchain,
      env: Map[String, String]
  ): IO[NativeError, Path] =
    val bridgeRoot = work.resolve("bridge").resolve(toolchain.revision.value)
    val classes = bridgeRoot.resolve("classes")
    val source = bridgeRoot
      .resolve("src")
      .resolve("subprocess")
      .resolve("NativeBridge.scala")
    val child = work.resolve("child")
    for
      jars <- NativeClasspath
        .bridgeJars(toolchain)
        .mapError(NativeError.Fetch(_))
      _ <- write(
        source,
        NativeBridge.source(
          "hello",
          main.binary,
          clang.clang,
          clang.clangpp,
          classpath
        )
      )
      _ <- JvmCompile
        .compile(
          CompileRequest(
            CompileToolchain(
              JvmCompile.Scala390,
              Platform.Jvm,
              jdk,
              ScalacFlags.empty
            ),
            List(source),
            jars,
            classes,
            bridgeRoot.resolve("analysis"),
            Nil
          )
        )
        .mapError(NativeError.Compile(_))
      _ <- emptyDir(child)
      bridge <- ZIO
        .fromEither(MainClass.from(NativeBridge.MainName))
        .mapError(error => NativeError.Link(LinkError.Linker(error.message)))
      binary <- ChildProcess
        .run(
          ChildRequest(
            Platform.Jvm,
            jdk.home.resolve("bin").resolve("java"),
            ChildClasspath(classes, jars),
            bridge,
            child,
            linkerEnv(env)
          )
        )
        .mapError(error => NativeError.Link(LinkError.Linker(error.message)))
      linked <- binary.exit.value match
        case 0 => readBinary(child.resolve("binary.path"))
        case _ =>
          ZIO.fail(
            NativeError.Link(
              LinkError.Linker(shown(binary.out.text, binary.err.text))
            )
          )
    yield linked

  private def shown(out: String, err: String): String =
    val text = List(out, err).filter(_.nonEmpty).mkString("\n")
    if text.isEmpty then "the native linker failed" else text

  private def readBinary(path: Path): IO[NativeError, Path] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then
          Right(Path.of(Files.readString(path).trim))
        else
          Left(
            NativeError.Link(
              LinkError.Linker("the native bridge did not write a binary")
            )
          )
      }
      .mapError(_ =>
        NativeError.Link(LinkError.Linker(s"could not read $path"))
      )
      .flatMap(ZIO.fromEither)

  private def write(path: Path, text: String): IO[NativeError, Unit] =
    Workspace
      .writeString(path, text)
      .mapError(_ =>
        NativeError.Link(LinkError.Linker(s"could not write $path"))
      )

  private def emptyDir(path: Path): IO[NativeError, Unit] =
    Workspace
      .recreate(path)
      .mapError(_ =>
        NativeError.Link(LinkError.Linker(s"could not prepare $path"))
      )
      .unit
