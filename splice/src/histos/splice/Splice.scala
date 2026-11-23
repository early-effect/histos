package histos.splice

import zio.*

import java.io.File
import java.nio.file.{Files, Path}

/** Effectful splice core. The sbt AutoPlugin is a thin wrapper around this program. */
object Splice:

  val maven: SpliceResolver    = SpliceResolver.Maven
  val jsDelivr: SpliceResolver = SpliceResolver.Cdn(
    "jsdelivr",
    (n, v, p) => s"https://cdn.jsdelivr.net/npm/$n@$v/${p.stripPrefix("/")}",
  )
  val unpkg: SpliceResolver = SpliceResolver.Cdn(
    "unpkg",
    (n, v, p) => s"https://unpkg.com/$n@$v/${p.stripPrefix("/")}",
  )

  val github: SpliceResolver = SpliceResolver.GitHub { (owner, repo, tag) =>
    s"https://github.com/$owner/$repo/archive/refs/tags/$tag.tar.gz"
  }

  def githubArchive(expand: (String, String, String) => String): SpliceResolver =
    SpliceResolver.GitHub(expand)

  def cdn(id: String)(expand: (String, String, String) => String): SpliceResolver =
    SpliceResolver.Cdn(id, expand)

  def github(specifier: String, repository: String, tag: String, path: String): SpliceLib.GitHub =
    SpliceLib.GitHub(specifier, repository, tag, path, None)

  def file(specifier: String, file: File): SpliceLib.File =
    SpliceLib.File(specifier, file)

  def lib(name: String, version: String, path: String): SpliceLib.Cdn =
    SpliceLib.Cdn(name, name, version, path, None)

  def webjar(name: String, version: String, path: String): SpliceLib.WebJar =
    SpliceLib.WebJar(name, "org.webjars.npm", name, version, path)

  def webjar(
      specifier: String,
      organization: String,
      name: String,
      version: String,
      path: String,
  ): SpliceLib.WebJar =
    SpliceLib.WebJar(specifier, organization, name, version, path)

  def resolve(libs: Seq[SpliceLib], env: ResolveEnv): IO[SpliceError, Map[String, Path]] =
    Resolve.files(libs, env)

  /** Bundle the linker's imports and stop. `NODE_ENV` is `development`. Source maps default on. */
  def fast(
      linker: Path,
      output: Path,
      libs: Map[String, Path] = Map.empty,
      sourceMaps: Boolean = true,
  ): IO[SpliceError, Path] =
    linked(linker, output, libs, Minify.None, sourceMaps)

  /** Bundle and minify. `NODE_ENV` is `production`. Source maps default off. */
  def full(
      linker: Path,
      output: Path,
      libs: Map[String, Path] = Map.empty,
      sourceMaps: Boolean = false,
  ): IO[SpliceError, Path] =
    linked(linker, output, libs, Minify.Esbuild, sourceMaps)

  /** Closure advanced on the linker output. Mapped libraries are not shaken. */
  def closure(
      linker: Path,
      output: Path,
      libs: Map[String, Path] = Map.empty,
      sourceMaps: Boolean = false,
  ): IO[SpliceError, Path] =
    linked(linker, output, libs, Minify.Closure, sourceMaps)

  private def linked(
      linker: Path,
      output: Path,
      libs: Map[String, Path],
      minify: Minify,
      sourceMaps: Boolean,
  ): IO[SpliceError, Path] =
    ZIO
      .attemptBlocking(Files.readString(linker))
      .mapError(error => SpliceError.Io(Option(error.getMessage).getOrElse(linker.toString)))
      .flatMap: text =>
        run(
          SpliceInput(
            linker = List(LinkerFile(label = "main.js", contents = text, origin = Some(linker))),
            libs = libs,
            output = output,
            minify = minify,
            sourceMaps = sourceMaps,
          )
        )

  def run(input: SpliceInput): IO[SpliceError, Path] =
    if bundlesLinker(input) then bundleProgram(input)
    else scriptOrClosure(input)

  /** `spliceFast` and `spliceFull` with at least one mapped library. esbuild bundles the linker's own imports. */
  private def bundlesLinker(input: SpliceInput): Boolean =
    input.libs.nonEmpty && (input.minify == Minify.None || input.minify == Minify.Esbuild)

  private def bundleProgram(input: SpliceInput): IO[SpliceError, Path] =
    for
      _   <- checkLibFiles(input.libs)
      _   <- oneLinkerFile(input.linker)
      hit <- bundleHit(input)
      _   <- ZIO.unless(hit)(bundleBuild(input))
    yield input.output

  private def oneLinkerFile(files: List[LinkerFile]): IO[SpliceError, Unit] =
    files match
      case Nil | _ :: Nil => ZIO.unit
      case many           => ZIO.fail(SpliceError.SeveralModules(many.map(_.label)))

  /** Stamp of the previous run's durable inputs, checked before esbuild. A missing sidecar is a miss. An empty one
    * means esbuild read nothing that is still on disk.
    */
  private def bundleHit(input: SpliceInput): IO[SpliceError, Boolean] =
    input.cache match
      case None        => ZIO.succeed(false)
      case Some(stamp) =>
        ZIO
          .attemptBlocking {
            recordedInputs(stamp) match
              case Some(paths) if paths.forall(p => Files.isRegularFile(p)) =>
                Closure.cacheHit(stamp, bundleDigestOf(input, paths), input.output, mapPath(input))
              case _ => false
          }
          .mapError(e => SpliceError.Io(s"could not read $stamp: ${e.getMessage}"))

  private def bundleBuild(input: SpliceInput): IO[SpliceError, Unit] =
    val file  = input.linker.headOption
    val entry = EsbuildNative.Entry(
      js = file.fold("")(_.contents),
      path = file.flatMap(_.origin),
      label = file.fold("main.js")(_.label),
      aliases = input.libs,
      env = NodeEnv.of(input.minify),
      minify = input.minify == Minify.Esbuild,
      sourceMapAt = mapPath(input),
    )
    for
      built <- EsbuildNative.bundleEntry(entry, input.cacheDir, input.localOnly)
      durable = built.inputs.filter(p => Files.isRegularFile(p))
      _ <- write(input.output, finishJs(built.js, built.sourceMap, input))
      _ <- ZIO.foreachDiscard(built.sourceMap)(write(SourceMaps.mapPath(input.output), _))
      _ <- rememberBundle(input, durable)
    yield ()

  private def bundleDigestOf(input: SpliceInput, inputs: List[Path]): String =
    EsbuildNative.bundleDigest(
      linker = linkerText(input),
      aliases = input.libs,
      inputs = inputs,
      output = input.output,
      sourceMaps = input.sourceMaps,
      minify = input.minify == Minify.Esbuild,
      env = NodeEnv.of(input.minify),
    )

  private def linkerText(input: SpliceInput): String =
    input.linker.map(_.contents).mkString("\n")

  private def inputsSidecar(stamp: Path): Path =
    stamp.resolveSibling(stamp.getFileName.toString + ".inputs")

  private def recordedInputs(stamp: Path): Option[List[Path]] =
    val side = inputsSidecar(stamp)
    if !Files.isRegularFile(side) then None
    else
      Some(
        Files
          .readString(side)
          .linesIterator
          .map(_.trim)
          .filter(_.nonEmpty)
          .map(Path.of(_))
          .toList
      )

  private def rememberBundle(input: SpliceInput, inputs: List[Path]): IO[SpliceError, Unit] =
    input.cache match
      case None        => ZIO.unit
      case Some(stamp) =>
        ZIO
          .attemptBlocking {
            Closure.storeCache(stamp, bundleDigestOf(input, inputs))
            val side = inputsSidecar(stamp)
            Option(side.getParent).foreach(Files.createDirectories(_))
            val body = inputs.map(_.toAbsolutePath.normalize.toString).sorted.mkString("\n")
            Files.writeString(side, if body.isEmpty then "" else body + "\n")
            ()
          }
          .mapError(e => SpliceError.Io(s"could not write $stamp: ${e.getMessage}"))

  /** Empty `spliceLibs`, and `spliceClosure`. The primary path does not scan linker text; this one still does. */
  private def scriptOrClosure(input: SpliceInput): IO[SpliceError, Path] =
    for
      _         <- checkLibFiles(input.libs)
      _         <- ZIO.foreachDiscard(input.linker)(file => unresolvedIn(file.contents, file.label, input.libs))
      libraries <- bundled(input)
      linker = linkerOf(input)
      _ <- leftover(linker, input.libs.keys, input.output)
      digest = digestOf(input, libraries, linker)
      hit <- cached(input, digest)
      _   <- ZIO.unless(hit)(build(input, libraries, linker) *> remember(input, digest))
    yield input.output

  /** Every mapped library, bundled by esbuild into one script that binds each as its `__splice_<id>`. */
  private def bundled(input: SpliceInput): IO[SpliceError, String] =
    val libs = input.libs.toList.sortBy(_._1)
    if libs.isEmpty then ZIO.succeed("")
    else
      val entry    = libs.map((spec, path) => s"export * as ${JsModules.ident(spec)} from ${jsString(path)};")
      val bindings = libs.map((spec, _) =>
        val id = JsModules.ident(spec)
        s"const $id = ${EsbuildNative.Libraries}.$id;"
      )
      EsbuildNative
        .bundle(
          entry.mkString("\n"),
          libs.filter((spec, _) => JsModules.isBare(spec)).toMap,
          NodeEnv.of(input.minify),
          input.cacheDir,
          input.localOnly,
        )
        .map(bundle => bundle + bindings.mkString("", "\n", "\n"))

  /** The linker's output as one script, its exports dropped when a minifier will read it as a script. */
  private def linkerOf(input: SpliceInput): String =
    val js = input.linker.map(_.contents).mkString("\n")
    input.minify match
      case Minify.None                     => js
      case Minify.Esbuild | Minify.Closure => JsModules.dropExports(js)

  private def digestOf(input: SpliceInput, libraries: String, linker: String): Option[String] =
    input.minify match
      case Minify.None    => None
      case Minify.Esbuild => Some(EsbuildNative.programDigest(libraries, linker, input.output, input.sourceMaps))
      case Minify.Closure => Some(Closure.programDigest(libraries, linker, input.output, input.sourceMaps))

  /** Whether the output on disk was built from exactly these inputs. */
  private def cached(input: SpliceInput, digest: Option[String]): IO[SpliceError, Boolean] =
    (input.cache, digest) match
      case (Some(stamp), Some(d)) =>
        ZIO
          .attemptBlocking(Closure.cacheHit(stamp, d, input.output, mapPath(input)))
          .mapError(e => SpliceError.Io(s"could not read $stamp: ${e.getMessage}"))
      case _ => ZIO.succeed(false)

  private def remember(input: SpliceInput, digest: Option[String]): IO[SpliceError, Unit] =
    (input.cache, digest) match
      case (Some(stamp), Some(d)) =>
        ZIO
          .attemptBlocking(Closure.storeCache(stamp, d))
          .mapError(e => SpliceError.Io(s"could not write $stamp: ${e.getMessage}"))
      case _ => ZIO.unit

  private def build(input: SpliceInput, libraries: String, linker: String): IO[SpliceError, Unit] =
    for
      compiled <- input.minify match
        case Minify.None    => ZIO.succeed(Closure.Compiled(libraries + linker, fastMap(libraries, input)))
        case Minify.Esbuild =>
          EsbuildNative
            .optimize(libraries + linker, input.cacheDir, input.localOnly)
            .map(js => Closure.Compiled(js, None))
        case Minify.Closure =>
          // Closure reads only Scala.js's output; the library bundle goes ahead of it, and its bindings are externs.
          Closure.optimize(
            inputs = List("linker.js" -> linker),
            prefix = List("libraries.js" -> libraries),
            extraExterns = input.libs.keys.toList.sorted.map(JsModules.ident),
            sourceMaps = input.sourceMaps,
            sourceMapFile = input.output.getFileName.toString,
          )
      _ <- write(input.output, finishJs(compiled.js, compiled.sourceMap, input))
      _ <- ZIO.foreachDiscard(compiled.sourceMap)(write(SourceMaps.mapPath(input.output), _))
    yield ()

  private def mapPath(input: SpliceInput): Option[Path] =
    if input.sourceMaps then Some(SourceMaps.mapPath(input.output)) else None

  /** `path` as a JS string literal, in forward slashes, which esbuild takes on every host. */
  private def jsString(path: Path): String =
    val p = path.toAbsolutePath.normalize.toString.replace('\\', '/')
    "\"" + p.replace("\"", "\\\"") + "\""

  private def checkLibFiles(libs: Map[String, Path]): IO[SpliceError, Unit] =
    ZIO.foreachDiscard(libs.toList): (spec, path) =>
      ZIO.unless(Files.isRegularFile(path))(ZIO.fail(SpliceError.MissingFile(spec, path.toString))).unit

  private def finishJs(js: String, map: Option[String], input: SpliceInput): String =
    if map.isDefined then SourceMaps.annotate(js, SourceMaps.mapFileName(input.output))
    else js

  private def fastMap(libraries: String, input: SpliceInput): Option[String] =
    if !input.sourceMaps then None
    else
      val dest     = SourceMaps.mapPath(input.output)
      val sections = SourceMaps.sectionsFor(libraries, input.linker, dest)
      if sections.isEmpty then None
      else Some(SourceMaps.indexed(input.output.getFileName.toString, sections))

  private def leftover(body: String, mapped: Iterable[String], output: Path): IO[SpliceError, Unit] =
    JsModules.leftoverBare(body, mapped) match
      case Nil       => ZIO.unit
      case spec :: _ => ZIO.fail(SpliceError.LeftoverSpecifier(spec, output.toString))

  private def unresolvedIn(js: String, referring: String, libs: Map[String, Path]): IO[SpliceError, Unit] =
    JsModules.bareRefs(js, referring).find((spec, _) => !libs.contains(spec)) match
      case Some((spec, file)) => ZIO.fail(SpliceError.Unresolved(spec, file))
      case None               => ZIO.unit

  private def write(path: Path, body: String): IO[SpliceError, Unit] =
    ZIO
      .attempt {
        Option(path.getParent).foreach(Files.createDirectories(_))
        Files.writeString(path, body)
        ()
      }
      .mapError(e => SpliceError.Io(s"could not write $path: ${e.getMessage}"))
end Splice
