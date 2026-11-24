package histos.splice

import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import zio.*

import java.io.BufferedInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.security.MessageDigest
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.matching.Regex

/** Pinned native esbuild. `spliceFast` and `spliceFull` bundle the linker's own imports. `spliceClosure` still bundles
  * each mapped library as a `__splice_*` namespace. Fetched through Coursier the first time a build needs it, never
  * committed.
  */
object EsbuildNative:

  val Version: String = "0.28.2"

  /** The global the library bundle is, whose members are the mapped libraries' namespaces. */
  private[splice] val Libraries: String = "__splice_libs"

  final case class Pin(id: String, url: String, member: String, sha256: String, windows: Boolean)

  def currentPin: Either[SpliceError, Pin] =
    pinFor(sys.props.getOrElse("os.name", ""), sys.props.getOrElse("os.arch", ""))

  def pinFor(osName: String, osArch: String): Either[SpliceError, Pin] =
    val os   = osName.toLowerCase
    val arch = osArch.toLowerCase
    val id   =
      if os.contains("mac") || os.contains("darwin") then
        if arch.contains("aarch64") || arch.contains("arm64") then Some("darwin-arm64")
        else if arch.contains("amd64") || arch.contains("x86_64") then Some("darwin-x64")
        else None
      else if os.contains("linux") then
        if arch.contains("aarch64") || arch.contains("arm64") then Some("linux-arm64")
        else if arch.contains("amd64") || arch.contains("x86_64") then Some("linux-x64")
        else None
      else if os.contains("win") then
        if arch.contains("aarch64") || arch.contains("arm64") then Some("win32-arm64")
        else if arch.contains("amd64") || arch.contains("x86_64") then Some("win32-x64")
        else None
      else None
    id.flatMap(pins.get)
      .toRight(
        SpliceError.Minify(
          s"spliceFull needs a pinned esbuild for this host (got os.name=$osName os.arch=$osArch). Supported: ${pins.keys.toList.sorted.mkString(", ")}"
        )
      )

  private[histos] final case class Started(temporary: Path, pid: Long)

  def optimize(js: String, cacheDir: Path, localOnly: Boolean): IO[SpliceError, String] =
    optimizeObserved[SpliceError](js, cacheDir, localOnly)(_ => ZIO.unit)

  private[histos] def optimizeObserved[E](js: String, cacheDir: Path, localOnly: Boolean)(
      onStarted: Started => IO[E, Unit]
  ): IO[SpliceError | E, String] =
    for
      pin     <- ZIO.fromEither(currentPin)
      bin     <- binary(pin, cacheDir, localOnly)
      outcome <- execObserved(bin, js, List("--minify", "--target=es2015"), None, None, false)(onStarted)
      out     <- outcome match
        case Outcome.Wrote(out, _, _)    => ZIO.succeed(out)
        case Outcome.Failed(code, error) => ZIO.fail(SpliceError.Minify(s"esbuild exit $code\n$error"))
    yield out

  /** Bundles `entry` into one script that defines [[Libraries]]. Each bare specifier in `aliases` resolves to its file,
    * so a library that imports another mapped library gets the same module. `process.env.NODE_ENV` reads as `env`.
    */
  def bundle(
      entry: String,
      aliases: Map[String, Path],
      env: NodeEnv,
      cacheDir: Path,
      localOnly: Boolean,
  ): IO[SpliceError, String] =
    val args = List(
      "--bundle",
      "--format=iife",
      s"--global-name=$Libraries",
      "--platform=browser",
      "--log-level=warning",
      "--color=false",
      "--log-override:empty-import-meta=error",
      s"""--define:process.env.NODE_ENV="${env.value}"""",
    ) ++ aliasArgs(aliases)
    for
      pin     <- ZIO.fromEither(currentPin)
      bin     <- binary(pin, cacheDir, localOnly)
      outcome <- exec(bin, entry, args)
      out     <- outcome match
        case Outcome.Wrote(out, _, _) => ZIO.succeed(out)
        case Outcome.Failed(_, what)  => ZIO.fail(bundleError(what))
    yield out

  /** One linker file esbuild bundles into a classic script. `sourceMapAt` is where the map will be written, so its
    * `sources` are relative to that file. `inputs` are the files esbuild read, absolute.
    */
  private[splice] final case class Entry(
      js: String,
      path: Option[Path],
      label: String,
      aliases: Map[String, Path],
      env: NodeEnv,
      minify: Boolean,
      sourceMapAt: Option[Path],
  )

  private[splice] final case class Bundled(js: String, sourceMap: Option[String], inputs: List[Path])

  def bundleEntry(entry: Entry, cacheDir: Path, localOnly: Boolean): IO[SpliceError, Bundled] =
    val args =
      List(
        "--bundle",
        "--format=iife",
        "--platform=browser",
        "--log-level=warning",
        "--color=false",
        "--log-override:empty-import-meta=error",
        s"""--define:process.env.NODE_ENV="${entry.env.value}"""",
      ) ++ (if entry.minify then List("--minify", "--target=es2015") else Nil)
        ++ aliasArgs(entry.aliases)
    val reported = entry.path.fold("in.js")(_.getFileName.toString)
    for
      pin     <- ZIO.fromEither(currentPin)
      bin     <- binary(pin, cacheDir, localOnly)
      outcome <- exec(bin, entry.js, args, entry.path, entry.sourceMapAt, metafile = true)
      out     <- outcome match
        case Outcome.Wrote(js, map, inputs) => ZIO.succeed(Bundled(js, map, inputs))
        case Outcome.Failed(_, what)        => ZIO.fail(bundleError(what, reported, entry.label))
    yield out

  /** The first error esbuild reported: an import it could not resolve, or anything else, with the file it named. An
    * error in the generated entry is reported as `entryLabel`.
    */
  private[splice] def bundleError(
      stderr: String,
      entryName: String = "in.js",
      entryLabel: String = "in.js",
  ): SpliceError =
    reports(stderr).headOption match
      case None                   => SpliceError.Bundle(None, stderr.trim)
      case Some(Report(text, at)) =>
        val file = at.map(name => if name == entryName then entryLabel else name)
        (text, file) match
          case (couldNotResolve(spec), Some(where)) => SpliceError.Unresolved(spec, where)
          case _                                    => SpliceError.Bundle(file, text)

  /** One `[ERROR]` esbuild printed, and the file its location names. */
  private final case class Report(text: String, file: Option[String])

  private val errorMark: Regex       = """(?m)^(?:✘|X) \[ERROR\] """.r
  private val location: Regex        = """\s+(.+?):\d+:\d+:\s*""".r
  private val couldNotResolve: Regex = """Could not resolve "([^"]+)".*""".r

  private val SourceMappingComment: Regex =
    """(?m)^[ \t]*//[@#][ \t]+sourceMappingURL=\S+[ \t]*\r?\n?""".r

  private def reports(stderr: String): List[Report] =
    errorMark.split(stderr).toList.drop(1).map { block =>
      val lines = block.linesIterator.toList
      val file  = lines.drop(1).collectFirst { case location(path) => Path.of(path).getFileName.toString }
      Report(lines.headOption.getOrElse("").trim, file)
    }

  /** Hashes what the minifier is given, so a change anywhere a library reaches rebuilds. */
  def programDigest(libraries: String, linker: String, output: Path, sourceMaps: Boolean): String =
    val md                   = MessageDigest.getInstance("SHA-256")
    def add(s: String): Unit =
      md.update(s.getBytes(StandardCharsets.UTF_8))
    add("esbuild-native")
    add(Version)
    add(currentPin.map(_.id).getOrElse("unknown"))
    add(currentPin.map(_.sha256).getOrElse(""))
    add(output.toAbsolutePath.normalize.toString)
    add("maps:" + sourceMaps)
    add(libraries)
    add(linker)
    md.digest.map("%02x".format(_)).mkString

  /** Hashes the linker text and every file esbuild read, so a transitive edit misses the stamp. */
  def bundleDigest(
      linker: String,
      aliases: Map[String, Path],
      inputs: List[Path],
      output: Path,
      sourceMaps: Boolean,
      minify: Boolean,
      env: NodeEnv,
  ): String =
    val md                   = MessageDigest.getInstance("SHA-256")
    def add(s: String): Unit =
      md.update(s.getBytes(StandardCharsets.UTF_8))
    add("bundle-entry")
    add(Version)
    add(currentPin.map(_.id).getOrElse("unknown"))
    add(currentPin.map(_.sha256).getOrElse(""))
    add(output.toAbsolutePath.normalize.toString)
    add("maps:" + sourceMaps)
    add("minify:" + minify)
    add(env.value)
    add(linker)
    aliases.toList.sortBy(_._1).foreach { (spec, path) =>
      add(spec)
      add(path.toAbsolutePath.normalize.toString)
    }
    inputs.map(_.toAbsolutePath.normalize).sortBy(_.toString).foreach { path =>
      add(path.toString)
      add(Resolve.sha256(path))
    }
    md.digest.map("%02x".format(_)).mkString

  private def aliasArgs(aliases: Map[String, Path]): List[String] =
    aliases.toList.sortBy(_._1).map { (spec, path) =>
      s"--alias:$spec=${path.toAbsolutePath.normalize}"
    }

  private val pins: Map[String, Pin] =
    def tgz(id: String, member: String, sha: String, windows: Boolean) =
      id -> Pin(
        id = id,
        url = s"https://registry.npmjs.org/@esbuild/$id/-/$id-$Version.tgz",
        member = member,
        sha256 = sha,
        windows = windows,
      )
    Map(
      tgz(
        "darwin-arm64",
        "package/bin/esbuild",
        "10b6243df618d374bb2d5c9cfbe7052e1405f6aa4e53a6164f11a91b9f2e1384",
        false,
      ),
      tgz(
        "darwin-x64",
        "package/bin/esbuild",
        "7fe5c9c905fff0a05d92db98929434ab4d3b6dd92d7a7688922db74380c75df3",
        false,
      ),
      tgz(
        "linux-arm64",
        "package/bin/esbuild",
        "90bd269553d258e80b19e3ccab4f98f0831f87442a2f056510ce24fd1b425fc2",
        false,
      ),
      tgz(
        "linux-x64",
        "package/bin/esbuild",
        "e1698a3d5c6c0798fee4fd3b5cc816651f460c63d390a7a26ea4beb0b1884100",
        false,
      ),
      tgz(
        "win32-arm64",
        "package/esbuild.exe",
        "530fb3933af14eefe85dfd06502f826e6ef60fdf4e75228ea67c696db312ecb1",
        true,
      ),
      tgz("win32-x64", "package/esbuild.exe", "c7bee37877d0aa6a046e52783fa0a2cf1a9ce5579d68bb3083bda99d4bff18ef", true),
    )

  /** One lock per destination. Parallel tests and parallel sbt tasks install the same pin, and a second `REPLACE`
    * unlinks the binary while another fiber is hashing it or starting the process.
    */
  private val publishLocks = new ConcurrentHashMap[String, AnyRef]

  private def binary(pin: Pin, cacheDir: Path, localOnly: Boolean): IO[SpliceError, Path] =
    val dest = cacheDir.resolve("sbt-splice-esbuild").resolve(Version).resolve(pin.id).resolve(fileName(pin))
    val env  = ResolveEnv(Nil, cacheDir, localOnly, Map.empty, cacheDir)
    for
      hit <- ZIO.attemptBlocking(matches(dest, pin)).mapError(e => SpliceError.Io(e.getMessage))
      _   <- ZIO.unless(hit)(stage(pin, dest, env))
      _   <- verify(dest, pin)
    yield dest

  private def stage(pin: Pin, dest: Path, env: ResolveEnv): IO[SpliceError, Unit] =
    ZIO.acquireReleaseWith(
      ZIO.succeed(dest.resolveSibling(s"${fileName(pin)}.${UUID.randomUUID}.tmp"))
    )(tmp => ZIO.attemptBlocking(Files.deleteIfExists(tmp)).ignore) { tmp =>
      for
        tgz <- Resolve.fetchCached(pin.url, env)
        _   <- extractMember(tgz, pin.member, tmp)
        _   <- verify(tmp, pin)
        _   <- ZIO.unless(pin.windows)(ZIO.attemptBlocking(chmodX(tmp)).mapError(e => SpliceError.Io(e.getMessage)))
        _   <- publish(tmp, dest, pin)
      yield ()
    }

  /** Installs `tmp` as `dest`, unless `dest` already matches `pin`. The loser deletes `tmp` and leaves the live binary
    * in place.
    */
  private def publish(tmp: Path, dest: Path, pin: Pin): IO[SpliceError, Unit] =
    val lock = publishLocks.computeIfAbsent(dest.toAbsolutePath.normalize.toString, _ => new Object)
    ZIO
      .attemptBlocking {
        lock.synchronized {
          if matches(dest, pin) then Files.deleteIfExists(tmp)
          else
            Files.createDirectories(dest.getParent)
            try Files.move(tmp, dest, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
            catch
              case _: AtomicMoveNotSupportedException =>
                Files.move(tmp, dest, StandardCopyOption.REPLACE_EXISTING)
          ()
        }
      }
      .mapError(e => SpliceError.Io(e.getMessage))

  private def matches(dest: Path, pin: Pin): Boolean =
    Files.isRegularFile(dest) && Resolve.sha256(dest) == pin.sha256

  private def fileName(pin: Pin): String = if pin.windows then "esbuild.exe" else "esbuild"

  private def verify(dest: Path, pin: Pin): IO[SpliceError, Unit] =
    ZIO
      .attemptBlocking(Resolve.sha256(dest))
      .mapError(e => SpliceError.Io(e.getMessage))
      .flatMap { actual =>
        if actual == pin.sha256 then ZIO.unit
        else ZIO.fail(SpliceError.ChecksumMismatch(s"esbuild@${pin.id}-$Version", pin.sha256, actual))
      }

  private def extractMember(archive: Path, member: String, dest: Path): IO[SpliceError, Unit] =
    ZIO
      .attemptBlocking {
        Using.resource(
          new TarArchiveInputStream(
            new GzipCompressorInputStream(new BufferedInputStream(Files.newInputStream(archive)))
          )
        ): tar =>
          var found = false
          var entry = tar.getNextEntry
          while entry != null && !found do
            if entry.getName == member && !entry.isDirectory then
              Files.createDirectories(dest.getParent)
              Files.copy(tar, dest, StandardCopyOption.REPLACE_EXISTING)
              found = true
            entry = tar.getNextEntry
          if !found then Left(SpliceError.MissingArchivePath("esbuild", archive.toString, member))
          else Right(())
      }
      .mapError(e => SpliceError.Io(s"could not read $archive: ${e.getMessage}"))
      .flatMap {
        case Left(err) => ZIO.fail(err)
        case Right(_)  => ZIO.unit
      }

  private def chmodX(path: Path): Unit =
    val perms = Set(
      PosixFilePermission.OWNER_READ,
      PosixFilePermission.OWNER_WRITE,
      PosixFilePermission.OWNER_EXECUTE,
      PosixFilePermission.GROUP_READ,
      PosixFilePermission.GROUP_EXECUTE,
      PosixFilePermission.OTHERS_READ,
      PosixFilePermission.OTHERS_EXECUTE,
    ).asJava
    Files.setPosixFilePermissions(path, perms)
    ()

  /** What one esbuild run left. `inputs` are the files `--metafile` listed, absolute, including any temporary entry. */
  private enum Outcome:
    case Wrote(js: String, sourceMap: Option[String], inputs: List[Path])
    case Failed(code: Int, stderr: String)

  /** Runs esbuild in a directory of its own that is gone afterwards. `entryPath` is bundled in place; otherwise `js` is
    * written to `in.js`. `sourceMapAt` is the final map path, so `sources` are rewritten relative to it before the
    * temporary directory disappears.
    */
  private def exec(
      bin: Path,
      js: String,
      args: List[String],
      entryPath: Option[Path] = None,
      sourceMapAt: Option[Path] = None,
      metafile: Boolean = false,
  ): IO[SpliceError, Outcome] =
    execObserved[SpliceError](bin, js, args, entryPath, sourceMapAt, metafile)(_ => ZIO.unit)

  private def execObserved[E](
      bin: Path,
      js: String,
      args: List[String],
      entryPath: Option[Path],
      sourceMapAt: Option[Path],
      metafile: Boolean,
  )(onStarted: Started => IO[E, Unit]): IO[SpliceError | E, Outcome] =
    ZIO.scoped:
      val entryAbs = entryPath.map(_.toAbsolutePath.normalize)
      for
        dir <- ZIO.acquireRelease(attempt(Files.createTempDirectory("sbt-splice-esbuild-")))(dir =>
          ZIO.attemptBlocking(deleteRecursively(dir)).ignore
        )
        hidden <- ZIO.acquireRelease(attempt(entryAbs.flatMap(hideSourceMappingComment)))(hidden =>
          ZIO.attemptBlocking(restoreSourceMappingComment(entryAbs, hidden)).ignore
        )
        builder <- attempt {
          val entry = entryAbs match
            case Some(path) => path.toString
            case None       =>
              Files.writeString(dir.resolve("in.js"), js, StandardCharsets.UTF_8)
              "in.js"
          val flags = (if sourceMapAt.isDefined then List("--sourcemap=external") else Nil) ++
            (if metafile then List("--metafile=meta.json") else Nil)
          val builder =
            new ProcessBuilder((bin.toAbsolutePath.toString :: entry :: (args ++ flags)) :+ "--outfile=out.js"*)
          builder.directory(dir.toFile)
          builder.redirectInput(ProcessBuilder.Redirect.INHERIT)
          builder
        }
        outcome <- ZIO.scoped {
          for
            process <- ZIO.acquireRelease(attempt(builder.start()))(process =>
              ZIO.attemptBlockingInterrupt {
                process.destroyForcibly()
                process.waitFor()
                ()
              }.ignore
            )
            started <- attempt(Started(dir, process.pid()))
            _       <- onStarted(started)
            result  <- attempt(collected(process, dir, sourceMapAt, metafile))
          yield result
        }.exit
        _      <- attempt(restoreSourceMappingComment(entryAbs, hidden))
        result <- outcome
      yield result

  private def attempt[A](body: => A): IO[SpliceError, A] =
    ZIO
      .attemptBlockingInterrupt(body)
      .mapError(error => SpliceError.Io(s"esbuild: ${Option(error.getMessage).getOrElse(error.getClass.getName)}"))

  private def collected(process: Process, dir: Path, sourceMapAt: Option[Path], metafile: Boolean): Outcome =
    val out  = dir.resolve("out.js")
    val err  = String(process.getErrorStream.readAllBytes(), StandardCharsets.UTF_8)
    val code = process.waitFor()
    if code != 0 then Outcome.Failed(code, err)
    else if !Files.isRegularFile(out) then Outcome.Failed(code, s"esbuild wrote no out.js\n$err")
    else
      val written = Files.readString(out, StandardCharsets.UTF_8)
      val map     = sourceMapAt.map { dest =>
        SourceMaps.retarget(Files.readString(dir.resolve("out.js.map"), StandardCharsets.UTF_8), dir, dest)
      }
      val inputs =
        if metafile then
          JsonText.objectKeys(Files.readString(dir.resolve("meta.json"), StandardCharsets.UTF_8), "inputs").map { key =>
            val path = Path.of(key)
            if path.isAbsolute then path.normalize else dir.resolve(key).normalize
          }
        else Nil
      Outcome.Wrote(written, map, inputs)

  /** Text to put back when `path` carried a source-map comment. esbuild follows that comment and records its sources.
    * With the comment gone, the bundle map names this file.
    */
  private def hideSourceMappingComment(path: Path): Option[String] =
    val original = Files.readString(path, StandardCharsets.UTF_8)
    val stripped = SourceMappingComment.replaceAllIn(original, "")
    if stripped == original then None
    else
      Files.writeString(path, stripped, StandardCharsets.UTF_8)
      Some(original)

  private def restoreSourceMappingComment(path: Option[Path], hidden: Option[String]): Unit =
    (path, hidden) match
      case (Some(file), Some(original)) =>
        Files.writeString(file, original, StandardCharsets.UTF_8)
        ()
      case _ => ()

  private def deleteRecursively(path: Path): Unit =
    if Files.isDirectory(path) then
      val stream = Files.list(path)
      try stream.forEach(p => deleteRecursively(p))
      finally stream.close()
    Files.deleteIfExists(path)
    ()
end EsbuildNative
