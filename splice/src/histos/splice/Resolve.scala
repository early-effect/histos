package histos.splice

import coursier.cache.{CachePolicy, FileCache}
import coursier.util.Artifact
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import zio.*

import java.io.BufferedInputStream
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import java.util.zip.ZipFile
import scala.concurrent.ExecutionContext
import scala.util.Using

/** Turn spliceLibs coordinates into files. CDN goes through Coursier FileCache. */
object Resolve:

  def files(libs: Seq[SpliceLib], env: ResolveEnv): IO[SpliceError, Map[String, Path]] =
    val dups = libs.groupBy(_.specifier).collect { case (s, xs) if xs.sizeIs > 1 => s }
    dups.headOption match
      case Some(s) => ZIO.fail(SpliceError.DuplicateLib(s))
      case None    =>
        ZIO.foreach(libs)(lib => one(lib, env).map(lib.specifier -> _)).map(_.toMap)

  private def one(lib: SpliceLib, env: ResolveEnv): IO[SpliceError, Path] =
    lib match
      case f: SpliceLib.File =>
        val path = f.file.toPath
        ZIO.unless(Files.isRegularFile(path))(ZIO.fail(SpliceError.MissingFile(f.specifier, path.toString))).as(path)
      case c: SpliceLib.Cdn    => cdn(c, env)
      case w: SpliceLib.WebJar => webjar(w, env)
      case g: SpliceLib.GitHub => github(g, env)

  private def cdn(lib: SpliceLib.Cdn, env: ResolveEnv): IO[SpliceError, Path] =
    val pin = lib.sha256.map(_.trim.toLowerCase).filter(_.nonEmpty)
    pin match
      case None           => ZIO.fail(SpliceError.MissingSha256(lib.specifier))
      case Some(expected) =>
        val cdns = env.resolvers.collect { case c: SpliceResolver.Cdn => c }
        if cdns.isEmpty then ZIO.fail(SpliceError.NoResolver(lib.specifier, "cdn"))
        else fetchFirst(lib, expected, cdns.toList, env)

  private def fetchFirst(
      lib: SpliceLib.Cdn,
      expected: String,
      cdns: List[SpliceResolver.Cdn],
      env: ResolveEnv,
  ): IO[SpliceError, Path] =
    cdns match
      case Nil =>
        ZIO.fail(
          SpliceError.NotFound(
            lib.specifier,
            s"${lib.name}@${lib.version}/${lib.path}",
          )
        )
      case c :: rest =>
        val url = c.expand(lib.name, lib.version, lib.path.stripPrefix("/"))
        fetch(url, env).foldZIO(
          {
            case SpliceError.NotFound(_, _) if rest.nonEmpty =>
              fetchFirst(lib, expected, rest, env)
            case other => ZIO.fail(other)
          },
          path =>
            val actual = sha256(path)
            if actual == expected then ZIO.succeed(path)
            else ZIO.fail(SpliceError.ChecksumMismatch(lib.specifier, expected, actual)),
        )

  private[splice] def fetchCached(url: String, env: ResolveEnv): IO[SpliceError, Path] =
    fetch(url, env)

  private def fetch(url: String, env: ResolveEnv): IO[SpliceError, Path] =
    ZIO
      .attemptBlocking {
        given ExecutionContext = ExecutionContext.global
        val policies           =
          if env.localOnly then Seq(CachePolicy.LocalOnly)
          else Seq(CachePolicy.FetchMissing)
        val cache = FileCache().copy(location = env.cacheDir.toFile, checksums = Seq(None), cachePolicies = policies)
        cache.file(Artifact(url)).run.unsafeRun()
      }
      .mapError(e => SpliceError.Io(s"coursier fetch $url: ${e.getMessage}"))
      .flatMap {
        case Left(err) => ZIO.fail(SpliceError.NotFound(url, err.describe))
        case Right(f)  => ZIO.succeed(f.toPath)
      }

  private def webjar(lib: SpliceLib.WebJar, env: ResolveEnv): IO[SpliceError, Path] =
    val mavenOn = env.resolvers.exists {
      case SpliceResolver.Maven => true
      case _                    => false
    }
    if !mavenOn then ZIO.fail(SpliceError.NoResolver(lib.specifier, "maven"))
    else
      env.webjars.get(lib.specifier) match
        case None =>
          ZIO.fail(
            SpliceError.NotFound(
              lib.specifier,
              s"${lib.organization}:${lib.name}:${lib.version}",
            )
          )
        case Some(jar) => extract(lib, jar, env.extractDir)

  private def extract(lib: SpliceLib.WebJar, jar: Path, destDir: Path): IO[SpliceError, Path] =
    val entry = s"META-INF/resources/webjars/${lib.name}/${lib.version}/${lib.path.stripPrefix("/")}"
    if entry.contains("..") then ZIO.fail(SpliceError.Io(s"illegal webjar path ${lib.path}"))
    else
      ZIO
        .attemptBlocking {
          Using.resource(new ZipFile(jar.toFile)): zf =>
            Option(zf.getEntry(entry)) match
              case None    => Left(SpliceError.MissingJarPath(lib.specifier, jar.toString, entry))
              case Some(e) =>
                val dest = destDir.resolve(s"${JsModules.ident(lib.specifier)}.js")
                Files.createDirectories(dest.getParent)
                Using.resource(zf.getInputStream(e)): in =>
                  Files.copy(in, dest, StandardCopyOption.REPLACE_EXISTING)
                Right(dest)
        }
        .mapError(e => SpliceError.Io(s"could not read ${jar}: ${e.getMessage}"))
        .flatMap {
          case Left(err) => ZIO.fail(err)
          case Right(p)  => ZIO.succeed(p)
        }

  private def github(lib: SpliceLib.GitHub, env: ResolveEnv): IO[SpliceError, Path] =
    val pin = lib.sha256.map(_.trim.toLowerCase).filter(_.nonEmpty)
    pin match
      case None           => ZIO.fail(SpliceError.MissingSha256(lib.specifier))
      case Some(expected) =>
        val origins = env.resolvers.collect { case g: SpliceResolver.GitHub => g }
        if origins.isEmpty then ZIO.fail(SpliceError.NoResolver(lib.specifier, "github"))
        else
          parseRepo(lib.repository) match
            case Left(err)            => ZIO.fail(err)
            case Right((owner, repo)) =>
              if !validTag(lib.tag) then ZIO.fail(SpliceError.Io(s"""GitHub tag must be exact, got "${lib.tag}""""))
              else if illegalPath(lib.path) then ZIO.fail(SpliceError.Io(s"illegal archive path ${lib.path}"))
              else fetchGithub(lib, owner, repo, expected, origins.toList, env)

  private def fetchGithub(
      lib: SpliceLib.GitHub,
      owner: String,
      repo: String,
      expected: String,
      origins: List[SpliceResolver.GitHub],
      env: ResolveEnv,
  ): IO[SpliceError, Path] =
    origins match
      case Nil =>
        ZIO.fail(SpliceError.NotFound(lib.specifier, s"${lib.repository}@${lib.tag}/${lib.path}"))
      case origin :: rest =>
        fetchGithubTags(lib, origin, owner, repo, expected, env).foldZIO(
          {
            case SpliceError.NotFound(_, _) if rest.nonEmpty =>
              fetchGithub(lib, owner, repo, expected, rest, env)
            case other => ZIO.fail(other)
          },
          archive => extractTarGz(lib, archive, env.extractDir),
        )

  private def fetchGithubTags(
      lib: SpliceLib.GitHub,
      origin: SpliceResolver.GitHub,
      owner: String,
      repo: String,
      expected: String,
      env: ResolveEnv,
  ): IO[SpliceError, Path] =
    tagCandidates(lib.tag) match
      case Nil  => ZIO.fail(SpliceError.NotFound(lib.specifier, lib.tag))
      case tags =>
        def loop(rest: List[String]): IO[SpliceError, Path] =
          rest match
            case Nil =>
              ZIO.fail(SpliceError.NotFound(lib.specifier, s"${lib.repository}@${lib.tag}"))
            case tag :: more =>
              fetch(origin.expand(owner, repo, tag), env).foldZIO(
                {
                  case SpliceError.NotFound(_, _) if more.nonEmpty => loop(more)
                  case other                                       => ZIO.fail(other)
                },
                path =>
                  val actual = sha256(path)
                  if actual == expected then ZIO.succeed(path)
                  else ZIO.fail(SpliceError.ChecksumMismatch(lib.specifier, expected, actual)),
              )
        loop(tags)

  private def extractTarGz(lib: SpliceLib.GitHub, archive: Path, destDir: Path): IO[SpliceError, Path] =
    val want = lib.path.stripPrefix("/")
    ZIO
      .attemptBlocking {
        Using.resource(
          new TarArchiveInputStream(
            new GzipCompressorInputStream(new BufferedInputStream(Files.newInputStream(archive)))
          )
        ): tar =>
          var found: Option[Path] = None
          var entry               = tar.getNextEntry
          while entry != null && found.isEmpty do
            if !entry.isDirectory then
              insideRoot(entry.getName) match
                case Some(n) if n == want =>
                  val dest = destDir.resolve(s"${JsModules.ident(lib.specifier)}.js")
                  Files.createDirectories(dest.getParent)
                  Files.copy(tar, dest, StandardCopyOption.REPLACE_EXISTING)
                  found = Some(dest)
                case _ => ()
            entry = tar.getNextEntry
          found.toRight(SpliceError.MissingArchivePath(lib.specifier, archive.toString, want))
      }
      .mapError(e => SpliceError.Io(s"could not read ${archive}: ${e.getMessage}"))
      .flatMap {
        case Left(err) => ZIO.fail(err)
        case Right(p)  => ZIO.succeed(p)
      }

  private def parseRepo(repository: String): Either[SpliceError, (String, String)] =
    repository.split("/", -1) match
      case Array(owner, repo) if owner.nonEmpty && repo.nonEmpty && !owner.contains("..") && !repo.contains("..") =>
        Right((owner, repo))
      case _ =>
        Left(SpliceError.Io(s"""GitHub repository must be owner/repo, got "$repository""""))

  private def validTag(tag: String): Boolean =
    tag.nonEmpty && !tag.contains("/") && !tag.contains("..")

  private def tagCandidates(tag: String): List[String] =
    val alt =
      if tag.startsWith("v") && tag.length > 1 then tag.substring(1)
      else s"v$tag"
    if alt == tag then List(tag) else List(tag, alt)

  private def illegalPath(path: String): Boolean =
    val n = path.stripPrefix("/")
    n.isEmpty || n.contains("..")

  /** GitHub tag archives have one root dir `{repo}-{tag}`; strip it. */
  private def insideRoot(name: String): Option[String] =
    val n = name.stripPrefix("./").replace('\\', '/')
    n.indexOf('/') match
      case -1 => None
      case i  =>
        val rest = n.substring(i + 1)
        if rest.isEmpty || rest.contains("..") then None else Some(rest)

  def sha256(path: Path): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(Files.readAllBytes(path))
      .map("%02x".format(_))
      .mkString
end Resolve
