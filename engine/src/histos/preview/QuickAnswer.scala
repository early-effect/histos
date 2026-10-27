package histos.preview

import histos.testrun.{QuickRecord, SuiteName, SuiteStamp}
import histos.zinc.GraphCompile
import histos.{Module, Platform, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, UIO, ZIO}

/** What a quick run would do with the classes already on disk. Preview does not launch a test framework and does not
  * write a record.
  *
  * The digest is the class files plus the module's scalac flags. A preview does not resolve jars or a JDK, so a record
  * written by a run that hashed those does not match.
  */
enum QuickAnswer:
  /** The test class directory is absent. No suite would be skipped. */
  case Uncompiled

  /** Suites named from `.class` files. `skip` is a stored digest that still matches. `run` would run: the digest
    * missed, there is no record, or the class is gone. `unknown` could not be read.
    */
  case Named(skip: List[String], run: List[String], unknown: List[String])

  /** Class files are present, but suites could not be named without a framework. `skip` are quick records whose digest
    * still matches. The rest are not known to be skippable.
    */
  case Partial(skip: List[String])

object QuickAnswer:
  /** The cover [[read]] recomputes. Tests that store a record use this same value. */
  private[histos] def cover(module: Module): SuiteStamp.Cover =
    SuiteStamp.Cover(Nil, Nil, "", module.flags.argv)

  def read(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
  ): UIO[QuickAnswer] =
    val classes = TestClasses(repo, module.name, version, platform)
    isDirectory(classes).flatMap:
      case false => ZIO.succeed(QuickAnswer.Uncompiled)
      case true  =>
        namesOf(classes).flatMap:
          case Seen.Failed => ZIO.succeed(QuickAnswer.Partial(Nil))
          case Seen.Empty  =>
            recordNames(repo, module.name, version, platform).map:
              case Nil    => QuickAnswer.Uncompiled
              case found  => QuickAnswer.Named(Nil, found.sorted, Nil)
          case Seen.Inner =>
            matchingRecords(repo, module, modules, version, platform, classes)
          case Seen.Names(found) =>
            judge(repo, module, modules, version, platform, classes, found)

  private def judge(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      classes: Path,
      names: List[String],
  ): UIO[QuickAnswer] =
    directories(repo, module, modules, version, platform, classes).flatMap: dirs =>
      SuiteStamp
        .digests(names, dirs, cover(module))
        .foldZIO(
          _ => ZIO.succeed(QuickAnswer.Partial(Nil)),
          answers => split(repo, module.name, version, platform, names.sorted, answers),
        )

  private def split(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      names: List[String],
      answers: Map[String, Option[histos.jdk.Sha256]],
  ): UIO[QuickAnswer] =
    ZIO
      .foldLeft(names)(Buckets.empty): (buckets, name) =>
        answers.get(name).flatten match
          case None =>
            ZIO.succeed(buckets.ran(name))
          case Some(digest) =>
            SuiteName.from(name) match
              case Left(_) =>
                ZIO.succeed(buckets.unread(name))
              case Right(suite) =>
                QuickRecord
                  .hit(repo, module, version, platform, suite, digest)
                  .foldZIO(
                    _ => ZIO.succeed(buckets.unread(name)),
                    hit =>
                      if hit then ZIO.succeed(buckets.skipped(name))
                      else ZIO.succeed(buckets.ran(name)),
                  )
      .map: buckets =>
        if buckets.skip.isEmpty && buckets.run.isEmpty && buckets.unknown.isEmpty then
          QuickAnswer.Uncompiled
        else QuickAnswer.Named(buckets.skip, buckets.run, buckets.unknown)

  private def matchingRecords(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      classes: Path,
  ): UIO[QuickAnswer] =
    recordNames(repo, module.name, version, platform).flatMap: names =>
      if names.isEmpty then ZIO.succeed(QuickAnswer.Partial(Nil))
      else
        directories(repo, module, modules, version, platform, classes).flatMap: dirs =>
          SuiteStamp
            .digests(names, dirs, cover(module))
            .foldZIO(
              _ => ZIO.succeed(QuickAnswer.Partial(Nil)),
              answers =>
                ZIO
                  .filter(names.sorted)(name => hit(repo, module.name, version, platform, name, answers))
                  .map(skip => QuickAnswer.Partial(skip)),
            )

  private def hit(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      name: String,
      answers: Map[String, Option[histos.jdk.Sha256]],
  ): UIO[Boolean] =
    answers.get(name).flatten match
      case None => ZIO.succeed(false)
      case Some(digest) =>
        SuiteName.from(name) match
          case Left(_) => ZIO.succeed(false)
          case Right(suite) =>
            QuickRecord
              .hit(repo, module, version, platform, suite, digest)
              .foldZIO(_ => ZIO.succeed(false), matched => ZIO.succeed(matched))

  private def directories(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      classes: Path,
  ): UIO[List[Path]] =
    val upstream = Module.chain(module, modules, platform).map: dependency =>
      GraphCompile.classesDir(repo, dependency.name, version, platform)
    val candidates =
      classes :: GraphCompile.classesDir(repo, module.name, version, platform) :: upstream
    ZIO.filter(candidates)(isDirectory).map(_.distinct)

  private def recordNames(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): UIO[List[String]] =
    val base = records(repo, module, version, platform)
    isDirectory(base).flatMap:
      case false => ZIO.succeed(Nil)
      case true  =>
        namesOf(base).map:
          case Seen.Names(found) => found
          case Seen.Inner        => Nil
          case Seen.Empty        => Nil
          case Seen.Failed       => Nil

  /** Quick records are path segments, not `.class` files. A failed walk is no names, not a suite. */
  private def namesOf(root: Path): UIO[Seen] =
    ZIO
      .acquireReleaseWith(open(root))(close)(fold)
      .foldZIO(
        _ => ZIO.succeed(Seen.Failed),
        files => ZIO.succeed(classify(root, files)),
      )

  private def open(root: Path): IO[Walk, java.util.stream.Stream[Path]] =
    ZIO.attemptBlocking(Files.walk(root)).mapError(_ => Walk.Failed)

  private def close(walk: java.util.stream.Stream[Path]): UIO[Unit] =
    ZIO.attemptBlocking(walk.close()).ignore

  private def fold(
      walk: java.util.stream.Stream[Path]
  ): IO[Walk, List[Path]] =
    ZIO.attemptBlocking(walk.iterator.asScala.toList).mapError(_ => Walk.Failed)

  private def classify(root: Path, files: List[Path]): Seen =
    val classes = files.filter(path => Files.isRegularFile(path) && classFile(path))
    if classes.nonEmpty then
      val names = classes.flatMap(path => shown(root, path)).distinct.sorted
      if names.isEmpty then Seen.Inner else Seen.Names(names)
    else
      val leaves = files.filter(path => Files.isRegularFile(path) && path != root)
      val names  = leaves.flatMap(path => dotted(root, path)).distinct.sorted
      if names.isEmpty then Seen.Empty else Seen.Names(names)

  private def classFile(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString.endsWith(".class"))

  private def shown(root: Path, path: Path): Option[String] =
    dotted(root, path).flatMap(suiteOf)

  private def dotted(root: Path, path: Path): Option[String] =
    val relative = root.relativize(path)
    val parts    = relative.iterator.asScala.map(_.toString).toList
    if parts.isEmpty || parts.exists(_.isEmpty) then None
    else Some(parts.mkString("."))

  private def suiteOf(binary: String): Option[String] =
    val name =
      if binary.endsWith(".class") then binary.stripSuffix(".class") else binary
    if name.isEmpty || name.contains("$$") then None
    else if name.endsWith("$") then
      val outer = name.dropRight(1)
      if outer.isEmpty || outer.contains("$") then None else Some(outer)
    else if name.contains("$") then None
    else Some(name)

  private def isDirectory(path: Path): UIO[Boolean] =
    ZIO.attemptBlocking(Files.isDirectory(path)).orDie

  private def records(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("test-quick")
      .resolve(module.value)
      .resolve(version.value)
      .resolve(platform.token)

  private enum Seen:
    case Failed
    case Empty
    case Inner
    case Names(names: List[String])

  private enum Walk:
    case Failed

  private final case class Buckets(skip: List[String], run: List[String], unknown: List[String]):
    def skipped(name: String): Buckets = copy(skip = skip :+ name)
    def ran(name: String): Buckets     = copy(run = run :+ name)
    def unread(name: String): Buckets  = copy(unknown = unknown :+ name)

  private object Buckets:
    val empty: Buckets = Buckets(Nil, Nil, Nil)

  private object TestClasses:
    def apply(
        repo: Path,
        module: ProjectName,
        version: ScalaVersion,
        platform: Platform,
    ): Path =
      histos.testrun.TestCompile.testClasses(repo, module, version, platform)
end QuickAnswer
