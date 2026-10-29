package histos.preview

import histos.jdk.{Machine, Toolchain}
import histos.node.NodeToolchain
import histos.process.Process
import histos.resolve.{BuildResolvers, Resolve}
import histos.testrun.{QuickRecord, SuiteAsk, SuiteCover, SuiteName, SuiteStamp}
import histos.toolchain.RuntimeIdentity
import histos.zinc.GraphCompile
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, UIO, ZIO}

/** What a quick run would do with the classes and the lock already on disk. Preview does not launch a framework, fetch
  * a dependency, or write a record.
  *
  * A skip is a stored digest of [[SuiteCover]]. A cover that cannot be built is not known to be skippable.
  */
enum QuickAnswer:
  /** The test class directory is absent. No suite would be skipped. */
  case Uncompiled

  /** Suites named from `.class` files. `skip` is a stored digest that still matches. `run` would run: the digest
    * missed, or the class is gone. `unknown` could not be read, or the cover could not be built.
    */
  case Named(skip: List[String], run: List[String], unknown: List[String])

  /** Class files are present, but suites could not be named without a framework. `skip` are quick records whose digest
    * still matches. The rest are not known to be skippable.
    */
  case Partial(skip: List[String])
end QuickAnswer

object QuickAnswer:
  def read(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      graph: ProjectGraph,
  ): UIO[QuickAnswer] =
    val classes = TestClasses(repo, module.name, version, platform)
    isDirectory(classes).flatMap:
      case false => ZIO.succeed(QuickAnswer.Uncompiled)
      case true  =>
        namesOf(classes).flatMap:
          case Seen.Failed => ZIO.succeed(QuickAnswer.Partial(Nil))
          case seen        =>
            cover(repo, module, modules, version, platform, graph).flatMap: found =>
              decide(repo, module, modules, version, platform, classes, seen, found)

  private def decide(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      classes: Path,
      seen: Seen,
      found: Option[SuiteStamp.Cover],
  ): UIO[QuickAnswer] =
    seen match
      case Seen.Failed => ZIO.succeed(QuickAnswer.Partial(Nil))
      case Seen.Empty  =>
        recordNames(repo, module.name, version, platform).flatMap:
          case Nil   => ZIO.succeed(QuickAnswer.Uncompiled)
          case names =>
            found match
              case None        => ZIO.succeed(QuickAnswer.Named(Nil, Nil, names.sorted))
              case Some(stamp) =>
                judge(repo, module, modules, version, platform, classes, names, stamp)
      case Seen.Inner =>
        found match
          case None        => ZIO.succeed(QuickAnswer.Partial(Nil))
          case Some(stamp) =>
            matchingRecords(repo, module, modules, version, platform, classes, stamp)
      case Seen.Names(names) =>
        found match
          case None        => ZIO.succeed(QuickAnswer.Named(Nil, Nil, names.sorted))
          case Some(stamp) =>
            judge(repo, module, modules, version, platform, classes, names, stamp)

  private def cover(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      graph: ProjectGraph,
  ): UIO[Option[SuiteStamp.Cover]] =
    Resolve
      .readLock(repo)
      .either
      .flatMap:
        case Left(_)     => ZIO.succeed(None)
        case Right(lock) =>
          Machine
            .load(repo)
            .either
            .flatMap:
              case Left(_)        => ZIO.succeed(None)
              case Right(machine) =>
                accepted(platform, machine).map: runtime =>
                  runtime.flatMap: (jdk, node) =>
                    SuiteCover
                      .stored(
                        repo,
                        SuiteAsk(
                          module,
                          modules,
                          version,
                          platform,
                          lock,
                          BuildResolvers(graph.resolvers),
                          graph.tools,
                          machine.native,
                          jdk,
                          node,
                        ),
                      )
                      .toOption

  private def accepted(
      platform: Platform,
      machine: Machine,
  ): UIO[Option[(RuntimeIdentity, Option[RuntimeIdentity])]] =
    Toolchain
      .resolve(machine)
      .either
      .flatMap:
        case Left(_)    => ZIO.succeed(None)
        case Right(jdk) =>
          platform match
            case Platform.Jvm | Platform.Native => ZIO.succeed(Some((jdk.identity, None)))
            case Platform.Js                    =>
              NodeToolchain
                .resolve(machine)
                .provideLayer(Process.live)
                .either
                .map:
                  case Left(_)     => None
                  case Right(node) => Some((jdk.identity, Some(node.identity)))

  private def judge(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      classes: Path,
      names: List[String],
      stamp: SuiteStamp.Cover,
  ): UIO[QuickAnswer] =
    directories(repo, module, modules, version, platform, classes).flatMap: dirs =>
      SuiteStamp
        .digests(names, dirs, stamp)
        .foldZIO(
          _ => ZIO.succeed(QuickAnswer.Named(Nil, Nil, names.sorted)),
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
        if buckets.skip.isEmpty && buckets.run.isEmpty && buckets.unknown.isEmpty then QuickAnswer.Uncompiled
        else QuickAnswer.Named(buckets.skip, buckets.run, buckets.unknown)

  private def matchingRecords(
      repo: Path,
      module: Module,
      modules: List[Module],
      version: ScalaVersion,
      platform: Platform,
      classes: Path,
      stamp: SuiteStamp.Cover,
  ): UIO[QuickAnswer] =
    recordNames(repo, module.name, version, platform).flatMap: names =>
      if names.isEmpty then ZIO.succeed(QuickAnswer.Partial(Nil))
      else
        directories(repo, module, modules, version, platform, classes).flatMap: dirs =>
          SuiteStamp
            .digests(names, dirs, stamp)
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
      case None         => ZIO.succeed(false)
      case Some(digest) =>
        SuiteName.from(name) match
          case Left(_)      => ZIO.succeed(false)
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
    val upstream = Module
      .chain(module, modules, platform)
      .map: dependency =>
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
