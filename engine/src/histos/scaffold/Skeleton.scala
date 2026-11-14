package histos.scaffold

import histos.jdk.Sha256
import histos.publish.LauncherScript
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.ProjectName

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum SkeletonError:
  case Name(raw: String)
  case Exists(path: Path)
  case NotEmpty(path: Path)
  case Version
  case Io(failure: IoFailure)

  /** A full scaffold needs the rendering ship's shim digest. */
  case Shim

  def message: String = this match
    case Name(raw)      => s"'$raw' is not a project name"
    case Exists(path)   => s"refusing to overwrite $path"
    case NotEmpty(path) => s"$path is not an empty directory"
    case Version        => "histos version is unknown"
    case Io(failure)    => failure.message
    case Shim           => "the rendering ship has no shim digest"
end SkeletonError

/** The one-module project `histos new` and `histos init` write. The pin is the toolchain that wrote it. `scalaVersion`
  * and `zioVersion` are the versions this repository builds with.
  */
object Skeleton:
  val scalaVersion: String = "3.10.1"
  val zioVersion: String   = "2.1.27"

  val ignoreLines: List[String] = List(".histos/", "out/")

  def create(
      parent: Path,
      raw: String,
      version: String,
      kind: ScaffoldKind = ScaffoldKind.Full,
      shim: Option[Sha256] = None,
  ): IO[SkeletonError, Path] =
    for
      name <- ZIO.fromEither(ProjectName.from(raw)).mapError(_ => SkeletonError.Name(raw))
      dir = parent.resolve(name.value)
      _ <- empty(dir)
      _ <- write(dir, packageName(name.value), version, kind, None, shim)
    yield dir

  def init(
      dir: Path,
      version: String,
      kind: ScaffoldKind = ScaffoldKind.Full,
      draft: Option[String] = None,
      shim: Option[Sha256] = None,
  ): IO[SkeletonError, Path] =
    val raw = Option(dir.getFileName).fold("")(_.toString)
    for
      name <- ZIO.fromEither(ProjectName.from(raw)).mapError(_ => SkeletonError.Name(raw))
      _    <- absent(dir)
      _    <- write(dir, packageName(name.value), version, kind, draft, shim)
    yield dir

  def versionOf(raw: Option[String]): Either[SkeletonError, String] =
    raw.map(_.trim).filter(_.nonEmpty) match
      case Some(s"histos $version") if version.nonEmpty   => Right(version)
      case Some(version) if !version.startsWith("histos") => Right(version)
      case _                                              => Left(SkeletonError.Version)

  private def packageName(name: String): String =
    val stripped = name.replace("-", "")
    if stripped.nonEmpty then stripped else name

  private def empty(dir: Path): IO[SkeletonError, Unit] =
    ZIO
      .attemptBlocking {
        if !Files.exists(dir) then Right(())
        else if !Files.isDirectory(dir) then Left(SkeletonError.NotEmpty(dir))
        else
          val listed   = Files.list(dir)
          val occupied =
            try listed.iterator.asScala.nonEmpty
            finally listed.close()
          if occupied then Left(SkeletonError.NotEmpty(dir)) else Right(())
      }
      .mapError(_ => SkeletonError.Io(IoFailure(dir, IoFault.Stat)))
      .flatMap(ZIO.fromEither) *> absent(dir)

  private def absent(dir: Path): IO[SkeletonError, Unit] =
    ZIO
      .foreachDiscard(List(dir.resolve("histos.scala"), dir.resolve("build.properties"))): path =>
        ZIO.attemptBlocking(Files.exists(path)).mapError(_ => SkeletonError.Io(IoFailure(path, IoFault.Stat))).flatMap {
          exists =>
            if exists then ZIO.fail(SkeletonError.Exists(path)) else ZIO.unit
        }

  private def write(
      dir: Path,
      pkg: String,
      version: String,
      kind: ScaffoldKind,
      draft: Option[String],
      shim: Option[Sha256],
  ): IO[SkeletonError, Unit] =
    val sources = draft match
      case Some(text) =>
        List(
          dir.resolve("build.properties") -> s"histos.version=$version\n",
          dir.resolve("histos.scala")     -> text,
        )
      case None =>
        List(
          dir.resolve("build.properties")                        -> s"histos.version=$version\n",
          dir.resolve("histos.scala")                            -> buildFile(pkg),
          dir.resolve(s"app/src/main/scala/$pkg/Main.scala")     -> mainSource(pkg),
          dir.resolve(s"app/src/test/scala/$pkg/MainSpec.scala") -> specSource(pkg),
        )
    ZIO.foreachDiscard(sources): (path, text) =>
      Workspace.writeString(path, text).mapError(SkeletonError.Io(_)) *>
        ZIO.whenDiscard(kind == ScaffoldKind.Full)(gitignore(dir) *> script(dir, shim))

  private def gitignore(dir: Path): IO[SkeletonError, Unit] =
    val path = dir.resolve(".gitignore")
    Workspace.readIfPresent(path).mapError(SkeletonError.Io(_)).flatMap {
      case None =>
        Workspace.writeString(path, ignoreLines.mkString("", "\n", "\n")).mapError(SkeletonError.Io(_))
      case Some(text) =>
        val have    = text.linesIterator.map(_.trim).toSet
        val missing = ignoreLines.filterNot(have.contains)
        if missing.isEmpty then ZIO.unit
        else
          val gap    = if text.endsWith("\n") || text.isEmpty then "" else "\n"
          val suffix = gap + missing.mkString("", "\n", "\n")
          Workspace.writeString(path, text + suffix).mapError(SkeletonError.Io(_))
    }

  private def script(dir: Path, shim: Option[Sha256]): IO[SkeletonError, Unit] =
    shim match
      case None      => ZIO.fail(SkeletonError.Shim)
      case Some(sha) =>
        val path = dir.resolve("histos")
        ZIO.attemptBlocking(Files.exists(path)).mapError(_ => SkeletonError.Io(IoFailure(path, IoFault.Stat))).flatMap {
          exists =>
            if exists then ZIO.unit
            else
              Workspace.writeString(path, LauncherScript.posix(sha)).mapError(SkeletonError.Io(_)) *>
                ZIO
                  .attemptBlocking(path.toFile.setExecutable(true))
                  .mapError(_ => SkeletonError.Io(IoFailure(path, IoFault.Write)))
                  .unit
        }

  private def buildFile(pkg: String): String =
    s"""import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"$scalaVersion")
       |      .libraryDependencies(dep"dev.zio::zio:$zioVersion")
       |      .testDependencies(
       |        dep"dev.zio::zio-test:$zioVersion",
       |        dep"dev.zio::zio-test-sbt:$zioVersion",
       |      )
       |      .main(MainClass("$pkg.Main"))
       |    Projects(app)
       |""".stripMargin

  private def mainSource(pkg: String): String =
    s"""package $pkg
       |
       |import zio.*
       |
       |object Main extends ZIOAppDefault:
       |  val hello: String = "hello"
       |
       |  def run = Console.printLine(hello)
       |""".stripMargin

  private def specSource(pkg: String): String =
    s"""package $pkg
       |
       |import zio.test.*
       |
       |object MainSpec extends ZIOSpecDefault:
       |  def spec = suite("Main")(
       |    test("hello")(assertTrue(Main.hello == "hello"))
       |  )
       |""".stripMargin
end Skeleton
