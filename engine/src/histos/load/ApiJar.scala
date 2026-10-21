package histos.load

import histos.pack.ClassJar

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** The api jar tests compile a build file against. `dist/histos-api.jar` wins. Otherwise the classes just compiled. */
object ApiJar:
  def locate: IO[LoadError, Path] =
    val roots = repoRoots
    val packed = roots.map(_.resolve("dist").resolve("histos-api.jar"))
    ZIO.attemptBlocking(packed.find(path => Files.isRegularFile(path))).mapError(_ => LoadError.Io(LoadIo.Temp)).flatMap:
      case Some(jar) => ZIO.succeed(jar)
      case None =>
        compiled(roots) match
          case Some(classes) => jarOf(classes)
          case None          => ZIO.fail(LoadError.MissingFile(Path.of("dist/histos-api.jar")))

  private def repoRoots: List[Path] =
    val named = sys.env.get("HISTOS_REPO").filter(_.nonEmpty).map(Path.of(_))
    val cwd = Path.of("").toAbsolutePath.normalize
    (named.toList :+ cwd).distinct

  private def compiled(roots: List[Path]): Option[Path] =
    roots.foldLeft(Option.empty[Path]): (found, root) =>
      found.orElse(classesOf(root))

  private def classesOf(root: Path): Option[Path] =
    val api = root.resolve(".histos").resolve("classes").resolve("api")
    if !Files.isDirectory(api) then None
    else
      val listed = Files.list(api)
      val versions =
        try listed.iterator.asScala.filter(path => Files.isDirectory(path)).toList
        finally listed.close()
      val homes = versions.flatMap: version =>
        val home = version.resolve("jvm")
        if Files.isDirectory(home) then List(home) else Nil
      homes match
        case only :: Nil => Some(only)
        case _           => None

  private def jarOf(classes: Path): IO[LoadError, Path] =
    ZIO.attemptBlocking {
      val dir = Files.createTempDirectory("histos-api-jar")
      dir.resolve("histos-api.jar")
    }.mapError(_ => LoadError.Io(LoadIo.Temp)).flatMap: dest =>
      ClassJar.write(classes, dest).mapBoth(_ => LoadError.Io(LoadIo.Temp), _.path)
