package histos.cli

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object AssemblySpec extends histos.SuiteClock:
  private val utf8 = StandardCharsets.UTF_8

  private val checks = suite("runtime assembly")(
    test("the runtime jar names its main and contains no scala native classes"):
      for
        repo  <- System.env("HISTOS_REPO")
        xdg   <- System.env("XDG_CACHE_HOME")
        home  <- System.property("user.home")
        found <- ZIO.attemptBlocking(
          (rooted(repo, "dist/histos.jar") ++ cachedLaunchers(xdg, home)).find(path => Files.isRegularFile(path))
        )
        checked <- found match
          case None      => ZIO.succeed(assertTrue(found.nonEmpty))
          case Some(jar) =>
            for
              names    <- ZIO.attempt(namesOf(jar))
              manifest <- ZIO.attempt(manifestOf(jar))
            yield assertTrue(
              names.contains("histos/cli/Main.class"),
              !names.exists(_.startsWith("scala/scalanative/")),
              manifest.contains("Main-Class: histos.cli.Main"),
            )
      yield checked
  )

  private def rooted(repo: Option[String], name: String): List[Path] =
    repo.filter(_.nonEmpty).map(Path.of(_)).toList.map(_.resolve(name)) ++ List(
      Path.of(name),
      Path.of("..").resolve(name),
    )

  /** The launcher `./histos` downloaded when `dist/histos.jar` is not in the checkout. */
  private def cachedLaunchers(xdg: Option[String], home: Option[String]): List[Path] =
    val cache = xdg.filter(_.nonEmpty).map(Path.of(_)).orElse(home.filter(_.nonEmpty).map(Path.of(_).resolve(".cache")))
    cache.toList.flatMap: root =>
      val launchers = root.resolve("histos").resolve("launchers")
      if !Files.isDirectory(launchers) then Nil
      else
        val walk = Files.walk(launchers)
        try
          walk.iterator.asScala
            .filter(path => Files.isRegularFile(path) && Option(path.getFileName).exists(_.toString == "histos.jar"))
            .toList
            .sortBy(path => -Files.getLastModifiedTime(path).toMillis)
        finally walk.close()

  private def namesOf(path: Path): Set[String] =
    val zip = new ZipFile(path.toFile)
    try zip.entries.asScala.map(_.getName).toSet
    finally zip.close()

  private def manifestOf(path: Path): String =
    val zip = new ZipFile(path.toFile)
    try
      Option(zip.getEntry("META-INF/MANIFEST.MF")) match
        case None        => ""
        case Some(entry) =>
          val in = zip.getInputStream(entry)
          try new String(in.readAllBytes(), utf8)
          finally in.close()
    finally zip.close()

  def spec = checks @@ TestAspect.withLiveSystem @@ TestAspect.timeout(30.seconds)
end AssemblySpec
