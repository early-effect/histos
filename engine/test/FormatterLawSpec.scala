package histos.format

import histos.resolve.{LockedTools, Repositories, Resolve}
import histos.workspace.Workspace
import histos.{ScalafmtConf, ToolCatalog}

import java.nio.file.{Files, Path}

import zio.{Scope, ZIO}
import zio.test.*

/** The scalafmt jars this checkout locks. A cached jar is reused. A missing one is fetched. */
object FormatterLawSpec extends ZIOSpecDefault:
  private val messy = "object   A {  def f( x:Int)=x }\n"
  private val tidy  = "object A { def f(x: Int) = x }\n"
  private val conf  = Path.of(".scalafmt.conf")

  def spec = suite("formatter")(
    test("write rewrites a messy file, leaves a tidy one byte for byte, and proves the written file on the next run"):
      within("runner.dialect = scala3"): (repo, jars) =>
        for
          _      <- write(repo, "a/src/A.scala", messy)
          _      <- write(repo, "a/src/B.scala", tidy.replace("A", "B"))
          first  <- run(repo, jars, FormatMode.Write)
          wrote  <- read(repo, "a/src/A.scala")
          kept   <- read(repo, "a/src/B.scala")
          second <- run(repo, jars, FormatMode.Write)
          third  <- run(repo, jars, FormatMode.Check)
        yield assertTrue(
          first.changed == List(Path.of("a/src/A.scala")),
          first.unchanged == 1,
          first.faults.isEmpty,
          wrote == tidy,
          kept == tidy.replace("A", "B"),
          second.skipped == 1,
          second.unchanged == 1,
          second.changed.isEmpty,
          third.skipped == 2,
          !third.failed,
        )
    ,
    test("check names a messy file, writes nothing, and fails"):
      within("runner.dialect = scala3"): (repo, jars) =>
        for
          _      <- write(repo, "a/src/A.scala", messy)
          report <- run(repo, jars, FormatMode.Check)
          after  <- read(repo, "a/src/A.scala")
        yield assertTrue(
          report.changed == List(Path.of("a/src/A.scala")),
          after == messy,
          report.failed,
          report.summary.endsWith("Run histos fmt"),
        )
    ,
    test("a syntax error is a parse fault on its line, and an excluded file is counted, not read"):
      within("runner.dialect = scala3\nproject.excludeFilters = [\"Skip\\\\.scala$\"]"): (repo, jars) =>
        for
          _      <- write(repo, "a/src/Bad.scala", "object Bad {\n  def f( = 1\n}\n")
          _      <- write(repo, "a/src/Skip.scala", messy)
          report <- run(repo, jars, FormatMode.Check)
        yield assertTrue(
          report.excluded == 1,
          report.faults.collect { case FormatFault.Parse(file, line, _, _) => (file, line) } ==
            List((Path.of("a/src/Bad.scala"), 2)),
          report.failed,
        )
    ,
    test("a configuration scalafmt rejects is a config error"):
      within("maxColumn = \"wide\""): (repo, jars) =>
        write(repo, "a/src/A.scala", tidy) *>
          run(repo, jars, FormatMode.Check).either.map: outcome =>
            assertTrue(outcome.left.exists:
              case FormatError.Config(path, _) => path == conf
              case _                           => false)
    ,
    test("a core the lock does not hold is drift, named with both sides"):
      within("runner.dialect = scala3"): (repo, jars) =>
        val moved = jars.roots.map(found => found.copy(version = s"${found.version}-moved"))
        write(repo, "a/src/A.scala", tidy) *>
          run(repo, jars.copy(roots = moved), FormatMode.Check).either.map: outcome =>
            assertTrue(outcome == Left(FormatError.CoreDrift(jars.roots, moved)))
    ,
    test("a second call with the same jars does not load scalafmt-dynamic again"):
      within("runner.dialect = scala3"): (repo, jars) =>
        for
          _      <- write(repo, "a/src/A.scala", tidy)
          _      <- run(repo, jars, FormatMode.Check)
          first  <- ZIO.serviceWithZIO[Formatters](_.openings)
          _      <- run(repo, jars, FormatMode.Write)
          second <- ZIO.serviceWithZIO[Formatters](_.openings)
        yield assertTrue(first >= 1, second == first),
  ).provideSomeLayerShared[TestEnvironment & Scope](Resolve.retained(cache) ++ Formatters.live) @@ TestAspect.sequential

  private def run(repo: Path, jars: FormatterJars, mode: FormatMode) =
    listed(repo).flatMap(files =>
      Formatters.format(FormatRequest(repo, conf, FormatSelection(files, GitFilter.Named), mode, jars))
    )

  private def listed(repo: Path) =
    ZIO
      .attemptBlocking(
        Files.walk(repo.resolve("a")).filter(_.toString.endsWith(".scala")).map(repo.relativize).toList
      )
      .map(found => scala.jdk.CollectionConverters.ListHasAsScala(found).asScala.toList.sortBy(_.toString))
      .orDie

  /** A temporary repository whose configuration names the scalafmt this checkout locks, with the jars that lock holds.
    */
  private def within[E](settings: String)(body: (Path, FormatterJars) => ZIO[Formatters & Resolve, E, TestResult]) =
    val format = ToolCatalog.default.format
    for
      version <- ScalafmtVersion.of(root, ScalafmtConf.Root)
      lock    <- Resolve.readLock(root)
      tools   <- ZIO.serviceWith[Resolve](LockedTools(lock, ToolCatalog.default, Repositories.central, _, None))
      dynamic <- tools.selected(ScalafmtTools.dynamic(format, Repositories.central))
      core    <- tools.selected(ScalafmtTools.core(version, format, Repositories.central))
      jars = FormatterJars(dynamic, core, ScalafmtTools.coreRoots(version, format).map(ScalafmtPackage.of))
      repo   <- Workspace.work("histos-formatter")
      _      <- write(repo, ".scalafmt.conf", s"version = \"${version.value}\"\n$settings\n")
      result <- body(repo, jars)
    yield result

  private def write(repo: Path, file: String, text: String) =
    ZIO.attemptBlocking {
      Files.createDirectories(repo.resolve(file).getParent)
      Files.writeString(repo.resolve(file), text)
    }.orDie

  private def read(repo: Path, file: String) =
    ZIO.attemptBlocking(Files.readString(repo.resolve(file))).orDie

  private def cache: Path = _root_.coursier.cache.CacheDefaults.location.toPath

  /** The checkout that holds `dependencies.lock` and `histos.scala`, from the test's working directory up. */
  private def root: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else Option(path.getParent).flatMap(walk(_, left - 1))
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)
end FormatterLawSpec
