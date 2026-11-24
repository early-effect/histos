package histos.doctor

import histos.*
import histos.central.{FeedError, MavenModule, ReleaseFeed}
import histos.version.{BumpPart, ShipNumber}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.GraphCompile

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import org.objectweb.asm.{ClassWriter, Opcodes}
import zio.{Duration, IO, Ref, UIO, ZIO, ZLayer}
import zio.test.*

object CompatibilitySpec extends ZIOSpecDefault:
  private val version      = ScalaVersion("3.10.1")
  private val otherVersion = ScalaVersion("3.8.2")
  private val first        = module("first", Jvm)(version).artifact(artifact"first-artifact")
  private val second       = module("second", Jvm)(version).artifact(artifact"second-artifact")
  private val previous     = ShipNumber.trusted(0, 15, 0)
  private val built        = Projects.organization(organization"example.test")(
    ships(Ship("library").next(0, 15, 1)(first, second)),
    first,
    second,
  )
  private val caught = Projects.organization(organization"example.test")(
    ships(Ship("library").next(0, 15, 0)(first, second)),
    first,
    second,
  )
  private val bumped = Projects.organization(organization"example.test")(
    ships(Ship("library").next(0, 16, 0)(first, second)),
    first,
    second,
  )
  private val cross      = module("cross", Jvm)(version, otherVersion).artifact(artifact"cross-artifact")
  private val crossBuilt = Projects.organization(organization"example.test")(
    ships(Ship("cross-line").next(0, 15, 1)(cross)),
    cross,
  )

  def spec = suite("compatibility evidence")(
    test("a break only in the second ship member fails the actual binary check") {
      ZIO.scoped:
        for
          root   <- Workspace.work("compatibility")
          graph  <- ZIO.fromEither(built)
          old    <- products(root.resolve("baseline"), method = true)
          jar    <- baselineJar(root.resolve("previous.jar"), old, "fixture/Api.class")
          _      <- current(root, "first", method = true)
          _      <- current(root, "second", method = false)
          report <- run(root, graph, listed(Some(previous)), available(jar))
        yield assertTrue(
          report.exit == 1,
          report.errors == 1,
          report.oks == 1,
          report.found.exists(_.summary.contains("example.test:second-artifact_3")),
        )
    },
    test("each artifact uses its own coordinate and release, never the first member's jar") {
      ZIO.scoped:
        for
          root      <- Workspace.work("coordinates")
          graph     <- ZIO.fromEither(built)
          firstOld  <- products(root.resolve("first-old"), method = true, name = "fixture/First")
          secondOld <- products(root.resolve("second-old"), method = true, name = "fixture/Second")
          _         <- products(classes(root, "first", version), method = true, name = "fixture/First")
          _         <- products(classes(root, "second", version), method = true, name = "fixture/Second")
          seen      <- Ref.make(List.empty[BaselineCoordinate])
          secondRelease = ShipNumber.trusted(0, 14, 2)
          feed          = new ReleaseFeed:
            def newest(module: MavenModule): IO[FeedError, Option[ShipNumber]] =
              ZIO.some(if module.artifact.value == "first-artifact_3" then previous else secondRelease)
          baseline = new CompatibilityBaseline:
            def fetch(coordinate: BaselineCoordinate): IO[BaselineError, Path] =
              seen
                .update(_ :+ coordinate)
                .as(if coordinate.module.artifact.value == "first-artifact_3" then firstOld else secondOld)
          report  <- run(root, graph, feed, baseline)
          fetched <- seen.get
        yield assertTrue(
          report.exit == 0,
          report.oks == 2,
          fetched.map(c => (c.module.group.value, c.module.artifact.value, c.revision.render)) ==
            List(("example.test", "first-artifact_3", "0.15.0"), ("example.test", "second-artifact_3", "0.14.2")),
        )
    },
    test("every Scala version is analyzed even when both publish the same binary suffix") {
      ZIO.scoped:
        for
          root   <- Workspace.work("versions")
          graph  <- ZIO.fromEither(crossBuilt)
          old    <- products(root.resolve("old"), method = true)
          _      <- products(classes(root, "cross", version), method = true)
          _      <- products(classes(root, "cross", otherVersion), method = false)
          report <- run(root, graph, listed(Some(previous)), available(old))
        yield assertTrue(report.exit == 1, report.errors == 1, report.oks == 1)
    },
    test("an unavailable expected baseline cannot produce a green doctor report") {
      ZIO.scoped:
        for
          root  <- Workspace.work("unavailable")
          graph <- ZIO.fromEither(built)
          _     <- current(root, "first", method = true)
          _     <- current(root, "second", method = true)
          baseline = new CompatibilityBaseline:
            def fetch(coordinate: BaselineCoordinate): IO[BaselineError, Path] =
              ZIO.fail(BaselineError.MissingJar(coordinate))
          report <- run(root, graph, listed(Some(previous)), baseline)
        yield assertTrue(
          report.exit == 1,
          report.errors == 2,
          report.oks == 0,
          report.skipped == 0,
          report.found.collect {
            case IncompleteCompatibility(_, CompatibilityIncomplete.BaselineFailed(_), Some(`previous`)) =>
              ()
          }.size == 2,
        )
    },
    test("unreachable metadata remains incomplete, not first publication") {
      ZIO.scoped:
        for
          root  <- Workspace.work("metadata")
          graph <- ZIO.fromEither(built)
          _     <- current(root, "first", method = true)
          _     <- current(root, "second", method = true)
          feed = new ReleaseFeed:
            def newest(module: MavenModule): IO[FeedError, Option[ShipNumber]] =
              ZIO.fail(FeedError.Unreachable(module.metadata))
          report <- run(root, graph, feed, available(root.resolve("unused")))
        yield assertTrue(
          report.exit == 1,
          report.errors == 2,
          report.oks == 0,
          report.found.collect { case IncompleteCompatibility(_, CompatibilityIncomplete.ListingFailed(_), None) =>
            ()
          }.size == 2,
        )
    },
    test("missing current products remain incomplete even without stable releases") {
      ZIO.scoped:
        for
          root   <- Workspace.work("missing")
          graph  <- ZIO.fromEither(built)
          report <- run(root, graph, listed(None), available(root.resolve("unused")))
        yield assertTrue(
          report.exit == 1,
          report.errors == 2,
          report.oks == 0,
          report.found.collect { case IncompleteCompatibility(_, CompatibilityIncomplete.MissingCurrent(_), None) =>
            ()
          }.size == 2,
        )
    },
    test("first publication is explicit and never fetches a baseline") {
      ZIO.scoped:
        for
          root    <- Workspace.work("first-publication")
          graph   <- ZIO.fromEither(built)
          _       <- current(root, "first", method = true)
          _       <- current(root, "second", method = true)
          fetched <- Ref.make(List.empty[BaselineCoordinate])
          baseline = new CompatibilityBaseline:
            def fetch(coordinate: BaselineCoordinate): IO[BaselineError, Path] =
              fetched.update(_ :+ coordinate) *> ZIO.fail(BaselineError.MissingJar(coordinate))
          report <- run(root, graph, listed(None), baseline)
          calls  <- fetched.get
        yield assertTrue(
          report.exit == 0,
          report.oks == 2,
          calls.isEmpty,
          report.outcomes.collect { case Outcome.Ok(_, note) if note.contains("first publication") => note }.size == 2,
        )
    },
    test("a stable release equal to the catalog is still compared") {
      ZIO.scoped:
        for
          root   <- Workspace.work("caught")
          graph  <- ZIO.fromEither(caught)
          old    <- products(root.resolve("old"), method = true)
          _      <- current(root, "first", method = true)
          _      <- current(root, "second", method = false)
          report <- run(root, graph, listed(Some(previous)), available(old))
        yield assertTrue(report.exit == 1, report.errors == 1)
    },
    test("the existing early-semver minor bump covers a second-member break") {
      ZIO.scoped:
        for
          root   <- Workspace.work("bumped")
          graph  <- ZIO.fromEither(bumped)
          old    <- products(root.resolve("old"), method = true)
          _      <- current(root, "first", method = true)
          _      <- current(root, "second", method = false)
          report <- run(root, graph, listed(Some(previous)), available(old))
        yield assertTrue(
          report.exit == 0,
          report.oks == 2,
          report.outcomes.exists {
            case Outcome.Ok(_, note)                         => note.contains("covers a minor")
            case Outcome.Found(_) | Outcome.Skipped(_, _, _) => false
          },
        )
    },
    test("JVM inventory honors Scala suffixes, assembly identities and a JS-first platform list") {
      val scala2 = ScalaVersion("2.13.18")
      val mixed  = module("mixed", Js, Jvm)(version, scala2).artifact(artifact"mixed-library")
      val tool   = module("tool", Jvm)(version).artifact(artifact"standalone-tool").assembly(MainClass("fixture.Main"))
      val inventory = Projects.organization(organization"example.test")(
        ships(Ship("inventory").next(0, 15, 1)(mixed, tool)),
        mixed,
        tool,
      )
      ZIO.scoped:
        for
          root  <- Workspace.work("inventory")
          graph <- ZIO.fromEither(inventory)
          old   <- products(root.resolve("old"), method = true)
          _     <- products(classes(root, "mixed", version), method = true)
          _     <- products(classes(root, "mixed", scala2), method = true)
          _     <- current(root, "tool", method = true)
          seen  <- Ref.make(List.empty[BaselineCoordinate])
          baseline = new CompatibilityBaseline:
            def fetch(coordinate: BaselineCoordinate): IO[BaselineError, Path] = seen.update(_ :+ coordinate).as(old)
          report      <- run(root, graph, listed(Some(previous)), baseline)
          coordinates <- seen.get
        yield assertTrue(
          report.exit == 0,
          report.oks == 3,
          coordinates.map(_.module.artifact.value) == List("mixed-library_3", "mixed-library_2.13", "standalone-tool"),
        )
    },
    test("corrupt baseline jars produce incomplete analysis through the check boundary") {
      ZIO.scoped:
        for
          root  <- Workspace.work("corrupt")
          graph <- ZIO.fromEither(built)
          jar = root.resolve("corrupt.jar")
          _      <- Workspace.writeString(jar, "not a jar")
          _      <- current(root, "first", method = true)
          _      <- current(root, "second", method = true)
          report <- run(root, graph, listed(Some(previous)), available(jar))
        yield assertTrue(
          report.exit == 1,
          report.errors == 2,
          report.oks == 0,
          report.found.collect {
            case IncompleteCompatibility(
                  _,
                  CompatibilityIncomplete.AnalysisFailed(CompatibilityError.AnalysisFailed(_, _)),
                  Some(`previous`),
                ) =>
              ()
          }.size == 2,
        )
    },
    test("baseline extraction rejects escaping classfile paths") {
      ZIO.scoped:
        for
          root   <- Workspace.work("escape")
          old    <- products(root.resolve("old"), method = true)
          jar    <- baselineJar(root.resolve("escape.jar"), old, "../Escape.class")
          now    <- products(root.resolve("now"), method = true)
          result <- ZIO
            .serviceWithZIO[CompatibilityAnalyzer](_.analyze(CompatibilityInput(Platform.Jvm, jar, now)).either)
            .provideLayer(CompatibilityAnalyzer.jvm)
        yield assertTrue(result == Left(CompatibilityError.InvalidBaselineEntry(jar, "../Escape.class")))
    },
    test("TASTy entries obey the same traversal and embedded-NUL jail") {
      ZIO.scoped:
        for
          root    <- Workspace.work("tasty-jail")
          old     <- products(root.resolve("old"), method = true)
          now     <- products(root.resolve("now"), method = true)
          results <- ZIO.foreach(List("../Escape.tasty", "fixture/Invalid\u0000.tasty").zipWithIndex) {
            (entry, index) =>
              for
                jar    <- baselineJar(root.resolve(s"malignant-$index.jar"), old, entry)
                result <- ZIO
                  .serviceWithZIO[CompatibilityAnalyzer](_.analyze(CompatibilityInput(Platform.Jvm, jar, now)).either)
                  .provideLayer(CompatibilityAnalyzer.jvm)
              yield result == Left(CompatibilityError.InvalidBaselineEntry(jar, entry))
          }
        yield assertTrue(results == List(true, true))
    },
    test("embedded-NUL classfile entries return the exact typed analyzer error") {
      ZIO.scoped:
        for
          root <- Workspace.work("nul-analyzer")
          old  <- products(root.resolve("old"), method = true)
          entry = "fixture/Invalid\u0000.class"
          jar    <- baselineJar(root.resolve("nul.jar"), old, entry)
          now    <- products(root.resolve("now"), method = true)
          result <- ZIO
            .serviceWithZIO[CompatibilityAnalyzer](_.analyze(CompatibilityInput(Platform.Jvm, jar, now)).either)
            .provideLayer(CompatibilityAnalyzer.jvm)
        yield assertTrue(result == Left(CompatibilityError.InvalidBaselineEntry(jar, entry)))
    },
    test("embedded-NUL classfile entries produce doctor incomplete evidence without defects") {
      ZIO.scoped:
        for
          root  <- Workspace.work("nul-doctor")
          graph <- ZIO.fromEither(built)
          old   <- products(root.resolve("old"), method = true)
          entry = "fixture/Invalid\u0000.class"
          jar    <- baselineJar(root.resolve("nul.jar"), old, entry)
          _      <- current(root, "first", method = true)
          _      <- current(root, "second", method = true)
          report <- run(root, graph, listed(Some(previous)), available(jar))
        yield assertTrue(
          report.exit == 1,
          report.errors == 2,
          report.oks == 0,
          report.skipped == 0,
          report.found.collect {
            case IncompleteCompatibility(_, CompatibilityIncomplete.AnalysisFailed(error), Some(`previous`)) =>
              error
          } == List.fill(2)(CompatibilityError.InvalidBaselineEntry(jar, entry)),
        )
    },
    test("the JVM layer explicitly rejects JS and Native") {
      val errors = ZIO.foreach(List(Platform.Js, Platform.Native)): backend =>
        ZIO
          .serviceWithZIO[CompatibilityAnalyzer](
            _.analyze(CompatibilityInput(backend, Path.of("old"), Path.of("new"))).either
          )
          .map(_ == Left(CompatibilityError.UnsupportedBackend(backend)))
      val checked = errors.provideLayer(CompatibilityAnalyzer.jvm)
      checked.map(results => assertTrue(results == List(true, true)))
    },
    test("MiMa missing-class problems are typed and retain stable major-bump policy") {
      ZIO.scoped:
        for
          root   <- Workspace.work("missing-class")
          old    <- products(root.resolve("old"), method = true)
          now    <- Workspace.createDirectories(root.resolve("now"))
          result <- ZIO
            .serviceWithZIO[CompatibilityAnalyzer](_.analyze(CompatibilityInput(Platform.Jvm, old, now)))
            .provideLayer(CompatibilityAnalyzer.jvm)
        yield assertTrue(
          BinaryCheck.difference(ShipNumber.trusted(1, 0, 0), result) == BinaryCheck.Difference
            .Break(BumpPart.Major, 1),
          BinaryCheck.difference(previous, result) == BinaryCheck.Difference.Break(BumpPart.Minor, 1),
        )
    },
    test("missing baseline products from a resolver are an analysis failure, not compatible") {
      ZIO.scoped:
        for
          root   <- Workspace.work("missing-baseline")
          graph  <- ZIO.fromEither(built)
          _      <- current(root, "first", method = true)
          _      <- current(root, "second", method = true)
          report <- run(root, graph, listed(Some(previous)), available(root.resolve("absent.jar")))
        yield assertTrue(
          report.exit == 1,
          report.errors == 2,
          report.found.collect {
            case IncompleteCompatibility(
                  _,
                  CompatibilityIncomplete.AnalysisFailed(CompatibilityError.MissingPrevious(_)),
                  Some(`previous`),
                ) =>
              ()
          }.size == 2,
        )
    },
  )

  private def listed(release: Option[ShipNumber]): ReleaseFeed = new ReleaseFeed:
    def newest(module: MavenModule): IO[FeedError, Option[ShipNumber]] = ZIO.succeed(release)

  private def available(path: Path): CompatibilityBaseline = new CompatibilityBaseline:
    def fetch(coordinate: BaselineCoordinate): IO[BaselineError, Path] = ZIO.succeed(path)

  private def run(
      root: Path,
      graph: ProjectGraph,
      feed: ReleaseFeed,
      baseline: CompatibilityBaseline,
  ): UIO[DoctorReport] =
    val verification = (ZLayer.succeed(feed) ++ ZLayer.succeed(
      baseline
    ) ++ CompatibilityAnalyzer.jvm) >>> CompatibilityVerification.layer
    val checks = ShipChecks.checks(root, graph, Map.empty, feed, verification).filter(_.id == ShipChecks.binaryId)
    Runner.run(checks, DoctorContext(Nil, Set(Probe.Local)), Duration.Infinity)

  private def classes(root: Path, member: String, scalaVersion: ScalaVersion): Path =
    GraphCompile.classesDir(root, ProjectName.trusted(member), scalaVersion)

  private def current(root: Path, member: String, method: Boolean): IO[IoFailure, Path] =
    products(classes(root, member, version), method)

  private def baselineJar(path: Path, source: Path, entry: String): IO[IoFailure, Path] =
    ZIO.scoped:
      ZIO
        .acquireRelease(
          ZIO
            .attemptBlocking(new JarOutputStream(Files.newOutputStream(path)))
            .mapError(_ => IoFailure(path, IoFault.Write))
        )(jar => ZIO.attemptBlocking(jar.close()).orDie)
        .flatMap: jar =>
          ZIO
            .attemptBlocking:
              jar.putNextEntry(new JarEntry(entry))
              jar.write(Files.readAllBytes(source.resolve("fixture/Api.class")))
              jar.closeEntry()
              path
            .mapError(_ => IoFailure(path, IoFault.Write))

  private def products(path: Path, method: Boolean, name: String = "fixture/Api"): IO[IoFailure, Path] =
    ZIO
      .attemptBlocking:
        Files.createDirectories(path.resolve("fixture"))
        val writer = new ClassWriter(0)
        writer.visit(
          Opcodes.V1_8,
          Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
          name,
          "Ljava/lang/Object;",
          "java/lang/Object",
          Array.empty[String],
        )
        if method then
          writer
            .visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT, "kept", "()I", "()I", Array.empty[String])
            .visitEnd()
        writer.visitEnd()
        Files.write(path.resolve(s"$name.class"), writer.toByteArray)
        path
      .mapError(_ => IoFailure(path, IoFault.Write))
end CompatibilitySpec
