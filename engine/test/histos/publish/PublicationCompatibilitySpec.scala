package histos.build

import histos.*
import histos.build.BuildNode
import histos.cache.ContentStore
import histos.central.*
import histos.doctor.*
import histos.jdk.Sha256
import histos.plugin.{ApiDigest, SourceSet}
import histos.publish.*
import histos.resolve.Resolve
import histos.schedule.RemoteCache
import histos.version.{BumpPart, Session, ShipNumber}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompiledCell, GraphCompile, ProductDigest}
import java.nio.file.{Files, Path}
import org.objectweb.asm.{ClassWriter, Opcodes}
import zio.*
import zio.test.*

object PublicationCompatibilitySpec extends ZIOSpecDefault:
  private val version       = ScalaVersion("3.10.1")
  private val previous      = ShipNumber.trusted(0, 15, 0)
  private val firstPrevious = ShipNumber.trusted(0, 14, 2)
  private val first         = module("first", Jvm)(version).artifact(artifact"first-artifact")
  private val second        = module("second", Jvm, Js)(version).artifact(artifact"second-artifact")
  private val definitions   = Projects
    .organization(organization"example.test")
    .license(License(LicenseName("Apache-2.0"), MetadataUri("https://example.test/license")))
    .developer(Developer(DeveloperId("owner"), PomText("Owner")))
    .scm(Scm(MetadataUri("https://example.test/repo"), ScmConnection("scm:git:example.test/repo")))

  private enum Fixture:
    case Broken, Unavailable, Compatible, Covered, First, MissingCurrent, SecondVersion, Initialization, Corrupt

  private val writeSuites =
    List(PublishWrite.Local, PublishWrite.Snapshots, PublishWrite.Release).map: write =>
      suite(write.toString)(
        test("a break only in the second member prevents every publication callback") {
          boundary(write, Fixture.Broken, subset = false).map: (result, callbacks, stages, baselines) =>
            assertTrue(
              policyFailure(result),
              callbacks.isEmpty,
              stages.contains(stage(ProjectName("second"))),
              baselines.map(_.module.artifact.value) == List("first-artifact_3", "second-artifact_3"),
              baselines.map(_.revision) == List(firstPrevious, previous),
              baselines.forall(_.module.group == organization"example.test"),
            )
        },
        test("an unavailable second baseline prevents every publication callback, even when only first is selected") {
          boundary(write, Fixture.Unavailable, subset = true).map: (result, callbacks, stages, baselines) =>
            assertTrue(
              incompleteFailure(result, PublicationCompatibilityGap.BaselineMissing(previous)),
              callbacks.isEmpty,
              stages.toSet == Set(stage(ProjectName("first")), stage(ProjectName("second"))),
              baselines.size == 2,
            )
        },
        test("complete compatible evidence allows publication callbacks") {
          boundary(write, Fixture.Compatible, subset = false).map: (result, callbacks, _, baselines) =>
            assertTrue(
              result.exists(_.modules.size == (if write == PublishWrite.Snapshots then 6 else 3)),
              callbacks.toSet == Set("first-artifact_3", "second-artifact_3", "second-artifact_sjs1_3"),
              callbacks.size == (if write == PublishWrite.Snapshots then 6 else 3),
              baselines.size == 2,
            )
        },
      )

  def spec = suite("publication compatibility boundary")(
    suite("all direct write modes")(writeSuites*),
    test("a break only in another Scala version also prevents publication") {
      boundary(PublishWrite.Local, Fixture.SecondVersion, subset = true).map: (result, callbacks, stages, baselines) =>
        assertTrue(
          callbacks.isEmpty,
          baselines.size == 3,
          stages.contains(
            BuildNode.Compile(
              CompilationStage(Cell(ProjectName("second"), ScalaVersion("3.8.2"), Platform.Jvm), SourceSet.Main)
            )
          ),
          result.left.exists:
            case LocalPublishError.Compatibility(failures) =>
              failures.exists(failure =>
                failure.member == ProjectName("second") && failure.version == ScalaVersion("3.8.2") &&
                  failure.outcome == PublicationCompatibilityOutcome.Policy(previous, BumpPart.Minor, 1)
              )
            case _ => false,
        )
    },
    test("a corrupt second baseline retains its selected revision and analysis detail before any callback") {
      boundary(PublishWrite.Local, Fixture.Corrupt, subset = true).map: (result, callbacks, _, baselines) =>
        assertTrue(
          callbacks.isEmpty,
          baselines.map(_.revision) == List(firstPrevious, previous),
          result.left.exists:
            case LocalPublishError.Compatibility(failures) =>
              failures.size == 1 && failures.exists(failure =>
                failure.member == ProjectName("second") && failure.version == version &&
                  failure.coordinate == PublicationCoordinate(organization"example.test", "second-artifact_3") &&
                  (failure.outcome match
                    case PublicationCompatibilityOutcome.Incomplete(
                          PublicationCompatibilityGap.Analysis(old, current),
                          Some(`previous`),
                        ) =>
                      old.endsWith(Path.of("opaque-baseline.jar")) &&
                      current.endsWith(Path.of(".histos", "classes", "second", version.value, "jvm"))
                    case _ => false)
              )
            case _ => false,
        )
    },
    test("early-semver minor bump covers a second-member break") {
      boundary(PublishWrite.Local, Fixture.Covered, subset = true).map: (result, callbacks, _, _) =>
        assertTrue(result.exists(_.modules.nonEmpty), callbacks == List("first-artifact_3"))
    },
    test("analyzer initialization failure retains every applicable member before any callback") {
      boundary(PublishWrite.Local, Fixture.Initialization, subset = true).map: (result, callbacks, stages, baselines) =>
        assertTrue(
          callbacks.isEmpty,
          baselines.isEmpty,
          stages.toSet == Set(stage(ProjectName("first")), stage(ProjectName("second"))),
          result.left.exists:
            case LocalPublishError.Compatibility(failures) =>
              failures.map(_.member).toSet == Set(ProjectName("first"), ProjectName("second")) &&
              failures.forall(
                _.outcome == PublicationCompatibilityOutcome.Incomplete(
                  PublicationCompatibilityGap.AnalyzerInitialization,
                  None,
                )
              )
            case _ => false,
        )
    },
    test("first publication is explicit and does not fetch any baseline") {
      boundary(PublishWrite.Local, Fixture.First, subset = true).map: (result, callbacks, _, baselines) =>
        assertTrue(result.exists(_.modules.nonEmpty), callbacks == List("first-artifact_3"), baselines.isEmpty)
    },
    test("missing current classes remain fatal after the executor returns") {
      boundary(PublishWrite.Local, Fixture.MissingCurrent, subset = true).map: (result, callbacks, _, _) =>
        assertTrue(
          result.left.exists:
            case LocalPublishError.Compatibility(failures) =>
              failures.exists(_.outcome match
                case PublicationCompatibilityOutcome.Incomplete(PublicationCompatibilityGap.MissingCurrent(_), None) =>
                  true
                case _ => false)
            case _ => false
          ,
          callbacks.isEmpty,
        )
    },
  )

  private def stage(name: ProjectName): BuildNode =
    BuildNode.Compile(CompilationStage(Cell(name, version, Platform.Jvm), SourceSet.Main))

  private def policyFailure(
      result: Either[LocalPublish.ProjectionError | ExecutionError | IoFailure, PublishOutput]
  ): Boolean =
    result.left.exists:
      case LocalPublishError.Compatibility(failures) =>
        failures.exists(failure =>
          failure.member == ProjectName("second") && failure.version == version &&
            failure.coordinate == PublicationCoordinate(organization"example.test", "second-artifact_3") &&
            failure.outcome == PublicationCompatibilityOutcome.Policy(previous, BumpPart.Minor, 1)
        )
      case _ => false

  private def incompleteFailure(
      result: Either[LocalPublish.ProjectionError | ExecutionError | IoFailure, PublishOutput],
      gap: PublicationCompatibilityGap,
  ): Boolean = result.left.exists:
    case LocalPublishError.Compatibility(failures) =>
      failures.exists(failure =>
        failure.member == ProjectName("second") && failure.version == version &&
          failure.coordinate == PublicationCoordinate(organization"example.test", "second-artifact_3") &&
          failure.outcome == PublicationCompatibilityOutcome.Incomplete(gap, Some(previous))
      )
    case _ => false

  private def boundary(write: PublishWrite, fixture: Fixture, subset: Boolean) = ZIO.scoped:
    for
      repo <- Workspace.work("publish-compatibility")
      member =
        if fixture == Fixture.SecondVersion then
          module("second", Jvm, Js)(version, ScalaVersion("3.8.2")).artifact(artifact"second-artifact")
        else second
      ship =
        if fixture == Fixture.Covered then Ship("library").next(0, 16, 0)(first, member)
        else Ship("library").next(0, 15, 1)(first, member)
      graph <- ZIO.fromEither(definitions(ships(ship), first, member))
      _     <- Workspace.writeString(repo.resolve("releases"), "")
      old   <- products(repo.resolve("old"), method = true)
      corrupt = repo.resolve("opaque-baseline.jar")
      _         <- Workspace.writeString(corrupt, "not a jar")
      calls     <- Ref.make(List.empty[String])
      requested <- Ref.make(List.empty[BuildNode])
      fetched   <- Ref.make(List.empty[BaselineCoordinate])
      feed = new ReleaseFeed:
        def newest(module: MavenModule): IO[FeedError, Option[ShipNumber]] =
          ZIO.succeed(
            Option.unless(fixture == Fixture.First)(
              if module.artifact.value == "first-artifact_3" then firstPrevious else previous
            )
          )
      baseline = new CompatibilityBaseline:
        def fetch(coordinate: BaselineCoordinate): IO[BaselineError, Path] =
          fetched.update(_ :+ coordinate) *> (
            if fixture == Fixture.Unavailable && coordinate.module.artifact.value == "second-artifact_3" then
              ZIO.fail(BaselineError.MissingJar(coordinate))
            else if fixture == Fixture.Corrupt && coordinate.module.artifact.value == "second-artifact_3" then
              ZIO.succeed(corrupt)
            else ZIO.succeed(old)
          )
      verification =
        if fixture == Fixture.Initialization then
          ZLayer.fail(CompatibilityError.InitializationFailed(new java.io.IOException("fixture")))
        else
          (ZLayer.succeed(feed) ++ ZLayer.succeed(
            baseline
          ) ++ CompatibilityAnalyzer.jvm) >>> CompatibilityVerification.layer
      compiler = PublishCompiler[Any, IoFailure](nodes =>
        requested.set(nodes) *> ZIO
          .foreach(nodes): node =>
            node match
              case BuildNode.Compile(compilation) =>
                val cell     = compilation.cell
                val classes  = GraphCompile.classesDir(repo, cell.module, cell.version, cell.platform)
                val analysis = repo
                  .resolve(".histos/analysis")
                  .resolve(cell.module.value)
                  .resolve(cell.version.value)
                  .resolve(cell.platform.token)
                for
                  _ <- Workspace.writeString(repo.resolve(cell.module.value).resolve("src/Api.scala"), "object Api")
                  _ <-
                    if fixture == Fixture.MissingCurrent && cell.module == ProjectName("second") then ZIO.unit
                    else
                      products(
                        classes,
                        method = cell.module != ProjectName(
                          "second"
                        ) || (fixture != Fixture.Broken && fixture != Fixture.Covered &&
                          !(fixture == Fixture.SecondVersion && cell.version == ScalaVersion("3.8.2"))),
                      ).unit
                  key <-
                    if fixture == Fixture.MissingCurrent && cell.module == ProjectName("second") then
                      ZIO.succeed(Sha256.trusted("0" * 64))
                    else
                      ZIO
                        .fromEither(ProductDigest.write(analysis, classes, repo))
                        .mapError(_ => IoFailure(analysis, IoFault.Write))
                  compiled = CompiledCell(
                    cell.module,
                    cell.version,
                    cell.platform,
                    classes,
                    1,
                    Some(key),
                    true,
                    None,
                    None,
                  )
                yield node -> ExecutionProduct.Compiled(StageProduct(compilation, compiled, analysis, key, key, Nil))
              case BuildNode.Generate(_) | BuildNode.Link(_, _) => ZIO.fail(ExecutionError.WrongProduct(node))
          .map(values => new ExecutionProducts(values.toMap))
      )
      recording = ZLayer.fromZIO(
        ZIO
          .service[Publisher]
          .map(delegate =>
            new Publisher:
              def publish(artifact: Artifact): IO[PublishError, Published] =
                calls.update(_ :+ artifact.name.value) *> delegate.publish(artifact)
          )
      )
      env =
        if write == PublishWrite.Release then Map(Session.Key -> "release")
        else
          Map(
            Session.Key       -> "snapshot",
            Session.ShaKey    -> ("a" * 40),
            Session.BranchKey -> "other",
          )
      names =
        if subset then List(PublishName.Module(ProjectName("first")))
        else List(PublishName.Ship(histos.version.ShipName("library")))
      result <- LocalPublish
        .runUsing(compiler, verification >>> PublicationCompatibility.layer)(
          repo,
          graph,
          names,
          env,
          write,
          ApiDigest.trusted("0" * 64),
        )
        .either
        .provideLayer(
          (Publisher.local(repo.resolve("maven")) >>> recording) ++ Resolve.offline(repo.resolve("resolve")) ++
            ZLayer.succeed(ContentStore.in(repo.resolve("cache"))) ++ RemoteCache.off
        )
      callbacks <- calls.get
      stages    <- requested.get
      baselines <- fetched.get
    yield (result, callbacks, stages, baselines)

  private def products(path: Path, method: Boolean): IO[IoFailure, Path] =
    ZIO
      .attemptBlocking:
        Files.createDirectories(path.resolve("fixture"))
        val writer = new ClassWriter(0)
        writer.visit(
          Opcodes.V1_8,
          Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
          "fixture/Api",
          "Ljava/lang/Object;",
          "java/lang/Object",
          Array.empty[String],
        )
        if method then
          writer
            .visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT, "kept", "()I", "()I", Array.empty[String])
            .visitEnd()
        writer.visitEnd()
        Files.write(path.resolve("fixture/Api.class"), writer.toByteArray)
        path
      .mapError(_ => IoFailure(path, IoFault.Write))
end PublicationCompatibilitySpec
