package histos.load

import histos.jdk.Sha256
import histos.plugin.{
  ArtifactFormat,
  ArtifactIdentity,
  BindingClass,
  BindingKind,
  ClosureArtifact,
  ClosureRole,
  CompilerConstraint,
  FrozenDirectory,
  InputKey,
  RepoPath,
  StageAssurance,
  StageBinding,
  StageContract,
  StageInputs,
  StageRestrictions,
}
import histos.suite.HistosSpec
import histos.toolchain.HostCompilerSession
import histos.zinc.ProductDigest
import histos.ProjectName

import java.nio.file.{Files, Path}

import zio.test.*
import zio.{durationInt, Ref, ZIO}

object PluginCompileCacheSpec extends HistosSpec:
  def spec = suite("plugin compile cache")(
    test("a second identical compile does not start the compiler") {
      PluginCompileSupport
        .run { (root, _) =>
          compileMark(root, "one").flatMap { first =>
            compileMark(root, "one").map { second =>
              (first.key, second.key)
            }
          }
        }
        .map { ran =>
          assertTrue(ran.starts == 1, ran.value._1 == ran.value._2)
        }
    },
    test("a host api byte change misses the cache") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            jar    <- PluginCompileSupport.apiJar(root.resolve("api.jar"), "alpha-api")
            first  <- compileMark(root, "api", List(jar))
            _      <- PluginCompileSupport.apiJar(root.resolve("api.jar"), "beta-api")
            second <- compileMark(root, "api", List(jar))
          yield (first.key, second.key)
        }
        .map { ran =>
          assertTrue(ran.starts == 2, ran.value._1 != ran.value._2)
        }
    },
    test("a directory byte change misses the cache") {
      PluginCompileSupport
        .run { (root, _) =>
          val base = root.resolve("dir-app")
          for
            session <- prepared(root, "dir")
            source  <- PluginCompileSupport.write(
              base.resolve("src/Mark.scala"),
              """|object Mark:
                 |  def text: String = "one"
                 |""".stripMargin,
            )
            note   <- PluginCompileSupport.write(base.resolve("data/note.txt"), "alpha")
            first  <- compileDir(base, session, source, note)
            _      <- PluginCompileSupport.write(base.resolve("data/note.txt"), "beta")
            second <- compileDir(base, session, source, note)
          yield (first.key, second.key)
        }
        .map { ran =>
          assertTrue(ran.starts == 2, ran.value._1 != ran.value._2)
        }
    },
    test("a cache hit replaces the classes directory") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            first <- compileMark(root, "fresh")
            stale = root.resolve("fresh/classes/Stale.class")
            _      <- ZIO.attemptBlocking(Files.write(stale, Array[Byte](1, 2, 3))).orDie
            second <- compileMark(root, "fresh")
            gone   <- ZIO.attemptBlocking(!Files.exists(stale)).orDie
            names = second.snapshot.outputs.entries.map(_.path.value)
          yield (first.key, second.key, gone, names)
        }
        .map { ran =>
          val (first, second, gone, names) = ran.value
          assertTrue(
            ran.starts == 1,
            first == second,
            gone,
            !names.contains("Stale.class"),
            names.exists(_.endsWith("Mark.class")),
          )
        }
    },
    test("a tasty-only helper change forces the consumer to recompile") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            session   <- prepared(root, "helper")
            left      <- helper(root, session, "aa")
            right     <- helper(root, session, "bb")
            consumerA <- consumer(root, session, left, "aa")
            consumerB <- consumer(root, session, right, "bb")
          yield (left, right, consumerA, consumerB)
        }
        .map { ran =>
          val (left, right, useA, useB) = ran.value
          assertTrue(
            left.digest == right.digest,
            left.product != right.product,
            useA.text == "aa",
            useB.text == "bb",
            directoryClosure(useA.closure),
            directoryClosure(useB.closure),
          )
        }
    },
  ).provideShared(histos.zinc.Compilers.owned) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private def directoryClosure(closure: List[ClosureArtifact]): Boolean =
    closure match
      case ClosureArtifact(ArtifactIdentity(format, _), role) :: Nil =>
        format == ArtifactFormat.Directory && role == ClosureRole.Compile
      case _ => false

  private def compileDir(
      base: Path,
      session: HostCompilerSession,
      source: Path,
      note: Path,
  ): ZIO[PluginCompileSupport.Env & zio.Scope, PluginCompileError, PluginCompileResult] =
    for
      bytes  <- ZIO.attemptBlocking(Files.readAllBytes(note)).orDie
      stage  <- ZIO.fromEither(directoryStage(bytes))
      result <- PluginCompiler.compile(
        PluginCompileJob(
          ProjectName("app"),
          session,
          base,
          base,
          List(source),
          classes = base.resolve("classes"),
          analysis = base.resolve("analysis"),
          stages = List(stage),
        )
      )
    yield result

  private def directoryStage(bytes: Array[Byte]): Either[PluginCompileError, StageContract] =
    for
      file <- PluginCompileSupport.frozen("data/note.txt", bytes)
      path <- RepoPath.from("data").left.map(PluginCompileError.Path(_))
    yield StageContract(
      StageAssurance.HostKnown,
      List(KnownStageRegistry.ScalaQuotes),
      Nil,
      CompilerConstraint.unconstrained,
      List(StageBinding(InputKey.trusted("data"), BindingClass.Public, BindingKind.Directory)),
      StageInputs(Nil, List(FrozenDirectory(path, List(file))), Nil),
      StageRestrictions.v1,
    )

  private def compileMark(
      root: Path,
      name: String,
      api: List[Path] = Nil,
  ): ZIO[PluginCompileSupport.Env & zio.Scope, PluginCompileError, PluginCompileResult] =
    for
      store   <- ZIO.service[histos.cache.ContentStore]
      payload <- PluginCompileSupport
        .payload(root.resolve(name), "3.10.1", PluginCompileSupport.Scala3101)
        .mapError(PluginCompileError.Host(_))
      session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
      source  <- PluginCompileSupport.write(
        root.resolve(s"$name/src/Mark.scala"),
        """|object Mark:
           |  def text: String = "one"
           |""".stripMargin,
      )
      result <- PluginCompiler.compile(
        PluginCompileJob(
          ProjectName("app"),
          session,
          root.resolve(name),
          root.resolve(name),
          List(source),
          apiJars = api,
          classes = root.resolve(s"$name/classes"),
          analysis = root.resolve(s"$name/analysis"),
        )
      )
    yield result

  private final case class HelperOut(
      digest: Sha256,
      product: histos.plugin.ContentHash,
      classes: Path,
      session: histos.toolchain.HostCompilerSession,
  )

  private def prepared(root: Path, name: String) =
    for
      store   <- ZIO.service[histos.cache.ContentStore]
      payload <- PluginCompileSupport
        .payload(root.resolve(s"$name-payload"), "3.10.1", PluginCompileSupport.Scala3101)
        .mapError(PluginCompileError.Host(_))
      session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
    yield session

  private def helper(
      root: Path,
      session: histos.toolchain.HostCompilerSession,
      label: String,
  ): ZIO[PluginCompileSupport.Env & zio.Scope, PluginCompileError, HelperOut] =
    val base = root.resolve(s"helper-$label")
    for
      source <- PluginCompileSupport.write(
        base.resolve("src/helper/Helper.scala"),
        s"""|package helper
            |object Helper:
            |  type Label = "$label"
            |""".stripMargin,
      )
      result <- PluginCompiler.compile(
        PluginCompileJob(
          ProjectName("helper"),
          session,
          base,
          base,
          List(source),
          classes = base.resolve("classes"),
          analysis = base.resolve("analysis"),
        )
      )
      digest <- ZIO
        .fromEither(ProductDigest.ofDirectory(base.resolve("classes"), base))
        .mapError(PluginCompileError.Compile(_))
    yield HelperOut(digest, result.snapshot.outputs.identity.digest, base.resolve("classes"), session)

  private def consumer(
      root: Path,
      session: histos.toolchain.HostCompilerSession,
      helper: HelperOut,
      label: String,
  ): ZIO[PluginCompileSupport.Env & zio.Scope, PluginCompileError, Consumed] =
    val base = root.resolve(s"use-$label")
    for
      source <- PluginCompileSupport.write(
        base.resolve("src/Use.scala"),
        """|object Use:
           |  inline def token: String = scala.compiletime.constValue[helper.Helper.Label]
           |  def text: String = token
           |""".stripMargin,
      )
      result <- PluginCompiler.compile(
        PluginCompileJob(
          ProjectName("app"),
          session,
          base,
          base,
          List(source),
          classpath = List(helper.classes),
          classes = base.resolve("classes"),
          analysis = base.resolve("analysis"),
          helpers = List(ArtifactIdentity(histos.plugin.ArtifactFormat.Jar, helper.product)),
        )
      )
      text <- PluginCompileSupport.invoke(
        base.resolve("classes"),
        PluginCompileSupport.runtimeJars(session),
        "Use$",
        "text",
      )
    yield Consumed(text, result.snapshot.closure)

  private final case class Consumed(text: String, closure: List[ClosureArtifact])
end PluginCompileCacheSpec
