package histos.load

import histos.cache.ContentStore
import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{CompilerConstraint, EntryName, StageAssurance, StageContract, StageInputs, StageRestrictions}
import histos.schedule.Digest
import histos.suite.HistosSpec
import histos.toolchain.HostCompilerSession
import histos.workspace.{IoFailure, IoFault}
import histos.{ScalacFlag, ScalacFlags}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{Chunk, ZIO, durationInt}
import zio.test.*

object PluginCompilePolicySpec extends HistosSpec:
  private val strict     = ScalacFlags.of(ScalacFlag.Deprecation, ScalacFlag.FatalWarnings)
  private val sourceText =
    """|object Warning:
       |  @deprecated("use text", "1") def old: String = "compiled"
       |  def text: String = old
       |""".stripMargin

  private val producer = StageContract(
    StageAssurance.ProducerDeclared,
    List(EntryName("Warning")),
    Nil,
    CompilerConstraint.unconstrained,
    Nil,
    StageInputs(Nil, Nil, Nil),
    StageRestrictions.v1,
  )

  private val tests = suite("plugin compiler policy migration")(
    test("a former-format strict archive cannot bypass the corrected compiler") {
      PluginCompileSupport
        .run { (root, _) =>
          for
            store   <- ZIO.service[ContentStore]
            payload <- PluginCompileSupport
              .payload(root.resolve("payload"), "3.10.1", PluginCompileSupport.Scala3101)
              .mapError(PluginCompileError.Host(_))
            session <- PluginCompileSupport.session(payload, store).mapError(PluginCompileError.Host(_))
            source  <- PluginCompileSupport.write(root.resolve("src/Warning.scala"), sourceText)
            job = PluginCompileJob(
              PluginCompileSupport.App,
              session,
              root,
              root,
              List(source),
              classes = root.resolve("classes"),
              analysis = root.resolve("analysis"),
              stages = List(producer),
              flags = ScalacFlags.empty,
            )
            lenient <- PluginCompiler.compile(job)
            validId <- archiveId(lenient.key)
            valid   <- store
              .read(validId)
              .mapError(error => PluginCompileError.Compile(histos.zinc.CompileError.Stored(error)))
            archive <- ZIO.fromOption(valid).orElseFail(PluginCompileError.Io(IoFailure(root, IoFault.Read)))
            oldKey = formerKey(session, lenient, strict)
            oldId <- archiveId(oldKey)
            now   <- ZIO.clockWith(_.instant)
            _     <- store
              .put(oldId, archive, now)
              .mapError(error => PluginCompileError.Compile(histos.zinc.CompileError.Stored(error)))
            rejected <- PluginCompiler.compile(job.copy(flags = strict)).either
            repeated <- PluginCompiler.compile(job.copy(flags = strict)).either
            retained <- store
              .read(oldId)
              .mapError(error => PluginCompileError.Compile(histos.zinc.CompileError.Stored(error)))
          yield (rejected, repeated, retained.contains(archive))
        }
        .map { ran =>
          val (rejected, repeated, retained) = ran.value
          assertTrue(isStrictFailure(rejected), isStrictFailure(repeated), retained, ran.starts == 3)
        }
    }
  )

  def spec =
    (tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)).provideShared(histos.zinc.Compilers.owned)

  // This fixture freezes format 3 independently of the current key policy.
  private def formerKey(session: HostCompilerSession, compiled: PluginCompileResult, flags: ScalacFlags): Sha256 =
    val lines = List(
      "plugin-compile/3",
      s"scala ${session.manifest.scalaVersion.value}",
      s"jdk ${session.jdk.value}",
      s"flags ${flags.canonical}",
      "bootstrap HISTOS_WORKER=1",
      s"compiler-fingerprint ${session.fingerprint.value}",
    ) ++ compiled.snapshot.sources.map(source =>
      s"source ${source.path.value} ${source.kind} ${source.digest.value} ${source.length}"
    ) ++ List(
      "stage ProducerDeclared entries[Warning] helpers[] compiler[any,any] required[] files[] dirs[] env[] restrict[DeclaredRoot,Prohibited,Prohibited,Prohibited,Prohibited]"
    ) ++ session.tools.map(tool => s"tool ${tool.entry.digest.value}")
    Sha256Digest.of(Chunk.fromArray(lines.mkString("\n").getBytes(StandardCharsets.UTF_8)))

  private def archiveId(key: Sha256): ZIO[Any, PluginCompileError, Digest] =
    val hash = Sha256Digest.of(Chunk.fromArray(s"plugin-classes\n${key.value}".getBytes(StandardCharsets.UTF_8)))
    ZIO
      .fromOption(Digest.from(hash.value))
      .orElseFail(PluginCompileError.Io(IoFailure(Path.of("plugin-classes"), IoFault.Read)))

  private def isStrictFailure(result: Either[PluginCompileError, PluginCompileResult]): Boolean = result match
    case Left(PluginCompileError.Worker(code, detail)) => code != 0 && detail.contains("warnings")
    case Left(PluginCompileError.Stage(_)) | Left(PluginCompileError.UnknownStage(_)) | Left(
          PluginCompileError.Compile(_)
        ) | Left(PluginCompileError.Host(_)) | Left(PluginCompileError.Child(_)) | Left(PluginCompileError.Seal(_)) |
        Left(PluginCompileError.Io(_)) | Left(PluginCompileError.Linked(_)) | Left(PluginCompileError.InputChanged(_)) |
        Left(PluginCompileError.Path(_)) | Right(_) =>
      false
end PluginCompilePolicySpec
