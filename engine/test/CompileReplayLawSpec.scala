package histos.zinc

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.schedule.{CacheError, Digest}
import histos.toolchain.RuntimeIdentity
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{Platform, ScalaVersion, ScalacFlags}

import java.io.{ByteArrayOutputStream, DataOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant

import org.objectweb.asm.{ClassWriter, Opcodes}

import sbt.internal.inc.{Analysis, CompileOutput as ZincOutput, FileAnalysisStore}

import slayer.{any, stub, stubbed}

import xsbti.T2
import xsbti.compile.{AnalysisContents, CompileOrder, FileHash, MiniOptions, MiniSetup}

import zio.{Chunk, Scope, ZIO, ZLayer}
import zio.test.*

object CompileReplayLawSpec extends ZIOSpecDefault:
  private val jdk = JdkRuntime(
    Path.of("/jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
  )

  def spec = suite("compile replay laws")(
    test("a key that is not 64 hex digits fails before the store"):
      Workspace
        .work("histos-replay-key")
        .flatMap: dir =>
          val key     = Sha256.trusted("ab")
          val request = cell(dir.resolve("classes"), dir.resolve("analysis"), Nil)
          val program =
            for
              _ <- stub[ContentStore](_.read(any[Digest]))(ZIO.die(new IllegalStateException("read")))
              _ <- stub[ContentStore](_.put(any[Digest], any[Chunk[Byte]], any[Instant]))(
                ZIO.die(new IllegalStateException("put"))
              )
              store  <- ZIO.service[ContentStore]
              loaded <- CompileReplay.load(store, key, request).either
              saved  <- CompileReplay.save(store, key, request).either
            yield assertTrue(
              loaded == Left(CompileError.Io(IoFailure(Path.of(key.value), IoFault.Read))),
              saved == Left(CompileError.Io(IoFailure(Path.of(key.value), IoFault.Read))),
            )
          program.provideSomeLayer[TestEnvironment & Scope](stubbed[ContentStore])
    ,
    test("stored class bytes replace the disk, and an empty analysis keeps an empty api digest"):
      Workspace
        .work("histos-replay-bytes")
        .flatMap: dir =>
          val classes   = dir.resolve("classes")
          val analysis  = dir.resolve("analysis")
          val request   = cell(classes, analysis, List(dir.resolve("A.scala")))
          val key       = Sha256.of(Chunk.fromArray("compile-replay".getBytes(StandardCharsets.UTF_8)))
          val other     = Sha256.of(Chunk.fromArray("other-key".getBytes(StandardCharsets.UTF_8)))
          val original  = classBytes("demo/A", "n")
          val changed   = classBytes("demo/A", "m")
          val classFile = classes.resolve("demo").resolve("A.class")
          val ir        = classes.resolve("demo").resolve("A.sjsir")
          val nir       = classes.resolve("demo").resolve("A.nir")
          val note      = classes.resolve("notes.txt")
          val leftover  = classes.resolve("demo").resolve("B.class")
          val store     = ContentStore.in(dir.resolve("cas"))
          Digest.from(key.value) match
            case None =>
              ZIO.succeed(assertTrue(key.value.length == 32))
            case Some(_) =>
              for
                _        <- writeBytes(classFile, original)
                _        <- writeBytes(ir, "ir".getBytes(StandardCharsets.UTF_8))
                _        <- writeBytes(nir, "native".getBytes(StandardCharsets.UTF_8))
                _        <- Workspace.writeString(note, "note")
                _        <- emptyAnalysis(classes, analysis.resolve("inc.zip"))
                _        <- CompileReplay.save(store, key, request)
                _        <- writeBytes(classFile, changed)
                recalled <- CompileReplay.recall(request, key)
                _        <- writeBytes(leftover, "extra".getBytes(StandardCharsets.UTF_8))
                loaded   <- CompileReplay.load(store, key, request)
                restored <- readBytes(classFile)
                irBytes  <- readBytes(ir)
                noteText <- Workspace.readString(note)
                extra    <- exists(leftover)
                marker   <- exists(analysis.resolve("replay.key"))
                api      <- ZIO.fromEither(ProductDigest.read(analysis).flatMap(_ => ApiDigest.read(analysis)))
                first    <- ZIO.fromEither(ProductDigest.read(analysis))
                missed   <- CompileReplay.load(store, other, request)
                still    <- readBytes(classFile)
                _        <- writeBytes(classFile, changed)
                _        <- CompileReplay.save(store, key, request)
                _        <- CompileReplay.load(store, key, request)
                second   <- ZIO.fromEither(ProductDigest.read(analysis))
                apiAgain <- ZIO.fromEither(ApiDigest.read(analysis))
              yield assertTrue(
                recalled.contains(replayed(request, key)),
                loaded.contains(
                  CompileOutput(
                    classes,
                    3,
                    key,
                    changed = false,
                    sources = Some(SourceCount(0, request.sources.size)),
                    classesWritten = Some(0),
                    miss = None,
                  )
                ),
                restored == Chunk.fromArray(original),
                irBytes == Chunk.fromArray("ir".getBytes(StandardCharsets.UTF_8)),
                noteText == "note",
                extra == false,
                marker,
                api == ApiDigest.empty,
                missed.isEmpty,
                still == restored,
                second != first,
                apiAgain == ApiDigest.empty,
              )
    ,
    test("an analysis that is not zinc restores class bytes and does not stamp a hit"):
      Workspace
        .work("histos-replay-corrupt")
        .flatMap: dir =>
          val classes   = dir.resolve("classes")
          val analysis  = dir.resolve("analysis")
          val request   = cell(classes, analysis, Nil)
          val key       = Sha256.of(Chunk.fromArray("corrupt-replay".getBytes(StandardCharsets.UTF_8)))
          val classFile = classes.resolve("A.class")
          val leftover  = classes.resolve("B.class")
          val store     = ContentStore.in(dir.resolve("cas"))
          Digest.from(key.value) match
            case None =>
              ZIO.succeed(assertTrue(key.value.length == 32))
            case Some(id) =>
              for
                _        <- Workspace.writeString(classFile, "one")
                _        <- Workspace.writeString(analysis.resolve("inc.zip"), "not-zinc")
                _        <- CompileReplay.save(store, key, request)
                _        <- Workspace.writeString(classFile, "two")
                _        <- Workspace.writeString(leftover, "extra")
                _        <- Workspace.writeString(analysis.resolve("replay.key"), "stale")
                loaded   <- CompileReplay.load(store, key, request).either
                restored <- Workspace.readString(classFile)
                extra    <- exists(leftover)
                marker   <- exists(analysis.resolve("replay.key"))
                recalled <- CompileReplay.recall(request, key)
              yield assertTrue(
                loaded == Left(CompileError.Stored(CacheError.Corrupt(id))),
                restored == "one",
                extra == false,
                marker == false,
                recalled.isEmpty,
              )
    ,
    test("an entry that escapes the classes directory leaves the bytes already there"):
      Workspace
        .work("histos-replay-slip")
        .flatMap: dir =>
          val classes   = dir.resolve("classes")
          val analysis  = dir.resolve("analysis")
          val request   = cell(classes, analysis, Nil)
          val key       = Sha256.of(Chunk.fromArray("slip-replay".getBytes(StandardCharsets.UTF_8)))
          val classFile = classes.resolve("A.class")
          val store     = ContentStore.in(dir.resolve("cas"))
          Digest.from(key.value) match
            case None =>
              ZIO.succeed(assertTrue(key.value.length == 32))
            case Some(id) =>
              for
                _        <- Workspace.writeString(classFile, "kept")
                _        <- put(store, id, frame(List("c/../evil" -> "pwn".getBytes(StandardCharsets.UTF_8))))
                loaded   <- CompileReplay.load(store, key, request).either
                restored <- Workspace.readString(classFile)
                inside   <- exists(classes.resolve("evil"))
                beside   <- exists(classes.getParent.resolve("evil"))
              yield assertTrue(
                loaded == Left(CompileError.Stored(CacheError.Corrupt(id))),
                restored == "kept",
                inside == false,
                beside == false,
              )
    ,
    test("recall stays a hit when the class bytes change"):
      Workspace
        .work("histos-replay-hit")
        .flatMap: dir =>
          val classes  = dir.resolve("classes")
          val analysis = dir.resolve("analysis")
          val request  = cell(classes, analysis, Nil)
          val key      = Sha256.of(Chunk.fromArray("recall-bytes".getBytes(StandardCharsets.UTF_8)))
          for
            _      <- Workspace.writeString(classes.resolve("A.class"), "one")
            _      <- Workspace.writeString(analysis.resolve("inc.zip"), "not-zinc")
            _      <- Workspace.writeString(analysis.resolve("replay.key"), key.value + "\n")
            first  <- CompileReplay.recall(request, key)
            _      <- Workspace.writeString(classes.resolve("A.class"), "two")
            second <- CompileReplay.recall(request, key)
          yield assertTrue(first.contains(replayed(request, key)), second == first)
    ,
    test("a missing classes directory does not store a blob"):
      Workspace
        .work("histos-replay-missing")
        .flatMap: dir =>
          val classes  = dir.resolve("classes")
          val analysis = dir.resolve("analysis")
          val request  = cell(classes, analysis, Nil)
          val key      = Sha256.of(Chunk.fromArray("missing-classes".getBytes(StandardCharsets.UTF_8)))
          val store    = ContentStore.in(dir.resolve("cas"))
          Digest.from(key.value) match
            case None =>
              ZIO.succeed(assertTrue(key.value.length == 32))
            case Some(id) =>
              for
                _      <- Workspace.writeString(analysis.resolve("inc.zip"), "not-zinc")
                saved  <- CompileReplay.save(store, key, request).either
                stored <- store.read(id)
              yield assertTrue(
                saved == Left(CompileError.Io(IoFailure(classes, IoFault.Stat))),
                stored.isEmpty,
              ),
  )

  private def cell(classes: Path, analysis: Path, sources: List[Path]): CompileRequest =
    CompileRequest(
      CompileToolchain(ScalaVersion.trusted("3.10.0"), Platform.Jvm, jdk, ScalacFlags.empty),
      sources,
      Nil,
      classes,
      analysis,
      Nil,
      Nil,
    )

  private def replayed(request: CompileRequest, key: Sha256): CompileOutput =
    CompileOutput(
      request.classes,
      0,
      key,
      changed = false,
      sources = Some(SourceCount(0, request.sources.size)),
      classesWritten = Some(0),
      miss = None,
    )

  private def emptyAnalysis(classes: Path, zip: Path): ZIO[Any, IoFailure, Unit] =
    ZIO
      .attemptBlocking {
        Option(zip.getParent).foreach(dir => Files.createDirectories(dir))
        val setup = MiniSetup.of(
          ZincOutput(classes),
          MiniOptions.of(Array.empty[FileHash], Array.empty[String], Array.empty[String]),
          "3.10.0",
          CompileOrder.Mixed,
          true,
          Array.empty[T2[String, String]],
        )
        FileAnalysisStore.binary(zip.toFile).set(AnalysisContents.create(Analysis.empty, setup))
      }
      .mapError(_ => IoFailure(zip, IoFault.Write))
      .unit

  private def classBytes(name: String, method: String): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, name, "", "java/lang/Object", Array.empty[String])
    val init = writer.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", "", Array.empty[String])
    init.visitCode()
    init.visitVarInsn(Opcodes.ALOAD, 0)
    init.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    init.visitInsn(Opcodes.RETURN)
    init.visitMaxs(1, 1)
    init.visitEnd()
    val extra = writer.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, method, "()V", "", Array.empty[String])
    extra.visitCode()
    extra.visitInsn(Opcodes.RETURN)
    extra.visitMaxs(0, 0)
    extra.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def frame(entries: List[(String, Array[Byte])]): Chunk[Byte] =
    val out  = new ByteArrayOutputStream
    val data = new DataOutputStream(out)
    data.write("HC1\n".getBytes(StandardCharsets.US_ASCII))
    entries.foreach: (path, body) =>
      val raw = path.getBytes(StandardCharsets.UTF_8)
      data.writeShort(raw.length)
      data.write(raw)
      data.writeInt(body.length)
      data.write(body)
    data.flush()
    Chunk.fromArray(out.toByteArray)

  private def put(store: ContentStore, id: Digest, bytes: Chunk[Byte]): ZIO[Any, CacheError, Unit] =
    store.put(id, bytes, Instant.EPOCH)

  private def writeBytes(path: Path, bytes: Array[Byte]): ZIO[Any, IoFailure, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  private def readBytes(path: Path): ZIO[Any, IoFailure, Chunk[Byte]] =
    ZIO
      .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(_ => IoFailure(path, IoFault.Read))

  private def exists(path: Path): ZIO[Any, IoFailure, Boolean] =
    ZIO.attemptBlocking(Files.exists(path)).mapError(_ => IoFailure(path, IoFault.Stat))
end CompileReplayLawSpec
