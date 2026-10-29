package histos.testrun

import histos.jdk.{Sha256, Sha256Error}
import histos.native.{ClassShapes, NativeOrigin}
import histos.resolve.{BuildResolvers, DependencyLock, LockedCell, LockedTools, Resolve}
import histos.toolchain.BuildCompiler
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{ApiDigest, GraphCompile}
import histos.{Jvm, Module, Platform, ProjectGraph, ProjectName, Projects, ScalaVersion, module}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import org.objectweb.asm.{ClassWriter, Opcodes}

import zio.{Chunk, Scope, ZIO}
import zio.test.{assertTrue, suite, test}

/** Suite stamp and the quick record. No compiler and no child.
  */
object StampLaw:
  private val suiteClass = "keep.Suite"
  private val suiteName  = SuiteName("keep.Suite")
  private val app        = ProjectName("app")
  private val core       = ProjectName("core")

  def laws =
    suite("digest")(
      indexed,
      referenced,
      moved,
      missed,
      unparsed,
      blocked,
      chosen,
      unnamed,
      matched,
      shaped,
      hashed,
      refused,
    )

  def gaps =
    test("a missing upstream API file adds no line, and an unreadable file is a gap"):
      BuildCompiler.version.flatMap: version =>
        prepared(version).flatMap: (repo, ask) =>
          val quiet = SuiteCover.stored(repo, ask) match
            case Right(cover) => Right(cover.apis)
            case Left(gap)    => Left(gap.message)
          analysis(repo, version) match
            case None      => ZIO.dieMessage("classes path has no analysis directory")
            case Some(dir) =>
              val path = dir.resolve(ApiDigest.fileName)
              Workspace
                .writeString(path, "unreadable\n")
                .flatMap: _ =>
                  deny(path).flatMap: _ =>
                    val blocked = SuiteCover.stored(repo, ask) match
                      case Left(CoverGap.NoApi(name, found, platform)) =>
                        Right((name, found, platform))
                      case Left(gap) => Left(gap.message)
                      case Right(_)  => Left("loaded")
                    ZIO.succeed:
                      assertTrue(
                        quiet == Right(Nil),
                        blocked == Right((core, version, Platform.Jvm)),
                      )

  private def indexed =
    test("one class index stamps a suite the same way"):
      classes("keep/Suite", "keep/Other").flatMap: root =>
        val cover = SuiteStamp.Cover(Nil, Nil, "jdk", Nil, Nil)
        for
          tree  <- SuiteStamp.index(List(root))
          suite <- ZIO.fromEither(SuiteStamp.digest(suiteClass, tree, cover))
          other <- ZIO.fromEither(SuiteStamp.digest("keep.Other", tree, cover))
          again <- SuiteStamp.digest(suiteClass, List(root), cover)
          rest  <- SuiteStamp.digest("keep.Other", List(root), cover)
        yield assertTrue(suite == again, other == rest, suite != other, suite.isDefined)

  private def referenced =
    test("a referenced class is in the digest and an unreferenced class is not"):
      val cover = SuiteStamp.Cover(Nil, Nil, "jdk", Nil, Nil)
      classes("keep/Suite", "keep/Used", "keep/Other").flatMap: root =>
        SuiteStamp
          .digest(suiteClass, List(root), cover)
          .flatMap: before =>
            writeClass(root, "keep/Used", "java/lang/Object", field = true).flatMap: _ =>
              SuiteStamp
                .digest(suiteClass, List(root), cover)
                .flatMap: used =>
                  writeClass(root, "keep/Other", "java/lang/Object", field = true).flatMap: _ =>
                    SuiteStamp
                      .digest(suiteClass, List(root), cover)
                      .map: other =>
                        assertTrue(before.isDefined, used != before, other == used)

  private def moved =
    test("upstream jars, the JDK, the flags, and an API digest each change the digest"):
      val base  = SuiteStamp.Cover(Nil, List("up-a"), "jdk-a", Nil, List("core aaa"))
      val forms = List(
        base.copy(upstream = List("up-b")),
        base.copy(jdk = "jdk-b"),
        base.copy(flags = List("-encoding", "UTF-8")),
        base.copy(apis = List("core bbb")),
      )
      classes("keep/Suite").flatMap: root =>
        ZIO
          .foreach(base :: forms)(cover => SuiteStamp.digest(suiteClass, List(root), cover))
          .map: found =>
            found.collect { case Some(sha) => sha } match
              case first :: rest => assertTrue(rest.size == 4, rest.forall(_ != first))
              case Nil           => assertTrue(false)

  private def missed =
    test("a stored success misses when the upstream jar, the JDK, or the flags differ"):
      val base  = SuiteStamp.Cover(Nil, List("up-a"), "jdk-a", Nil, Nil)
      val forms = List(
        base.copy(upstream = List("up-b")),
        base.copy(jdk = "jdk-b"),
        base.copy(flags = List("-encoding", "UTF-8")),
      )
      BuildCompiler.version.flatMap: version =>
        classes("keep/Suite").flatMap: root =>
          Workspace
            .work("histos-stamp-record")
            .flatMap: repo =>
              digests(root, base :: forms).flatMap: found =>
                found.collect { case Some(sha) => sha } match
                  case kept :: changed if changed.size == 3 =>
                    QuickRecord
                      .save(repo, app, version, Platform.Jvm, suiteName, kept)
                      .flatMap: _ =>
                        QuickRecord
                          .hit(repo, app, version, Platform.Jvm, suiteName, kept)
                          .flatMap: held =>
                            ZIO
                              .foreach(changed)(sha =>
                                QuickRecord.hit(repo, app, version, Platform.Jvm, suiteName, sha)
                              )
                              .map: hits =>
                                assertTrue(held, hits == List(false, false, false))
                  case _ => ZIO.succeed(assertTrue(false))

  private def unparsed =
    test("a class file that cannot be parsed is a stamp error"):
      Workspace
        .work("histos-stamp-class")
        .flatMap: root =>
          val path = root.resolve("keep").resolve("Broken.class")
          writeBytes(path, Array[Byte](1, 2, 3, 4)).flatMap: _ =>
            SuiteStamp
              .index(List(root))
              .either
              .map: found =>
                assertTrue(found == Left(StampError.Class(path)))

  private def blocked =
    test("an unreadable class directory is a stamp error"):
      Workspace
        .work("histos-stamp-sealed")
        .flatMap: root =>
          deny(root).flatMap: _ =>
            SuiteStamp
              .index(List(root))
              .either
              .map: found =>
                assertTrue(found == Left(StampError.Io(IoFailure(root, IoFault.Walk))))

  private def chosen =
    test("a ZIO spec is chosen and a helper is not"):
      val spec   = ClassShapes.Shape("demo.Check$", module = true, parents = List("zio.test.ZIOSpecAbstract"))
      val helper = ClassShapes.Shape("demo.Help", module = false, parents = List("java.lang.Object"))
      val world  = Map(spec.name -> spec, helper.name -> helper)
      val marks  = List(SuiteNames.Mark("zio.test.ZIOSpecAbstract", module = true))
      assertTrue(SuiteNames.choose(List(spec, helper), world, marks) == List("demo.Check"))

  private def unnamed =
    test("an empty suite name is not a suite"):
      assertTrue(SuiteName.from("") == Left(SuiteNameError.Empty))

  private def matched =
    test("a stored success matches only that digest"):
      val kept  = Sha256.of(Chunk.fromArray("kept".getBytes(StandardCharsets.UTF_8)))
      val other = Sha256.of(Chunk.fromArray("other".getBytes(StandardCharsets.UTF_8)))
      BuildCompiler.version.flatMap: version =>
        Workspace
          .work("histos-stamp-match")
          .flatMap: repo =>
            QuickRecord
              .save(repo, app, version, Platform.Jvm, suiteName, kept)
              .flatMap: _ =>
                QuickRecord
                  .hit(repo, app, version, Platform.Jvm, suiteName, kept)
                  .flatMap: held =>
                    QuickRecord
                      .hit(repo, app, version, Platform.Jvm, suiteName, other)
                      .map: missed =>
                        assertTrue(held, !missed, kept != other)

  private def shaped =
    test("a quick record that is not a stored success is an error"):
      record("nope\n").flatMap: (path, found) =>
        ZIO.succeed(assertTrue(found == Left(QuickError.Shape(path))))

  private def hashed =
    test("a quick record whose digest is not a sha256 is an error"):
      record("success not-a-sha\n").flatMap: (path, found) =>
        ZIO.succeed(assertTrue(found == Left(QuickError.Hash(path, Sha256Error.Bad("not-a-sha")))))

  private def refused =
    test("a failed save is the write error and a failed clear is the delete error"):
      BuildCompiler.version.flatMap: version =>
        val kept = Sha256.of(Chunk.fromArray("kept".getBytes(StandardCharsets.UTF_8)))
        Workspace
          .work("histos-stamp-save")
          .flatMap: repo =>
            val path = QuickRecord.file(repo, app, version, Platform.Jvm, suiteName)
            Option(path.getParent) match
              case None         => ZIO.dieMessage("quick record has no parent")
              case Some(parent) =>
                Workspace
                  .createDirectories(parent)
                  .flatMap: _ =>
                    mode(parent, "r-x------").flatMap: _ =>
                      QuickRecord
                        .save(repo, app, version, Platform.Jvm, suiteName, kept)
                        .either
                        .flatMap: saved =>
                          Workspace
                            .work("histos-stamp-clear")
                            .flatMap: other =>
                              val clearPath = QuickRecord.file(other, app, version, Platform.Jvm, suiteName)
                              Workspace
                                .createDirectories(clearPath)
                                .flatMap: _ =>
                                  writeBytes(clearPath.resolve("child"), Array[Byte](1)).flatMap: _ =>
                                    QuickRecord
                                      .clear(other, app, version, Platform.Jvm, suiteName)
                                      .either
                                      .map: cleared =>
                                        assertTrue(
                                          saved == Left(QuickError.Save(IoFailure(path, IoFault.Write))),
                                          cleared == Left(QuickError.Clear(IoFailure(clearPath, IoFault.Delete))),
                                        )

  private def prepared(version: ScalaVersion) =
    val coreDef = module("core", Jvm)(version)
    val appDef  = module("app", Jvm)(version).dependsOn(coreDef)
    for
      jdk   <- GraphCompile.jdkHere
      graph <- ZIO.fromEither(Projects(coreDef, appDef))
      root  <- Workspace.work("histos-stamp-gap")
      _     <- Workspace.writeString(Resolve.lockPath(root), lockText(graph))
      _     <- histos.ToolFixture.seed(root, graph)
      lock  <- Resolve.readLock(root)
      found <- both(graph)
    yield (
      root,
      SuiteAsk(
        found._2,
        List(found._1, found._2),
        version,
        Platform.Jvm,
        lock,
        BuildResolvers(graph.resolvers),
        graph.tools,
        NativeOrigin.Central,
        jdk.identity,
        None,
      ),
    )

  private def both(graph: ProjectGraph): ZIO[Any, Nothing, (Module, Module)] =
    val modules = modulesOf(graph)
    (modules.find(_.name == core), modules.find(_.name == app)) match
      case (Some(upstream), Some(leaf)) => ZIO.succeed((upstream, leaf))
      case _                            => ZIO.dieMessage("stamp graph is missing a module")

  private def analysis(repo: Path, version: ScalaVersion): Option[Path] =
    GraphCompile.analysisDirOf(GraphCompile.classesDir(repo, core, version, Platform.Jvm))

  private def digests(root: Path, covers: List[SuiteStamp.Cover]) =
    ZIO.foreach(covers)(cover => SuiteStamp.digest(suiteClass, List(root), cover))

  private def classes(names: String*): ZIO[Scope, IoFailure, Path] =
    Workspace
      .work("histos-stamp-classes")
      .flatMap: root =>
        ZIO
          .foreachDiscard(names.toList): name =>
            val parent = if name == "keep/Suite" then "keep/Used" else "java/lang/Object"
            writeClass(root, name, parent, field = false)
          .as(root)

  private def record(text: String) =
    BuildCompiler.version.flatMap: version =>
      Workspace
        .work("histos-stamp-record")
        .flatMap: repo =>
          val path = QuickRecord.file(repo, app, version, Platform.Jvm, suiteName)
          Workspace
            .writeString(path, text)
            .flatMap: _ =>
              QuickRecord.hit(repo, app, version, Platform.Jvm, suiteName, zero).either.map(found => (path, found))

  private def zero =
    Sha256.of(Chunk.fromArray("zero".getBytes(StandardCharsets.UTF_8)))

  private def writeClass(root: Path, internal: String, parent: String, field: Boolean) =
    writeBytes(root.resolve(internal + ".class"), classBytes(internal, parent, field))

  private def writeBytes(path: Path, bytes: Array[Byte]): ZIO[Any, IoFailure, Unit] =
    val made = Option(path.getParent) match
      case Some(parent) => Workspace.createDirectories(parent)
      case None         => ZIO.dieMessage("class has no parent")
    made *> ZIO
      .attemptBlocking(Files.write(path, bytes))
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  private def classBytes(internal: String, parent: String, field: Boolean): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, internal, null, parent, null)
    if field then Option(writer.visitField(Opcodes.ACC_PUBLIC, "extra", "I", null, null)).foreach(_.visitEnd())
    writer.visitEnd()
    writer.toByteArray

  private def deny(path: Path): ZIO[Scope, IoFailure, Unit] =
    mode(path, "---------")

  private def mode(path: Path, denied: String): ZIO[Scope, IoFailure, Unit] =
    ZIO
      .acquireRelease(
        ZIO
          .attemptBlocking(Files.setPosixFilePermissions(path, PosixFilePermissions.fromString(denied)))
          .mapError(_ => IoFailure(path, IoFault.Write))
      )(_ =>
        ZIO
          .attemptBlocking(Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwx------")))
          .ignore
      )
      .unit

  private def modulesOf(graph: ProjectGraph): List[Module] =
    graph.projects.collect { case item: Module => item }

  private def lockText(graph: ProjectGraph): String =
    val cells = modulesOf(graph).flatMap: item =>
      item.versions.all.flatMap: version =>
        item.platforms.all.map: platform =>
          LockedCell(item.name, version, platform, Nil, Nil, Nil)
    DependencyLock.render(DependencyLock(cells))
end StampLaw
