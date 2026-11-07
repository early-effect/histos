package histos.load

import histos.cache.ContentStore
import histos.plugin.ApiDigest
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** A plugin tree is compiled once and reused while `.complete` is present. A type error is the compiler text. */
object PluginCompileSuccessLawSpec extends ZIOSpecDefault:
  private val launcher = ApiDigest.trusted("ab" * 32)

  def spec = suite("plugin compile produces classes")(
    test("a marked tree is reused, a type error is the compiler text, and a reserved class is named"):
      Workspace.work("histos-plugin-compile").flatMap: dir =>
        val tree  = dir.resolve("tree")
        val store = ContentStore.in(dir.resolve("cas"))
        val plug  = tree.resolve("Plug.scala")
        val notes = tree.resolve("notes.txt")
        for
          _        <- Workspace.writeString(plug, good)
          _        <- Workspace.writeString(notes, "note")
          libs     <- libraries
          compiled <- PluginCompile.fromTree(store, launcher, tree, libs)
          first    <- facts(compiled)
          _        <- ZIO.attemptBlocking(Files.delete(compiled.resolve("Plug.class"))).orDie
          reused   <- PluginCompile.fromTree(store, launcher, tree, libs)
          cached   <- facts(reused)
          _        <- Workspace.writeString(plug, bad)
          broken   <- PluginCompile.fromTree(store, launcher, tree, libs).either
          jarPath  <- zip(
            dir.resolve("plugin.jar"),
            List(
              "Plug.scala" -> good.getBytes(StandardCharsets.UTF_8),
              "notes.txt"  -> "note".getBytes(StandardCharsets.UTF_8),
            ),
          )
          packed <- PluginCompile.fromJar(store, launcher, jarPath, Nil, libs)
          jarred <- facts(packed)
          leak   <- Workspace.writeString(dir.resolve("leak").resolve("histos").resolve("leaklaw").resolve("Mark.scala"), mark)
          leaked <- PluginCompile.fromTree(store, launcher, dir.resolve("leak"), libs)
          named  <- PluginCompile.engineClass(leaked)
        yield
          val brokenText = broken match
            case Left(error) => error.message
            case Right(_)    => ""
          assertTrue(
            first.classFile,
            first.note == "note",
            first.marked,
            first.reserved.isEmpty,
            reused == compiled,
            !cached.classFile,
            cached.marked,
            brokenText.contains("String"),
            brokenText.contains("Int"),
            jarred.classFile,
            jarred.note == "note",
            named.exists(_.startsWith("histos.leaklaw.Mark")),
          )
  ) @@ TestAspect.withLiveSystem

  private val good =
    """object Plug:
      |  def ready: Int = 1
      |""".stripMargin

  private val bad =
    """object Plug:
      |  def ready: Int = "no"
      |""".stripMargin

  private val mark =
    """package histos.leaklaw
      |
      |object Mark:
      |  def n: Int = 1
      |""".stripMargin

  private final case class Facts(classFile: Boolean, note: String, marked: Boolean, reserved: Option[String])

  private def libraries: ZIO[Any, Nothing, List[Path]] =
    val found = List(classOf[scala.collection.immutable.Nil.type], classOf[scala.quoted.Quotes]).flatMap(
      Boundary.codeSource
    )
    if found.isEmpty then ZIO.dieMessage("the scala library is not on this classpath")
    else ZIO.succeed(found)

  private def facts(root: Path): ZIO[Any, Nothing, Facts] =
    for
      classFile <- ZIO.attemptBlocking(Files.isRegularFile(root.resolve("Plug.class"))).orDie
      marked    <- ZIO.attemptBlocking(Files.isRegularFile(root.resolve(".complete"))).orDie
      note      <- ZIO.attemptBlocking(Files.readString(root.resolve("notes.txt"))).orDie
      reserved  <- PluginCompile.engineClass(root).catchAll(error => ZIO.dieMessage(error.message))
    yield Facts(classFile, note, marked, reserved)

  private def zip(path: Path, entries: List[(String, Array[Byte])]): ZIO[Any, IoFailure, Path] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val out = new java.util.zip.ZipOutputStream(Files.newOutputStream(path))
        try
          entries.foreach: (name, body) =>
            out.putNextEntry(new java.util.zip.ZipEntry(name))
            out.write(body)
            out.closeEntry()
        finally out.close()
        path
      }
      .mapError(_ => IoFailure(path, IoFault.Write))
end PluginCompileSuccessLawSpec
