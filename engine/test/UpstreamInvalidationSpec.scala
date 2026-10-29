package histos.zinc

import histos.native.NativeOrigin
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildProcess, ChildRequest}
import histos.{Jvm, Platform, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO, durationInt}
import zio.test.*

/** A public change in an upstream class file must not leave the downstream class calling the old descriptor.
  */
object UpstreamInvalidationSpec extends histos.SuiteClock:
  private val version = ScalaVersion("3.9.0")

  private val upstreamSource =
    """object Upstream:
      |  def greet(name: String): String = name
      |""".stripMargin

  private val upstreamChanged =
    """object Upstream:
      |  def greet(name: String, n: Int): String = name
      |""".stripMargin

  private val downstreamSource =
    """object Downstream:
      |  def main(args: Array[String]): Unit =
      |    println(Upstream.greet("ok"))
      |""".stripMargin

  private val asked = suite("classpath analysis")(
    test(
      "a public descriptor change invalidates the downstream class"
    ):
      ZIO.scoped:
        for
          jdk   <- GraphCompile.jdkHere
          root  <- temp
          built <- ZIO
            .attempt {
              val upstream   = module("upstream", Jvm)(version)
              val downstream =
                module("downstream", Jvm)(version).dependsOn(upstream)
              write(
                root,
                "upstream/src/main/scala/Upstream.scala",
                upstreamSource,
              )
              write(
                root,
                "downstream/src/main/scala/Downstream.scala",
                downstreamSource,
              )
              write(
                root,
                ".histos/dependencies.lock",
                "cell upstream 3.9.0 jvm\ncell downstream 3.9.0 jvm\n",
              )
              Projects(upstream, downstream)
            }
            .flatMap(ZIO.fromEither(_))
          first <- histos.ToolFixture
            .at(root, built)(
              GraphCompile.run(
                root,
                built,
                Nil,
                jdk,
                NativeOrigin.Central,
                None,
                _ => ZIO.unit,
              )
            )
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          work = root.resolve("run")
          _   <- ZIO.attempt(Files.createDirectories(work))
          ran <- run(jdk, first, work)
          before = first
            .collectFirst:
              case cell if cell.module.value == "downstream" =>
                classMarks(cell.classes)
            .getOrElse(Map.empty)
          _ <- ZIO.attempt(
            write(
              root,
              "upstream/src/main/scala/Upstream.scala",
              upstreamChanged,
            )
          )
          second <- histos.ToolFixture
            .at(root, built)(
              GraphCompile
                .console(root, built, Nil, jdk, NativeOrigin.Central, None)
                .runCollect
            )
            .either
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          feeds = second match
            case Right(found) => found
            case Left(_)      => Chunk.empty
          failure = second match
            case Left(error) => Some(reported(error))
            case Right(_)    => None
        yield assertTrue(
          ran.exit.value == 0,
          ran.out.text.trim == "ok",
          failure == None,
          upstreamRebuilt(feeds),
          downstreamInvalidated(feeds, before),
        )
  )

  private def reported(
      error: GraphCompileError | histos.resolve.ResolveError | histos.workspace.IoFailure
  ): String =
    error match
      case found: GraphCompileError           => found.message
      case found: histos.resolve.ResolveError => found.message
      case found: histos.workspace.IoFailure  => found.message

  private def upstreamRebuilt(feeds: Chunk[CompileFeed]): Boolean =
    feeds.exists:
      case CompileFeed.Done(cell) if cell.module.value == "upstream" =>
        cell.changed || cell.classesWritten.exists(_ > 0)
      case _ => false

  /** A green second compile that keeps the old class file is the bug. A typed error means Zinc recompiled the unchanged
    * downstream source.
    */
  private def downstreamInvalidated(
      feeds: Chunk[CompileFeed],
      before: Map[String, ClassMark],
  ): Boolean =
    feeds.exists:
      case CompileFeed.Failed(
            GraphCompileError.Failed(
              module,
              _,
              _,
              CompileError.Failed(diagnostics),
            )
          ) =>
        module.value == "downstream" && diagnostics.exists(diagnostic => diagnostic.text.contains("greet"))
      case CompileFeed.Done(cell) if cell.module.value == "downstream" =>
        cell.classesWritten.exists(_ > 0) || classMarks(cell.classes) != before
      case _ => false

  private def run(
      jdk: histos.jdk.JdkRuntime,
      cells: List[CompiledCell],
      work: Path,
  ) =
    val down = cells.collect:
      case cell if cell.module.value == "downstream" => cell
    val up = cells.collect:
      case cell if cell.module.value == "upstream" => cell.classes
    (down, up) match
      case (cell :: Nil, classes :: Nil) =>
        JvmCompile
          .runtime(version)
          .flatMap: library =>
            ChildProcess.run(
              ChildRequest(
                Platform.Jvm,
                jdk.home.resolve("bin").resolve("java"),
                ChildClasspath(cell.classes, classes :: library),
                MainClass.trusted("Downstream"),
                work,
              )
            )
      case _ =>
        ZIO.fail(
          histos.process.ChildError.Classes(
            work.resolve("downstream-classes")
          )
        )

  private final case class ClassMark(modified: FileTime, bytes: Long)

  private def classMarks(root: Path): Map[String, ClassMark] =
    val found = List("Downstream.class", "Downstream$.class").flatMap: name =>
      val path = root.resolve(name)
      if Files.isRegularFile(path) then
        try
          Some(
            name -> ClassMark(
              Files.getLastModifiedTime(path),
              Files.size(path),
            )
          )
        catch case _: java.io.IOException => None
      else None
    found.toMap

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-upstream"))
    ): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths
            .sortBy(_.getNameCount)
            .reverse
            .foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec = asked.provideSomeLayerShared[
    zio.test.TestEnvironment & zio.Scope
  ](
    histos.ToolFixture.jvm(version)
  ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)
end UpstreamInvalidationSpec
