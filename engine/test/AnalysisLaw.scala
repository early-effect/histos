package histos.zinc

import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.ZIO
import zio.test.{assertTrue, suite, test}

/** Source stamps and the incremental compiler surface. No scalac and no child.
  */
object AnalysisLaw:
  def laws =
    suite("analysis")(missing, moved, hops, surface)

  private def missing =
    test("A missing analysis is every current source, and an unreadable file is unknown."):
      Workspace
        .work("histos-analysis")
        .flatMap: root =>
          val later   = root.resolve("Later.scala")
          val sooner  = root.resolve("Sooner.scala")
          val sources = List(later, sooner)
          val dir     = root.resolve("analysis")
          for
            _       <- Workspace.writeString(later, "object Later\n")
            _       <- Workspace.writeString(sooner, "object Sooner\n")
            initial <- SourceStale.read(dir, sources)
            _       <- Workspace.writeString(dir.resolve(AnalysisFile.name), "not an analysis")
            unread  <- SourceStale.read(dir, sources)
          yield assertTrue(
            initial == FileStaleness.Initial(sources),
            unread == FileStaleness.Unknown,
          )

  private def moved =
    test("A matching stamp is fresh, a changed source marks every dependent, and a new file is stale."):
      Workspace
        .work("histos-analysis")
        .flatMap: root =>
          val parent  = root.resolve("Parent.scala")
          val child   = root.resolve("Child.scala")
          val created = root.resolve("New.scala")
          val dir     = root.resolve("analysis")
          val pair    = List(parent, child)
          for
            _ <- Workspace.writeString(parent, "object Parent\n")
            _ <- Workspace.writeString(child, "object Child\n")
            _ <- remember(
              dir.resolve(AnalysisFile.name),
              root.resolve("classes"),
              List(parent       -> "demo.Parent", child -> "demo.Child"),
              List("demo.Child" -> "demo.Parent"),
            )
            fresh  <- SourceStale.read(dir, pair)
            _      <- Workspace.writeString(parent, "object Parent { def n = 1 }\n")
            marked <- SourceStale.read(dir, pair)
            _      <- Workspace.writeString(created, "object New\n")
            added  <- SourceStale.read(dir, pair :+ created)
          yield assertTrue(
            fresh == FileStaleness.Known(Nil),
            marked == FileStaleness.Known(pair),
            added == FileStaleness.Known(pair :+ created),
          )

  private def hops =
    test("A member-ref child and the grandchild two hops away are stale when the parent changes."):
      Workspace
        .work("histos-analysis")
        .flatMap: root =>
          val parent  = root.resolve("Parent.scala")
          val child   = root.resolve("Child.scala")
          val grand   = root.resolve("Grand.scala")
          val other   = root.resolve("Other.scala")
          val dir     = root.resolve("analysis")
          val sources = List(parent, child, grand, other)
          for
            _ <- Workspace.writeString(parent, "object Parent\n")
            _ <- Workspace.writeString(child, "object Child\n")
            _ <- Workspace.writeString(grand, "object Grand\n")
            _ <- Workspace.writeString(other, "object Other\n")
            _ <- remember(
              dir.resolve(AnalysisFile.name),
              root.resolve("classes"),
              List(
                parent -> "demo.Parent",
                child  -> "demo.Child",
                grand  -> "demo.Grand",
                other  -> "demo.Other",
              ),
              List("demo.Child" -> "demo.Parent", "demo.Grand" -> "demo.Child"),
            )
            _     <- Workspace.writeString(parent, "object Parent { def n = 1 }\n")
            stale <- SourceStale.read(dir, sources)
          yield assertTrue(stale == FileStaleness.Known(List(parent, child, grand)))

  private def surface =
    test(
      "Reading the incremental surface leaves a fixture class file and writes no inc.zip. That surface exposes compile and compileAllJava."
    ):
      Workspace
        .work("histos-analysis")
        .flatMap: root =>
          val kept = root.resolve("classes/demo/Keep.class")
          val body = "fixture class\n"
          for
            _ <- Workspace.writeString(kept, body)
            exposed = classOf[xsbti.compile.IncrementalCompiler].getMethods.map(_.getName).toSet
            after <- Workspace.readString(kept)
            wrote <- wroteArchive(root)
          yield assertTrue(
            after == body,
            !wrote,
            exposed.contains("compile"),
            exposed.contains("compileAllJava"),
          )

  private def remember(
      zip: Path,
      classes: Path,
      sources: List[(Path, String)],
      depends: List[(String, String)],
  ) =
    ZIO.attempt(StoredAnalysis.put(zip, classes, sources, depends))

  private def wroteArchive(root: Path) =
    ZIO.attemptBlocking:
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.exists: path =>
          Option(path.getFileName).exists(_.toString == AnalysisFile.name)
      finally walk.close()
end AnalysisLaw
