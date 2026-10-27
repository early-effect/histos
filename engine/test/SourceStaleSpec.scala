package histos.zinc

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

object SourceStaleSpec extends ZIOSpecDefault:
  private val asked = suite("source staleness from an existing analysis")(
    test("a missing analysis is every current source, and an unreadable file is unknown"):
      ZIO.scoped {
        for
          root <- keep
          source = root.resolve("Core.scala")
          _ <- ZIO.attempt(Files.writeString(source, "object Core"))
          missing <- SourceStale.read(root.resolve("absent"), List(source))
          zip = root.resolve("bad").resolve("inc.zip")
          _ <- ZIO.attempt {
            Files.createDirectories(zip.getParent)
            Files.writeString(zip, "not an analysis")
          }
          unread <- SourceStale.read(zip.getParent, List(source))
        yield assertTrue(
          missing == FileStaleness.Initial(List(source)),
          !Files.exists(root.resolve("absent")),
          unread == FileStaleness.Unknown,
        )
      }
    ,
    test("a matching stamp is fresh, a changed source marks its direct dependent, and a new file is stale"):
      ZIO.scoped {
        for
          root <- keep
          core = root.resolve("Core.scala")
          used = root.resolve("Use.scala")
          next = root.resolve("Next.scala")
          far  = root.resolve("Far.scala")
          _ <- ZIO.attempt {
            Files.writeString(core, "object Core")
            Files.writeString(used, "object Use")
            Files.writeString(next, "object Next")
            Files.writeString(far, "object Far")
          }
          classes = root.resolve("classes")
          zip     = root.resolve("analysis").resolve("inc.zip")
          _ <- ZIO.attempt {
            StoredAnalysis.put(
              zip,
              classes,
              List(core -> "demo.Core", used -> "demo.Use", far -> "demo.Far"),
              List("demo.Use" -> "demo.Core", "demo.Far" -> "demo.Use"),
            )
            val kept = classes.resolve("Keep.class")
            Files.createDirectories(kept.getParent)
            Files.write(kept, Array[Byte](1, 2, 3))
            Files.setLastModifiedTime(kept, FileTime.fromMillis(1700000000000L))
            Files.setLastModifiedTime(zip, FileTime.fromMillis(1700000000000L))
          }
          before <- census(classes)
          zipBefore <- ZIO.attempt(Files.readAllBytes(zip).toList)
          fresh <- SourceStale.read(zip.getParent, List(core, used, far))
          _     <- ZIO.attempt(Files.writeString(core, "object Core { def n = 1 }"))
          stale <- SourceStale.read(zip.getParent, List(core, used, far))
          added <- SourceStale.read(zip.getParent, List(core, used, next, far))
          gone  <- SourceStale.read(zip.getParent, List(used, far))
          after <- census(classes)
          zipAfter <- ZIO.attempt(Files.readAllBytes(zip).toList)
          zipTime  <- ZIO.attempt(Files.getLastModifiedTime(zip))
        yield assertTrue(
          fresh == FileStaleness.Known(Nil),
          stale == FileStaleness.Known(List(core, used)),
          added == FileStaleness.Known(List(core, used, next)),
          gone == FileStaleness.Known(List(used)),
          before == after,
          zipBefore == zipAfter,
          zipTime == FileTime.fromMillis(1700000000000L),
          !Files.exists(root.resolve("inc.zip")),
        )
      },
  )

  private final case class Mark(path: String, time: FileTime, bytes: Long)

  private def census(root: Path): ZIO[Any, Throwable, List[Mark]] =
    ZIO.attempt {
      if !Files.exists(root) then Nil
      else
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.filter(path => Files.isRegularFile(path)).sortBy(_.toString).map: path =>
          val name = root.relativize(path).iterator.asScala.map(_.toString).mkString("/")
          Mark(name, Files.getLastModifiedTime(path), Files.size(path))
    }

  private def keep: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-source-stale")))(delete)

  private def delete(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  def spec = asked
end SourceStaleSpec
