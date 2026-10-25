package histos.zinc

import java.nio.file.Path
import java.nio.file.attribute.FileTime

import zio.test.*

/** A failed class-file mark is an unknown write count, not zero and not the directory census.
  */
object ClassMarkSpec extends ZIOSpecDefault:
  private val earlier = FileTime.fromMillis(1_700_000_000_000L)
  private val later   = FileTime.fromMillis(1_700_000_030_000L)

  private val kept   = Path.of("Kept.class")
  private val edited = Path.of("Edited.class")
  private val added  = Path.of("Added.class")
  private val broken = Path.of("Broken.class")
  private val extra  = Path.of("Extra.class")
  private val note   = Path.of("README.txt")

  private val same  = JvmCompile.ClassMark(earlier, 10)
  private val moved = JvmCompile.ClassMark(later, 12)
  private val fresh = JvmCompile.ClassMark(later, 4)

  private val prior = Map(kept -> same, edited -> same)

  def spec = suite("class file marks")(
    test(
      "a failed mark is an unknown write count, a new file counts, and an unchanged mark does not"
    ):
      val poisoned = JvmCompile.foldWritten(
        Some(prior),
        Some(
          List(
            kept   -> JvmCompile.ClassStamp.Read(same),
            added  -> JvmCompile.ClassStamp.Read(fresh),
            broken -> JvmCompile.ClassStamp.Failed,
            extra  -> JvmCompile.ClassStamp.Read(fresh),
          )
        ),
      )
      val counted = JvmCompile.foldWritten(
        Some(prior),
        Some(
          List(
            kept   -> JvmCompile.ClassStamp.Read(same),
            edited -> JvmCompile.ClassStamp.Read(moved),
            added  -> JvmCompile.ClassStamp.Read(fresh),
            note   -> JvmCompile.ClassStamp.Skip,
          )
        ),
      )
      val unreadBefore =
        JvmCompile.foldWritten(
          None,
          Some(List(added -> JvmCompile.ClassStamp.Read(fresh))),
        )
      val unreadWalk  = JvmCompile.foldWritten(Some(prior), None)
      val noneWritten = JvmCompile.foldWritten(
        Some(prior),
        Some(
          List(
            kept   -> JvmCompile.ClassStamp.Read(same),
            edited -> JvmCompile.ClassStamp.Read(same),
          )
        ),
      )
      assertTrue(
        poisoned == None,
        counted == Some(2),
        unreadBefore == None,
        unreadWalk == None,
        noneWritten == Some(0),
      )
  )
end ClassMarkSpec
