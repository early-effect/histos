package histos.cli

import histos.pack.LazyVals

import java.nio.file.Path
import java.util.zip.ZipFile

import org.objectweb.asm.tree.{ClassNode, MethodInsnNode}

import scala.jdk.CollectionConverters.*

import zio.test.*

object LazyValsSpec extends ZIOSpecDefault:
  private val zioCli = suite("zio-cli lazy vals")(
    test("Args.Both no longer calls getOffsetStatic"):
      jar("zio-cli_3-") match
        case None       => assertTrue(false)
        case Some(path) =>
          val bytes = read(path, "zio/cli/Args$Both.class")
          LazyVals.catalog(List("zio/cli/Args$Both.class" -> bytes)) match
            case Left(error)    => assertTrue(error.message.isEmpty)
            case Right(handles) =>
              LazyVals.rewrite("zio/cli/Args$Both.class", bytes, handles, getClass.getClassLoader) match
                case Left(error)      => assertTrue(handles.size == 5 && error.message.isEmpty)
                case Right(rewritten) =>
                  assertTrue(
                    !calls(rewritten, "getOffsetStatic"),
                    !calls(rewritten, "objCAS"),
                    calls(rewritten, "findVarHandle"),
                  )
  )

  private def jar(prefix: String): Option[Path] =
    val separator = java.io.File.pathSeparator
    java.lang.System
      .getProperty("java.class.path")
      .split(separator)
      .toList
      .collectFirst:
        case entry if Path.of(entry).getFileName.toString.startsWith(prefix) => Path.of(entry)

  private def read(path: Path, name: String): Array[Byte] =
    val zip = new ZipFile(path.toFile)
    try
      val entry = zip.getEntry(name)
      val input = zip.getInputStream(entry)
      try input.readAllBytes()
      finally input.close()
    finally zip.close()

  private def calls(bytes: Array[Byte], name: String): Boolean =
    val node = new ClassNode()
    new org.objectweb.asm.ClassReader(bytes).accept(node, 0)
    node.methods.asScala.exists: method =>
      method.instructions.iterator.asScala.exists:
        case call: MethodInsnNode => call.name == name
        case _                    => false

  def spec = zioCli
end LazyValsSpec
