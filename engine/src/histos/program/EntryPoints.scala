package histos.program

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}

import histos.workspace.{IoFailure, IoFault}

import zio.ZIO

/** Classes in a directory that declare `public static void main(String[])`. */
object EntryPoints:
  def mains(root: Path): zio.IO[IoFailure, List[String]] =
    ZIO.attemptBlocking(read(root)).mapError(_ => IoFailure(root, IoFault.Walk))

  private def read(root: Path): List[String] =
    if !Files.isDirectory(root) then Nil
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.flatMap(file => mainOf(root, file)).toList.distinct.sorted
      finally walk.close()

  private def mainOf(root: Path, path: Path): Option[String] =
    Option(path.getFileName).map(_.toString) match
      case Some(name) if Files.isRegularFile(path) && name.endsWith(".class") =>
        val binary = root.relativize(path).toString.replace('\\', '/').stripSuffix(".class").replace('/', '.')
        if binary.isEmpty || binary.contains("$$") then None
        else if hasMain(path) then Some(shown(binary))
        else None
      case _ => None

  private def shown(name: String): String =
    if name.endsWith("$") && !name.endsWith("$$") then name.dropRight(1) else name

  private def hasMain(path: Path): Boolean =
    try
      val reader = new ClassReader(Files.readAllBytes(path))
      var found = false
      reader.accept(
        new ClassVisitor(Opcodes.ASM9):
          override def visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              exceptions: Array[String],
          ): MethodVisitor =
            if name == "main" && descriptor == "([Ljava/lang/String;)V" && publicStatic(access) then found = true
            null
        ,
        ClassReader.SKIP_DEBUG | ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES,
      )
      found
    catch case NonFatal(_) => false

  private def publicStatic(access: Int): Boolean =
    (access & Opcodes.ACC_PUBLIC) != 0 && (access & Opcodes.ACC_STATIC) != 0
