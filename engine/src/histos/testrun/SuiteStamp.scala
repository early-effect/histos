package histos.testrun

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes, Type}

import histos.jdk.Sha256

import zio.Chunk

/**
 * Digest of one suite's bytecode and the project classes it touches, plus the locked jars.
 * A class that this suite does not reference is not part of the digest.
 */
object SuiteStamp:
  def digest(suite: String, directories: List[Path], jars: List[String]): Option[String] =
    try
      val index = directories.flatMap(classes).toMap
      val start = List(suite, s"$suite$$").filter(index.contains)
      if start.isEmpty then None
      else
        val used = closure(start, index)
        val lines = used.toList.sorted.map(name => s"$name ${sha(index(name))}") ++ jars.sorted.map(jar => s"jar $jar")
        Some(Sha256.of(Chunk.fromArray(lines.mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))).value)
    catch case NonFatal(_) => None

  private def classes(root: Path): List[(String, Array[Byte])] =
    if !Files.isDirectory(root) then Nil
    else
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.flatMap { path =>
          if !Files.isRegularFile(path) then None
          else
            val name = root.relativize(path).toString.replace('\\', '/')
            if !name.endsWith(".class") then None
            else
              val binary = name.stripSuffix(".class").replace('/', '.')
              if binary.isEmpty then None else Some(binary -> Files.readAllBytes(path))
        }.toList
      finally walk.close()

  private def closure(start: List[String], index: Map[String, Array[Byte]]): Set[String] =
    def step(seen: Set[String], queue: List[String]): Set[String] =
      queue match
        case Nil => seen
        case name :: rest if seen.contains(name) => step(seen, rest)
        case name :: rest =>
          index.get(name) match
            case None => step(seen, rest)
            case Some(bytes) =>
              val next = references(bytes).flatMap(alias).filter(index.contains).filterNot(seen.contains)
              step(seen + name, rest ++ next)
    step(Set.empty, start.distinct)

  private def alias(name: String): List[String] =
    if name.endsWith("$") && !name.endsWith("$$") then List(name, name.dropRight(1))
    else List(name, s"$name$$")

  private def references(bytes: Array[Byte]): Set[String] =
    val found = scala.collection.mutable.Set.empty[String]
    def add(name: String): Unit =
      if name.nonEmpty then found += name.replace('/', '.')
    def collect(kind: Type): Unit =
      kind.getSort match
        case Type.OBJECT =>
          val name = kind.getInternalName
          if name != null then add(name)
        case Type.ARRAY => collect(kind.getElementType)
        case Type.METHOD =>
          kind.getArgumentTypes.foreach(collect)
          collect(kind.getReturnType)
        case _ => ()
    def note(text: String): Unit =
      if text != null && text.nonEmpty then
        try collect(Type.getType(text))
        catch case NonFatal(_) => ()
    try
      val reader = new ClassReader(bytes)
      reader.accept(new ClassVisitor(Opcodes.ASM9):
        override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit =
          if superName != null then add(superName)
          if interfaces != null then interfaces.foreach(add)

        override def visitField(access: Int, name: String, descriptor: String, signature: String, value: Object): org.objectweb.asm.FieldVisitor =
          note(descriptor)
          null

        override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor =
          note(descriptor)
          new MethodVisitor(Opcodes.ASM9):
            override def visitTypeInsn(opcode: Int, typeName: String): Unit = add(typeName)
            override def visitFieldInsn(opcode: Int, owner: String, name: String, descriptor: String): Unit =
              add(owner)
              note(descriptor)
            override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit =
              add(owner)
              note(descriptor)
            override def visitLdcInsn(value: Object): Unit =
              value match
                case kind: Type => collect(kind)
                case _          => ()
      , ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
    catch case NonFatal(_) => ()
    found.toSet

  private def sha(bytes: Array[Byte]): String =
    Sha256.of(Chunk.fromArray(bytes)).value
