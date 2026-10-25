package histos.testrun

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes, Type}

import histos.jdk.Sha256

import zio.Chunk

/**
 * Digest of one suite and the classes it references, plus the locked jars.
 * Each class is a fingerprint and the names it points at. The bytes are not kept.
 * A class this suite does not reference is not part of the digest.
 */
object SuiteStamp:
  /** Fingerprint of one class file, and the binary names it mentions. */
  private[testrun] final case class Print(fingerprint: String, refs: Set[String])

  def digest(suite: String, directories: List[Path], jars: List[String]): Option[String] =
    digest(suite, index(directories), jars)

  /** One walk of the class directories. `remember` stamps every suite from this. */
  private[testrun] def index(directories: List[Path]): Option[Map[String, Print]] =
    try Some(directories.flatMap(classes).toMap)
    catch case NonFatal(_) => None

  private[testrun] def digest(
      suite: String,
      indexed: Option[Map[String, Print]],
      jars: List[String]
  ): Option[String] =
    indexed.flatMap: tree =>
      val start = List(suite, s"$suite$$").filter(tree.contains)
      if start.isEmpty then None
      else
        val used = closure(start, tree)
        val lines =
          used.toList.sorted.flatMap(name =>
            tree.get(name).map(print => s"$name ${print.fingerprint}")
          ) ++ jars.sorted.map(jar => s"jar $jar")
        Some(
          Sha256
            .of(
              Chunk.fromArray(
                lines.mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)
              )
            )
            .value
        )

  private def classes(root: Path): List[(String, Print)] =
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
              if binary.isEmpty then None
              else
                val bytes = Files.readAllBytes(path)
                Some(binary -> Print(Sha256.of(Chunk.fromArray(bytes)).value, references(bytes)))
        }.toList
      finally walk.close()

  private def closure(start: List[String], index: Map[String, Print]): Set[String] =
    def step(seen: Set[String], queue: List[String]): Set[String] =
      queue match
        case Nil => seen
        case name :: rest if seen.contains(name) => step(seen, rest)
        case name :: rest =>
          index.get(name) match
            case None => step(seen, rest)
            case Some(print) =>
              val next =
                print.refs.flatMap(alias).filter(index.contains).filterNot(seen.contains)
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
