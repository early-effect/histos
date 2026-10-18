package histos.testrun

import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}

import histos.native.{ClassShapeError, ClassShapes}

import zio.{IO, ZIO}

/** Suites a provider would run, read from class files. The engine does not load the framework. */
object SuiteNames:
  final case class Mark(superclass: String, module: Boolean)

  enum Found:
    case Suites(names: List[String])
    case Unseen

  def discover(testClasses: Path, classpath: List[Path], framework: String): IO[ClassShapeError, Found] =
    ClassShapes.read(List(testClasses)).flatMap { local =>
      ZIO.attemptBlocking {
        if local.isEmpty then
          if sjsir(testClasses) then Found.Unseen else Found.Suites(Nil)
        else
          val marks = fingerprints(testClasses, classpath, framework)
          val world = parents(local, classpath)
          Found.Suites(choose(local, world, marks))
      }.mapError(_ => ClassShapeError.Read(testClasses))
    }

  private[testrun] def choose(local: List[ClassShapes.Shape], world: Map[String, ClassShapes.Shape], marks: List[Mark]): List[String] =
    local.flatMap { shape =>
      val name = shown(shape.name)
      if name.startsWith("subprocess.") || name.contains("$$") then None
      else if marks.exists(mark => mark.module == shape.module && reaches(shape.name, mark.superclass, world, Set.empty)) then Some(name)
      else None
    }.distinct.sorted

  private def shown(name: String): String =
    if name.endsWith("$") && !name.endsWith("$$") then name.dropRight(1) else name

  private def reaches(name: String, target: String, world: Map[String, ClassShapes.Shape], seen: Set[String]): Boolean =
    if name == target then true
    else if seen.contains(name) then false
    else
      world.get(name) match
        case None => false
        case Some(shape) => shape.parents.exists(parent => reaches(parent, target, world, seen + name))

  private def parents(local: List[ClassShapes.Shape], classpath: List[Path]): Map[String, ClassShapes.Shape] =
    def grow(have: Map[String, ClassShapes.Shape], wanted: List[String]): Map[String, ClassShapes.Shape] =
      wanted.filterNot(have.contains) match
        case Nil => have
        case missing =>
          val loaded = missing.flatMap(name => load(name, classpath))
          if loaded.isEmpty then have
          else
            val next = loaded.foldLeft(have)((acc, shape) => acc.updated(shape.name, shape))
            grow(next, loaded.flatMap(_.parents).distinct)
    val seed = local.map(shape => shape.name -> shape).toMap
    grow(seed, local.flatMap(_.parents).distinct)

  private def load(name: String, classpath: List[Path]): Option[ClassShapes.Shape] =
    val entry = name.replace('.', '/') + ".class"
    classpath.collectFirst(Function.unlift(path => bytes(path, entry))).flatMap(shape)

  private def bytes(path: Path, entry: String): Option[Array[Byte]] =
    if Files.isDirectory(path) then
      val file = path.resolve(entry)
      if Files.isRegularFile(file) then try Some(Files.readAllBytes(file)) catch case NonFatal(_) => None
      else None
    else if Files.isRegularFile(path) && fileName(path).endsWith(".jar") then
      try
        val zip = new ZipFile(path.toFile)
        try
          Option(zip.getEntry(entry)).map { item =>
            val input = zip.getInputStream(item)
            try input.readAllBytes()
            finally input.close()
          }
        finally zip.close()
      catch case NonFatal(_) => None
    else None

  private def shape(bytes: Array[Byte]): Option[ClassShapes.Shape] =
    try
      val reader = new ClassReader(bytes)
      val node = new org.objectweb.asm.tree.ClassNode()
      reader.accept(node, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
      val binary = Option(node.name).map(_.replace('/', '.')).getOrElse("")
      if binary.isEmpty || binary.contains("$$") then None
      else
        val interfaces = Option(node.interfaces).toList.flatMap(_.asScala)
        val supers = (Option(node.superName).toList ++ interfaces).map(_.replace('/', '.')).filter(name => name.nonEmpty && name != "java.lang.Object")
        Some(ClassShapes.Shape(binary, binary.endsWith("$"), supers))
    catch case NonFatal(_) => None

  private def fingerprints(testClasses: Path, classpath: List[Path], framework: String): List[Mark] =
    val known =
      if framework == "zio.test.sbt.ZTestFramework" then List(Mark("zio.test.ZIOSpecAbstract", module = true))
      else Nil
    val home = classpath.filter(path => owns(path, framework))
    (known ++ scan(testClasses) ++ home.flatMap(scan)).distinct

  private def owns(path: Path, framework: String): Boolean =
    bytes(path, framework.replace('.', '/') + ".class").isDefined

  private def scan(path: Path): List[Mark] =
    if Files.isDirectory(path) then
      val walk = Files.walk(path)
      try walk.iterator.asScala.flatMap(file => if classFile(file) then mark(read(file)) else None).toList
      finally walk.close()
    else if Files.isRegularFile(path) && fileName(path).endsWith(".jar") then
      try
        val zip = new ZipFile(path.toFile)
        try
          zip.entries.asScala.flatMap { entry =>
            if entry.isDirectory || !entry.getName.endsWith(".class") then None
            else
              val input = zip.getInputStream(entry)
              try mark(input.readAllBytes())
              finally input.close()
          }.toList
        finally zip.close()
      catch case NonFatal(_) => Nil
    else Nil

  private def mark(bytes: Array[Byte]): Option[Mark] =
    try
      var superclass: Option[String] = None
      var module: Option[Boolean] = None
      var fingerprint = false
      val reader = new ClassReader(bytes)
      reader.accept(new ClassVisitor(Opcodes.ASM9):
        override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit =
          if interfaces != null && interfaces.contains("sbt/testing/SubclassFingerprint") then fingerprint = true

        override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor =
          if name == "superclassName" then new MethodVisitor(Opcodes.ASM9):
            override def visitLdcInsn(value: Object): Unit =
              value match
                case text: String => superclass = Some(text)
                case _            => ()
          else if name == "isModule" then new MethodVisitor(Opcodes.ASM9):
            override def visitInsn(opcode: Int): Unit =
              if opcode == Opcodes.ICONST_1 then module = Some(true)
              else if opcode == Opcodes.ICONST_0 then module = Some(false)
          else null
      , ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
      superclass.filter(_ => fingerprint).map(name => Mark(name, module.getOrElse(true)))
    catch case NonFatal(_) => None

  private def sjsir(root: Path): Boolean =
    if !Files.isDirectory(root) then false
    else
      val walk = Files.walk(root)
      try walk.iterator.asScala.exists(path => classFile(path) == false && fileName(path).endsWith(".sjsir"))
      finally walk.close()

  private def classFile(path: Path): Boolean =
    Files.isRegularFile(path) && fileName(path).endsWith(".class")

  private def read(path: Path): Array[Byte] =
    Files.readAllBytes(path)

  private def fileName(path: Path): String =
    Option(path.getFileName).map(_.toString).getOrElse("")
