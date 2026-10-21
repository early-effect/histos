package histos.pack

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.Type
import org.objectweb.asm.tree.{ClassNode, MethodInsnNode}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

object LazyValsSpec extends ZIOSpecDefault:
  private val rewritten = suite("lazy vals")(
    test("a 3.3 lazy val becomes a VarHandle and still initializes"):
      val old = oldClass
      LazyVals.catalog(List("demo/OldLazy.class" -> old)) match
        case Left(error) => assertTrue(error.message.isEmpty)
        case Right(handles) =>
          LazyVals.rewrite("demo/OldLazy.class", old, handles, getClass.getClassLoader) match
            case Left(error) => assertTrue(error.message.isEmpty)
            case Right(bytes) =>
              val initialized =
                try
                  val loader = new ClassLoader(getClass.getClassLoader):
                    override def findClass(name: String) =
                      if name == "demo.OldLazy" then defineClass(name, bytes, 0, bytes.length)
                      else throw new ClassNotFoundException(name)
                  Class.forName("demo.OldLazy", true, loader)
                  None
                catch case error: Throwable => Some(error.toString)
              assertTrue(
                !calls(bytes, "scala/runtime/LazyVals$", "getOffsetStatic"),
                !calls(bytes, "scala/runtime/LazyVals$", "objCAS"),
                calls(bytes, "java/lang/invoke/MethodHandles$Lookup", "findVarHandle"),
                initialized.isEmpty,
              )
    ,
    test("the assembly writes the rewritten class"):
      ZIO.scoped:
        for
          root <- temp
          classes <- ZIO.attempt {
                       val dir = root.resolve("classes")
                       val path = dir.resolve("demo").resolve("Entry.class")
                       Files.createDirectories(path.getParent)
                       Files.write(path, Array[Byte](1))
                       dir
                     }
          dependency = root.resolve("old.jar")
          _ <- ZIO.attempt {
                 val out = new ZipOutputStream(Files.newOutputStream(dependency))
                 try
                   out.putNextEntry(new ZipEntry("demo/OldLazy.class"))
                   out.write(oldClass)
                   out.closeEntry()
                 finally out.close()
               }
          packed <- Assembly.write(List(classes), List(dependency), MainClass.trusted("demo.Entry"), root.resolve("app.jar"))
          bytes <- ZIO.attempt(entry(packed.path, "demo/OldLazy.class"))
        yield assertTrue(bytes.nonEmpty && bytes.forall(body => !calls(body, "scala/runtime/LazyVals$", "getOffsetStatic")))
    ,
    test("the LazyVals runtime stays and a stray call is refused"):
      val kept = LazyVals.rewrite("scala/runtime/LazyVals.class", forwarder, Map.empty, getClass.getClassLoader)
      val moduleBytes = runtimeModule
      val keptModule = LazyVals.rewrite("scala/runtime/LazyVals$.class", moduleBytes, Map.empty, getClass.getClassLoader)
      val refused = LazyVals.rewrite("demo/Stray.class", stray, Map.empty, getClass.getClassLoader)
      assertTrue(
        kept.exists(bytes => calls(bytes, "scala/runtime/LazyVals$", "getOffsetStatic")),
        keptModule.exists(_.sameElements(moduleBytes)),
        refused == Left(LazyValError.Unhandled("demo/Stray.class", "run still calls Unsafe lazy vals")),
      )
    ,
    test("a companion offset can publish a VarHandle for the module's private field"):
      val host = splitHost
      val module = splitModule
      LazyVals.catalog(List("demo/Split.class" -> host, "demo/Split$.class" -> module)) match
        case Left(error) => assertTrue(error.message.isEmpty)
        case Right(handles) =>
          val hostBytes = LazyVals.rewrite("demo/Split.class", host, handles, getClass.getClassLoader)
          val moduleBytes = LazyVals.rewrite("demo/Split$.class", module, handles, getClass.getClassLoader)
          val loaded = (hostBytes, moduleBytes) match
            case (Right(hostBody), Right(moduleBody)) =>
              val classes = Map("demo.Split" -> hostBody, "demo.Split$" -> moduleBody)
              try
                val loader = new ClassLoader(getClass.getClassLoader):
                  override def findClass(name: String) =
                    classes.get(name) match
                      case Some(body) => defineClass(name, body, 0, body.length)
                      case None       => throw new ClassNotFoundException(name)
                Class.forName("demo.Split", true, loader)
                Class.forName("demo.Split$", true, loader)
                None
              catch case error: Throwable => Some(error.toString)
            case _ => Some(s"$hostBytes $moduleBytes")
          assertTrue(
            loaded.isEmpty,
            hostBytes.exists(body => calls(body, "java/lang/invoke/MethodHandles", "privateLookupIn")),
            hostBytes.exists(body => !calls(body, "scala/runtime/LazyVals$", "getOffsetStatic")),
          )
  )

  private def entry(path: Path, name: String): Option[Array[Byte]] =
    val zip = new ZipFile(path.toFile)
    try
      Option(zip.getEntry(name)).map: item =>
        val input = zip.getInputStream(item)
        try input.readAllBytes()
        finally input.close()
    finally zip.close()

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-lazy"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  private def calls(bytes: Array[Byte], owner: String, name: String): Boolean =
    val node = new ClassNode()
    new org.objectweb.asm.ClassReader(bytes).accept(node, 0)
    node.methods.asScala.exists: method =>
      method.instructions.iterator.asScala.exists:
        case call: MethodInsnNode => call.owner == owner && call.name == name
        case _ => false

  private def oldClass: Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC, "demo/OldLazy", null, "java/lang/Object", null)
    writer.visitField(ACC_PUBLIC | ACC_STATIC | ACC_FINAL, "OFFSET$0", "J", null, null).visitEnd()
    writer.visitField(ACC_PRIVATE | ACC_VOLATILE, "value$lzy1", "Ljava/lang/Object;", null, null).visitEnd()
    val clinit = writer.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
    clinit.visitCode()
    clinit.visitFieldInsn(GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;")
    clinit.visitLdcInsn(Type.getObjectType("demo/OldLazy"))
    clinit.visitLdcInsn("value$lzy1")
    clinit.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getDeclaredField", "(Ljava/lang/String;)Ljava/lang/reflect/Field;", false)
    clinit.visitMethodInsn(INVOKEVIRTUAL, "scala/runtime/LazyVals$", "getOffsetStatic", "(Ljava/lang/reflect/Field;)J", false)
    clinit.visitFieldInsn(PUTSTATIC, "demo/OldLazy", "OFFSET$0", "J")
    clinit.visitInsn(RETURN)
    clinit.visitMaxs(0, 0)
    clinit.visitEnd()
    val init = writer.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    init.visitCode()
    init.visitVarInsn(ALOAD, 0)
    init.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    init.visitInsn(RETURN)
    init.visitMaxs(0, 0)
    init.visitEnd()
    val body = writer.visitMethod(ACC_PRIVATE, "value$lzyINIT1", "()Ljava/lang/Object;", null, null)
    body.visitCode()
    body.visitFieldInsn(GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;")
    body.visitVarInsn(ALOAD, 0)
    body.visitFieldInsn(GETSTATIC, "demo/OldLazy", "OFFSET$0", "J")
    body.visitInsn(ACONST_NULL)
    body.visitFieldInsn(GETSTATIC, "scala/runtime/LazyVals$Evaluating$", "MODULE$", "Lscala/runtime/LazyVals$Evaluating$;")
    body.visitMethodInsn(INVOKEVIRTUAL, "scala/runtime/LazyVals$", "objCAS", "(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z", false)
    body.visitInsn(POP)
    body.visitInsn(ACONST_NULL)
    body.visitInsn(ARETURN)
    body.visitMaxs(0, 0)
    body.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def forwarder: Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC | ACC_FINAL, "scala/runtime/LazyVals", null, "java/lang/Object", null)
    val method = writer.visitMethod(ACC_PUBLIC | ACC_STATIC, "getOffsetStatic", "(Ljava/lang/reflect/Field;)J", null, null)
    method.visitCode()
    method.visitFieldInsn(GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;")
    method.visitVarInsn(ALOAD, 0)
    method.visitMethodInsn(INVOKEVIRTUAL, "scala/runtime/LazyVals$", "getOffsetStatic", "(Ljava/lang/reflect/Field;)J", false)
    method.visitInsn(LRETURN)
    method.visitMaxs(0, 0)
    method.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def runtimeModule: Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC | ACC_FINAL, "scala/runtime/LazyVals$", null, "java/lang/Object", null)
    val method = writer.visitMethod(ACC_PUBLIC, "getOffsetStatic", "(Ljava/lang/reflect/Field;)J", null, null)
    method.visitCode()
    method.visitInsn(LCONST_0)
    method.visitInsn(LRETURN)
    method.visitMaxs(0, 0)
    method.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def splitHost: Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC, "demo/Split", null, "java/lang/Object", null)
    writer.visitField(ACC_PUBLIC | ACC_STATIC | ACC_FINAL, "OFFSET$0", "J", null, null).visitEnd()
    val clinit = writer.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
    clinit.visitCode()
    clinit.visitFieldInsn(GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;")
    clinit.visitLdcInsn(Type.getObjectType("demo/Split$"))
    clinit.visitLdcInsn("value$lzy1")
    clinit.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getDeclaredField", "(Ljava/lang/String;)Ljava/lang/reflect/Field;", false)
    clinit.visitMethodInsn(INVOKEVIRTUAL, "scala/runtime/LazyVals$", "getOffsetStatic", "(Ljava/lang/reflect/Field;)J", false)
    clinit.visitFieldInsn(PUTSTATIC, "demo/Split", "OFFSET$0", "J")
    clinit.visitInsn(RETURN)
    clinit.visitMaxs(0, 0)
    clinit.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def splitModule: Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC, "demo/Split$", null, "java/lang/Object", null)
    writer.visitField(ACC_PRIVATE | ACC_VOLATILE, "value$lzy1", "Ljava/lang/Object;", null, null).visitEnd()
    val init = writer.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    init.visitCode()
    init.visitVarInsn(ALOAD, 0)
    init.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    init.visitInsn(RETURN)
    init.visitMaxs(0, 0)
    init.visitEnd()
    val touch = writer.visitMethod(ACC_PUBLIC, "touch", "()V", null, null)
    touch.visitCode()
    touch.visitFieldInsn(GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;")
    touch.visitVarInsn(ALOAD, 0)
    touch.visitFieldInsn(GETSTATIC, "demo/Split", "OFFSET$0", "J")
    touch.visitInsn(ACONST_NULL)
    touch.visitLdcInsn("ok")
    touch.visitMethodInsn(INVOKEVIRTUAL, "scala/runtime/LazyVals$", "objCAS", "(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z", false)
    touch.visitInsn(POP)
    touch.visitInsn(RETURN)
    touch.visitMaxs(0, 0)
    touch.visitEnd()
    val clinit = writer.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
    clinit.visitCode()
    clinit.visitTypeInsn(NEW, "demo/Split$")
    clinit.visitInsn(DUP)
    clinit.visitMethodInsn(INVOKESPECIAL, "demo/Split$", "<init>", "()V", false)
    clinit.visitMethodInsn(INVOKEVIRTUAL, "demo/Split$", "touch", "()V", false)
    clinit.visitInsn(RETURN)
    clinit.visitMaxs(0, 0)
    clinit.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def stray: Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(V1_8, ACC_PUBLIC, "demo/Stray", null, "java/lang/Object", null)
    val method = writer.visitMethod(ACC_PUBLIC | ACC_STATIC, "run", "()V", null, null)
    method.visitCode()
    method.visitFieldInsn(GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;")
    method.visitInsn(ACONST_NULL)
    method.visitMethodInsn(INVOKEVIRTUAL, "scala/runtime/LazyVals$", "getOffsetStatic", "(Ljava/lang/reflect/Field;)J", false)
    method.visitInsn(POP2)
    method.visitInsn(RETURN)
    method.visitMaxs(0, 0)
    method.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  def spec = rewritten
