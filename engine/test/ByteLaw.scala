package histos.zinc

import histos.jdk.Sha256
import histos.native.{ClassShapeError, ClassShapes}
import histos.pack.{Assembly, LazyValError, LazyValFrame, LazyVals, MainClass}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import org.objectweb.asm.tree.*
import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes, Type}

import scala.jdk.CollectionConverters.*

import zio.ZIO
import zio.test.{assertTrue, suite, test}

/** Class digest, class shape, and the Scala 3.3 lazy-val rewrite. No scalac and no child.
  */
object ByteLaw:
  private val Ready      = "ready"
  private val Leaf       = "n$lzy"
  private val HandleName = s"$Leaf$$lzyHandle"

  def laws =
    suite("bytes")(
      paths,
      leaf,
      alien,
      times,
      ordered,
      truncated,
      absent,
      archived,
      initialized,
      assembled,
      runtime,
      windows,
      frames,
      companion,
    )

  private def paths =
    test("A line number and an absolute path are not part of the class digest."):
      Workspace
        .work("histos-bytes")
        .flatMap: root =>
          val left  = root.resolve("left")
          val right = root.resolve("right")
          for
            _ <- writeBytes(left.resolve("demo/Noted.class"), noted(3, "/var/build/Noted.scala"))
            _ <- writeBytes(right.resolve("demo/Noted.class"), noted(40, "/other/place/Noted.scala"))
          yield
            val one   = show(ProductDigest.ofDirectory(left))
            val other = show(ProductDigest.ofDirectory(right))
            assertTrue(one == other, one.isRight)

  private def leaf =
    test("A path leaf that contains `$` still hashes."):
      Workspace
        .work("histos-bytes")
        .flatMap: root =>
          val kept  = root.resolve("kept")
          val again = root.resolve("again")
          val other = root.resolve("other")
          for
            _ <- writeBytes(kept.resolve("demo/Noted.class"), noted(1, "/srv/a/Keep$.class"))
            _ <- writeBytes(again.resolve("demo/Noted.class"), noted(1, "/srv/b/Keep$.class"))
            _ <- writeBytes(other.resolve("demo/Noted.class"), noted(1, "/srv/a/Other$.class"))
          yield
            val same   = show(ProductDigest.ofDirectory(kept))
            val copied = show(ProductDigest.ofDirectory(again))
            val moved  = show(ProductDigest.ofDirectory(other))
            assertTrue(same == copied, same != moved, same.isRight)

  private def alien =
    test("Bytes that are not a class file are refused."):
      val got = ProductDigest.normalize("not a class".getBytes(StandardCharsets.UTF_8))
      assertTrue(got == Left(ClassBytesError.Unreadable))

  private def times =
    test("Jar entry timestamps are not part of the digest."):
      Workspace
        .work("histos-bytes")
        .flatMap: root =>
          val bytes = noted(1, "Noted.scala")
          val early = root.resolve("early.jar")
          val late  = root.resolve("late.jar")
          for
            _ <- writeJar(early, 1_700_000_000_000L, bytes)
            _ <- writeJar(late, 1_800_000_000_000L, bytes)
          yield
            val one   = show(ProductDigest.ofJar(early))
            val other = show(ProductDigest.ofJar(late))
            assertTrue(one == other, one.isRight)

  private def ordered =
    test("A directory of classes returns both shapes sorted by name."):
      Workspace
        .work("histos-bytes")
        .flatMap: root =>
          val classes = root.resolve("classes")
          for
            _      <- writeBytes(classes.resolve("pkg/App$.class"), plain("pkg/App$"))
            _      <- writeBytes(classes.resolve("pkg/App.class"), plain("pkg/App"))
            shapes <- ClassShapes.read(List(classes))
          yield assertTrue(
            shapes == List(
              ClassShapes.Shape("pkg.App", false, Nil),
              ClassShapes.Shape("pkg.App$", true, Nil),
            )
          )

  private def truncated =
    test("A short class file is an error."):
      Workspace
        .work("histos-bytes")
        .flatMap: root =>
          val path = root.resolve("Short.class")
          writeBytes(path, Array[Byte](0xca.toByte, 0xfe.toByte, 0xba.toByte, 0xbe.toByte)).flatMap: _ =>
            ClassShapes
              .read(List(root))
              .either
              .map: got =>
                assertTrue(got == Left(ClassShapeError.ClassFile(path.toString)))

  private def absent =
    test("A missing directory is empty."):
      Workspace
        .work("histos-bytes")
        .flatMap: root =>
          ClassShapes.read(List(root.resolve("missing"))).map(shapes => assertTrue(shapes == Nil))

  private def archived =
    test("A jar returns the class shape."):
      Workspace
        .work("histos-bytes")
        .flatMap: root =>
          val jar = root.resolve("app.jar")
          writeJar(jar, 0L, plain("pkg/App"), "pkg/App.class").flatMap: _ =>
            ClassShapes
              .read(List(jar))
              .map: shapes =>
                assertTrue(shapes == List(ClassShapes.Shape("pkg.App", false, Nil)))

  private def initialized =
    test("A Scala 3.3 lazy val becomes a VarHandle and still initializes."):
      val raw = box("demo/Box", "demo/Box")
      LazyVals.catalog(List("demo/Box.class" -> raw)) match
        case Left(error)    => ZIO.succeed(assertTrue(error.message.isEmpty))
        case Right(handles) =>
          LazyVals.rewrite("demo/Box.class", raw, handles, ClassLoader.getSystemClassLoader) match
            case Left(error)      => ZIO.succeed(assertTrue(error.message.isEmpty))
            case Right(rewritten) =>
              ZIO
                .attempt(loadValue(rewritten))
                .map: value =>
                  assertTrue(
                    value == Ready,
                    calls(rewritten, "findVarHandle"),
                    calls(rewritten, "compareAndSet"),
                    !calls(rewritten, "getOffsetStatic"),
                    !calls(rewritten, "objCAS"),
                  )

  private def assembled =
    test("The assembly writes that class."):
      Workspace
        .work("histos-bytes")
        .flatMap: root =>
          val raw     = box("demo/Box", "demo/Box")
          val classes = root.resolve("classes")
          val dest    = root.resolve("app.jar")
          writeBytes(classes.resolve("demo/Box.class"), raw).flatMap: _ =>
            ZIO
              .fromEither(MainClass.from("demo.Box"))
              .flatMap: main =>
                Assembly
                  .write(List(classes), Nil, main, dest)
                  .flatMap: _ =>
                    readEntry(dest, "demo/Box.class").map: stored =>
                      val expected =
                        LazyVals
                          .catalog(List("demo/Box.class" -> raw))
                          .flatMap: handles =>
                            LazyVals.rewrite("demo/Box.class", raw, handles, ClassLoader.getSystemClassLoader)
                      (stored, expected) match
                        case (Some(bytes), Right(rewritten)) =>
                          assertTrue(bytes.sameElements(rewritten), calls(bytes, "findVarHandle"))
                        case (None, expected) =>
                          val why = expected.fold(error => error.message, _ => "missing demo/Box.class")
                          assertTrue(why.isEmpty)
                        case (_, Left(error)) => assertTrue(error.message.isEmpty)

  private def runtime =
    test("The LazyVals runtime stays, and a stray call is refused."):
      val raw  = box("demo/Box", "demo/Box")
      val kept =
        LazyVals.rewrite("scala/runtime/LazyVals.class", raw, Map.empty, ClassLoader.getSystemClassLoader)
      val module =
        LazyVals.rewrite("scala/runtime/LazyVals$.class", raw, Map.empty, ClassLoader.getSystemClassLoader)
      val stray = LazyVals.rewrite("demo/Stray.class", strayCall(), Map.empty, ClassLoader.getSystemClassLoader)
      assertTrue(
        kept.exists(_.sameElements(raw)),
        module.exists(_.sameElements(raw)),
        stray == Left(LazyValError.StillCalls("demo/Stray.class", "touch")),
      )

  private def windows =
    test("A short window or a non-string ldc does not throw."):
      val loader = ClassLoader.getSystemClassLoader
      val short  = LazyVals.rewrite("demo/Short.class", shortWindow(), Map.empty, loader)
      val number = LazyVals.rewrite("demo/Number.class", numberLdc(), Map.empty, loader)
      assertTrue(
        short == Left(LazyValError.StillCalls("demo/Short.class", "touch")),
        number == Left(LazyValError.StillCalls("demo/Number.class", "<clinit>")),
      )

  private def frames =
    test("A rejected frame computation is a typed error."):
      val raw = rejected()
      LazyVals.catalog(List("demo/Reject.class" -> raw)) match
        case Left(error)    => assertTrue(error.message.isEmpty)
        case Right(handles) =>
          val got = LazyVals.rewrite("demo/Reject.class", raw, handles, ClassLoader.getSystemClassLoader)
          assertTrue(got == Left(LazyValError.Frames("demo/Reject.class", LazyValFrame.Rejected)))

  private def companion =
    test("A companion offset can publish a VarHandle for the module's private field."):
      val raw = box("demo/Use", "demo/Use$")
      LazyVals.catalog(List("demo/Use.class" -> raw)) match
        case Left(error)    => assertTrue(error.message.isEmpty)
        case Right(handles) =>
          LazyVals.rewrite("demo/Use.class", raw, handles, ClassLoader.getSystemClassLoader) match
            case Left(error)      => assertTrue(error.message.isEmpty)
            case Right(rewritten) =>
              assertTrue(
                handles.get(("demo/Use$", "OFFSET$0")).contains(LazyVals.Handle("demo/Use$", "OFFSET$0", HandleName)),
                calls(rewritten, "privateLookupIn"),
                calls(rewritten, "findVarHandle"),
                puts(rewritten, "demo/Use$", HandleName),
                texts(rewritten).contains(Leaf),
                !calls(rewritten, "getOffsetStatic"),
              )

  private def show(got: Either[CompileError, Sha256]): Either[String, String] =
    got.fold(error => Left(error.message), digest => Right(digest.value))

  private def noted(line: Int, text: String): Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, "demo/Noted", null, "java/lang/Object", null)
    val method = writer.visitMethod(Opcodes.ACC_PUBLIC, "path", "()Ljava/lang/String;", null, null)
    method.visitCode()
    val start = new org.objectweb.asm.Label()
    method.visitLabel(start)
    method.visitLineNumber(line, start)
    method.visitLdcInsn(text)
    method.visitInsn(Opcodes.ARETURN)
    method.visitMaxs(0, 0)
    method.visitEnd()
    writer.visitEnd()
    writer.toByteArray

  private def plain(internal: String): Array[Byte] =
    val writer = new ClassWriter(0)
    writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, internal, null, "java/lang/Object", null)
    writer.visitEnd()
    writer.toByteArray

  /** `owner` is the class that declares `OFFSET$`. A different `internal` name is the companion case. */
  private def box(internal: String, owner: String): Array[Byte] =
    val node = new ClassNode()
    node.version = Opcodes.V1_8
    node.access = Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER
    node.name = internal
    node.superName = "java/lang/Object"
    node.fields.add(new FieldNode(Opcodes.ACC_PRIVATE, Leaf, "Ljava/lang/Object;", null, null))
    node.fields.add(new FieldNode(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "OFFSET$0", "J", null, null))
    node.methods.add(constructor())
    node.methods.add(offsetInit(owner))
    if owner == internal then node.methods.add(reader())
    emit(node)

  private def constructor(): MethodNode =
    val method = new MethodNode(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
    method.instructions.add(new VarInsnNode(Opcodes.ALOAD, 0))
    method.instructions.add(
      new MethodInsnNode(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    )
    method.instructions.add(new InsnNode(Opcodes.RETURN))
    method

  private def offsetInit(owner: String): MethodNode =
    val method = new MethodNode(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null)
    val list   = method.instructions
    list.add(new FieldInsnNode(Opcodes.GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;"))
    list.add(new LdcInsnNode(Type.getObjectType(owner)))
    list.add(new LdcInsnNode(Leaf))
    list.add(
      new MethodInsnNode(
        Opcodes.INVOKEVIRTUAL,
        "java/lang/Class",
        "getDeclaredField",
        "(Ljava/lang/String;)Ljava/lang/reflect/Field;",
        false,
      )
    )
    list.add(
      new MethodInsnNode(
        Opcodes.INVOKEVIRTUAL,
        "scala/runtime/LazyVals$",
        "getOffsetStatic",
        "(Ljava/lang/reflect/Field;)J",
        false,
      )
    )
    list.add(new FieldInsnNode(Opcodes.PUTSTATIC, owner, "OFFSET$0", "J"))
    list.add(new InsnNode(Opcodes.RETURN))
    method

  private def reader(): MethodNode =
    val method = new MethodNode(Opcodes.ACC_PUBLIC, "n", "()Ljava/lang/Object;", null, null)
    val list   = method.instructions
    list.add(new FieldInsnNode(Opcodes.GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;"))
    list.add(new VarInsnNode(Opcodes.ALOAD, 0))
    list.add(new FieldInsnNode(Opcodes.GETSTATIC, "demo/Box", "OFFSET$0", "J"))
    list.add(new InsnNode(Opcodes.ACONST_NULL))
    list.add(new LdcInsnNode(Ready))
    list.add(
      new MethodInsnNode(
        Opcodes.INVOKEVIRTUAL,
        "scala/runtime/LazyVals$",
        "objCAS",
        "(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z",
        false,
      )
    )
    list.add(new InsnNode(Opcodes.POP))
    list.add(new VarInsnNode(Opcodes.ALOAD, 0))
    list.add(new FieldInsnNode(Opcodes.GETFIELD, "demo/Box", Leaf, "Ljava/lang/Object;"))
    list.add(new InsnNode(Opcodes.ARETURN))
    method

  private def strayCall(): Array[Byte] =
    val node = new ClassNode()
    node.version = Opcodes.V1_8
    node.access = Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER
    node.name = "demo/Stray"
    node.superName = "java/lang/Object"
    val method = new MethodNode(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "touch", "()V", null, null)
    val list   = method.instructions
    list.add(new InsnNode(Opcodes.ICONST_0))
    list.add(new InsnNode(Opcodes.POP))
    list.add(new FieldInsnNode(Opcodes.GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;"))
    list.add(new InsnNode(Opcodes.ACONST_NULL))
    list.add(
      new MethodInsnNode(
        Opcodes.INVOKEVIRTUAL,
        "scala/runtime/LazyVals$",
        "getOffsetStatic",
        "(Ljava/lang/reflect/Field;)J",
        false,
      )
    )
    list.add(new InsnNode(Opcodes.POP2))
    list.add(new InsnNode(Opcodes.RETURN))
    node.methods.add(method)
    emit(node)

  private def shortWindow(): Array[Byte] =
    val node = new ClassNode()
    node.version = Opcodes.V1_8
    node.access = Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER
    node.name = "demo/Short"
    node.superName = "java/lang/Object"
    val method = new MethodNode(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "touch", "()V", null, null)
    method.instructions.add(
      new MethodInsnNode(
        Opcodes.INVOKEVIRTUAL,
        "scala/runtime/LazyVals$",
        "getOffsetStatic",
        "(Ljava/lang/reflect/Field;)J",
        false,
      )
    )
    method.instructions.add(new InsnNode(Opcodes.RETURN))
    node.methods.add(method)
    emit(node)

  private def numberLdc(): Array[Byte] =
    val node = new ClassNode()
    node.version = Opcodes.V1_8
    node.access = Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER
    node.name = "demo/Number"
    node.superName = "java/lang/Object"
    node.fields.add(new FieldNode(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "OFFSET$0", "J", null, null))
    val method = offsetInit("demo/Number")
    method.instructions.iterator.asScala.foreach:
      case ldc: LdcInsnNode if ldc.cst.isInstanceOf[String] => ldc.cst = Int.box(1)
      case _                                                => ()
    node.methods.add(method)
    emit(node)

  private def rejected(): Array[Byte] =
    val node = new ClassNode()
    node.version = Opcodes.V1_8
    node.access = Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER
    node.name = "demo/Reject"
    node.superName = "java/lang/Object"
    node.fields.add(new FieldNode(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "OFFSET$0", "J", null, null))
    node.methods.add(offsetInit("demo/Reject"))
    val method = new MethodNode(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "go", "()V", null, null)
    val label  = new LabelNode()
    method.instructions.add(new JumpInsnNode(Opcodes.JSR, label))
    method.instructions.add(label)
    method.instructions.add(new InsnNode(Opcodes.RETURN))
    node.methods.add(method)
    emit(node)

  private def emit(node: ClassNode): Array[Byte] =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    node.accept(writer)
    writer.toByteArray

  private def loadValue(bytes: Array[Byte]): AnyRef =
    val loader = Defined(ClassLoader.getSystemClassLoader)
    val cls    = loader.load("demo.Box", bytes)
    val obj    = cls.getDeclaredConstructor().newInstance()
    cls.getMethod("n").invoke(obj)

  private def calls(bytes: Array[Byte], name: String): Boolean =
    node(bytes).methods.asScala.exists: method =>
      method.instructions.iterator.asScala.exists:
        case call: MethodInsnNode => call.name == name
        case _                    => false

  private def puts(bytes: Array[Byte], owner: String, name: String): Boolean =
    node(bytes).methods.asScala.exists: method =>
      method.instructions.iterator.asScala.exists:
        case field: FieldInsnNode =>
          field.getOpcode == Opcodes.PUTSTATIC && field.owner == owner && field.name == name
        case _ => false

  private def texts(bytes: Array[Byte]): Set[String] =
    node(bytes).methods.asScala
      .flatMap: method =>
        method.instructions.iterator.asScala.collect:
          case ldc: LdcInsnNode =>
            ldc.cst match
              case text: String => text
              case _            => ""
      .toSet

  private def node(bytes: Array[Byte]): ClassNode =
    val parsed = new ClassNode()
    new ClassReader(bytes).accept(parsed, 0)
    parsed

  private def writeBytes(path: Path, bytes: Array[Byte]): ZIO[Any, IoFailure, Unit] =
    val made = Option(path.getParent) match
      case Some(parent) => Workspace.createDirectories(parent)
      case None         => ZIO.dieMessage("class has no parent")
    made *> ZIO.attemptBlocking(Files.write(path, bytes)).mapError(_ => IoFailure(path, IoFault.Write)).unit

  private def writeJar(path: Path, time: Long, bytes: Array[Byte], name: String = "demo/Noted.class") =
    ZIO
      .attemptBlocking:
        val out = new ZipOutputStream(Files.newOutputStream(path))
        try
          val entry = new ZipEntry(name)
          entry.setTime(time)
          out.putNextEntry(entry)
          out.write(bytes)
          out.closeEntry()
          if name.endsWith(".class") then
            val note = new ZipEntry("notes.txt")
            note.setTime(time)
            out.putNextEntry(note)
            out.write("note".getBytes(StandardCharsets.UTF_8))
            out.closeEntry()
        finally out.close()
      .mapError(_ => IoFailure(path, IoFault.Write))
      .unit

  private def readEntry(path: Path, name: String): ZIO[Any, IoFailure, Option[Array[Byte]]] =
    ZIO
      .attemptBlocking:
        val zip = new ZipFile(path.toFile)
        try
          Option(zip.getEntry(name)).map: entry =>
            val input = zip.getInputStream(entry)
            try input.readAllBytes()
            finally input.close()
        finally zip.close()
      .mapError(_ => IoFailure(path, IoFault.Read))

  private final class Defined(parent: ClassLoader) extends ClassLoader(parent):
    def load(name: String, bytes: Array[Byte]) =
      defineClass(name, bytes, 0, bytes.length)
end ByteLaw
