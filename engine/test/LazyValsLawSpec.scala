package histos.pack

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes, Type}
import org.objectweb.asm.tree.*

import _root_.scala.jdk.CollectionConverters.*

import zio.test.*

object LazyValsLawSpec extends ZIOSpecDefault:
  private val flags        = Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL
  private val handleName   = "bitmap$lzy$lzyHandle"
  private val local        = "demo/Local"
  private val foreign      = "demo/Foreign"
  private val decoy        = "demo/Decoy"
  private val waitingOwner = "demo/Wait"
  private val offset       = "OFFSET$bitmap"
  private val kept         = "OFFSET$kept"
  private val plain        = "OFFSET$plain"
  private val waitOffset   = "OFFSET$wait"
  private val objCas       = "(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z"
  private val offsetStatic = "(Ljava/lang/reflect/Field;)J"

  def spec = suite("lazy val laws")(
    test("runtime and current classes stay, a lazy init is one handle, and an uncovered call is refused"):
      val marked     = "getOffsetStatic".getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
      val quiet      = "plain".getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
      val localBytes =
        emit(local, List(field(offset)), List(init(local, local, "bitmap$lzy", offset), cas(local, offset)))
      val decoyBytes = emit(
        decoy,
        List(field(plain), field(kept)),
        List(clinit(List(initWindow(decoy, decoy, "bitmap", plain), initWindow(decoy, decoy, "bitmap$lzy", kept)))),
      )
      val bare        = emit("demo/Bare", Nil, List(call("objCAS", objCas)))
      val localHandle = LazyVals.Handle(local, offset, handleName)
      val decoyHandle = LazyVals.Handle(decoy, kept, handleName)
      val catalog     = LazyVals.catalog(
        List(
          "scala/runtime/LazyVals.class"  -> marked,
          "scala/runtime/LazyVals$.class" -> marked,
          "demo/Quiet.class"              -> quiet,
          "demo/Local.class"              -> localBytes,
          "demo/Decoy.class"              -> decoyBytes,
          "demo/NoInit.class"             -> bare,
        )
      )
      val prefixed = LazyVals.catalog(List("extra/scala/runtime/LazyVals.class" -> localBytes))
      val missing  = LazyVals.rewrite(
        "demo/Orphan.class",
        emit("demo/Orphan", Nil, List(cas("demo/Orphan", "OFFSET$missing"))),
        Map.empty,
        loader,
      )
      val still = LazyVals.rewrite("demo/Bare.class", bare, Map.empty, loader)
      assertTrue(
        catalog == Right(Map((local, offset) -> localHandle, (decoy, kept) -> decoyHandle)),
        prefixed == Right(Map((local, offset) -> localHandle)),
        same("scala/runtime/LazyVals.class", marked),
        same("scala/runtime/LazyVals$.class", marked),
        same("demo/Quiet.class", quiet),
        missing == Left(LazyValError.MissingHandle("demo/Orphan.class", "demo/Orphan", "OFFSET$missing")),
        still == Left(LazyValError.StillCalls("demo/Bare.class", "cas")),
      )
    ,
    test(
      "a same-class init keeps the field flags, a foreign class looks itself up, and a waiting call is compareAndSet"
    ):
      val localBytes =
        emit(local, List(field(offset)), List(init(local, local, "bitmap$lzy", offset), cas(local, offset)))
      val foreignBytes =
        emit(foreign, List(field(offset)), List(init(foreign, "java/lang/String", "bitmap$lzy", offset)))
      val waited      = emit(waitingOwner, List(field(waitOffset)), List(waiting(waitingOwner, waitOffset)))
      val localHandle = LazyVals.Handle(local, offset, handleName)
      val written     = LazyVals.rewrite("demo/Local.class", localBytes, Map((local, offset) -> localHandle), loader)
      val lookedUp    = LazyVals.rewrite(
        "demo/Foreign.class",
        foreignBytes,
        Map((foreign, offset) -> LazyVals.Handle(foreign, offset, handleName)),
        loader,
      )
      val held = LazyVals.rewrite(
        "demo/Wait.class",
        waited,
        Map((waitingOwner, waitOffset) -> LazyVals.Handle(waitingOwner, waitOffset, handleName)),
        loader,
      )
      assertTrue(
        fieldOf(written, handleName) == Some(Field(flags, "Ljava/lang/invoke/VarHandle;")),
        names(written) == List(handleName),
        invoked(written) == Set("lookup", "findVarHandle", "compareAndSet"),
        fieldOf(lookedUp, handleName) == Some(Field(flags, "Ljava/lang/invoke/VarHandle;")),
        names(lookedUp) == List(handleName),
        invoked(lookedUp) == Set("lookup", "privateLookupIn", "findVarHandle"),
        fieldOf(held, handleName) == Some(Field(flags, "Ljava/lang/invoke/VarHandle;")),
        names(held) == List(handleName),
        invoked(held) == Set("<init>", "compareAndSet"),
      ),
  )

  private final case class Field(access: Int, desc: String)

  private def loader: ClassLoader =
    Option(getClass.getClassLoader).getOrElse(ClassLoader.getPlatformClassLoader)

  private def same(entry: String, bytes: Array[Byte]): Boolean =
    LazyVals.rewrite(entry, bytes, Map((local, offset) -> LazyVals.Handle(local, offset, handleName)), loader) match
      case Right(out) => out eq bytes
      case Left(_)    => false

  private def fieldOf(result: Either[LazyValError, Array[Byte]], name: String): Option[Field] =
    result.toOption.flatMap: bytes =>
      read(bytes).fields.asScala.find(_.name == name).map(found => Field(found.access, found.desc))

  private def names(result: Either[LazyValError, Array[Byte]]): List[String] =
    result.toOption.toList.flatMap(bytes => read(bytes).fields.asScala.map(_.name).toList)

  private def invoked(result: Either[LazyValError, Array[Byte]]): Set[String] =
    result.toOption.fold(Set.empty)(methodNames)

  private def methodNames(bytes: Array[Byte]): Set[String] =
    read(bytes).methods.asScala.flatMap { method =>
      method.instructions.iterator.asScala.collect { case insn: MethodInsnNode => insn.name }
    }.toSet

  private def read(bytes: Array[Byte]): ClassNode =
    val node = new ClassNode()
    new ClassReader(bytes).accept(node, 0)
    node

  private def emit(name: String, fields: List[FieldNode], methods: List[MethodNode]): Array[Byte] =
    val node = new ClassNode()
    node.version = Opcodes.V1_8
    node.access = Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER
    node.name = name
    node.superName = "java/lang/Object"
    fields.foreach(node.fields.add)
    methods.foreach(node.methods.add)
    val writer = new ClassWriter(0)
    node.accept(writer)
    writer.toByteArray

  private def field(name: String): FieldNode =
    new FieldNode(flags, name, "J", null, null)

  private def method(name: String, desc: String): MethodNode =
    val node = new MethodNode(Opcodes.ACC_STATIC, name, desc, null, null)
    node.maxStack = 8
    node.maxLocals = 0
    node

  private def init(owner: String, literal: String, storage: String, offsetName: String): MethodNode =
    clinit(List(initWindow(owner, literal, storage, offsetName)))

  private def clinit(windows: List[MethodNode => Unit]): MethodNode =
    val node = method("<clinit>", "()V")
    windows.foreach(_(node))
    node.instructions.add(new InsnNode(Opcodes.RETURN))
    node

  private def initWindow(owner: String, literal: String, storage: String, offsetName: String)(node: MethodNode): Unit =
    val insns = node.instructions
    insns.add(module)
    insns.add(new LdcInsnNode(Type.getObjectType(literal)))
    insns.add(new LdcInsnNode(storage))
    insns.add(
      invoke(
        Opcodes.INVOKEVIRTUAL,
        "java/lang/Class",
        "getDeclaredField",
        "(Ljava/lang/String;)Ljava/lang/reflect/Field;",
      )
    )
    insns.add(invoke(Opcodes.INVOKEVIRTUAL, "scala/runtime/LazyVals$", "getOffsetStatic", offsetStatic))
    insns.add(new FieldInsnNode(Opcodes.PUTSTATIC, owner, offsetName, "J"))

  private def cas(owner: String, offsetName: String, nops: Int = 0): MethodNode =
    val node  = method("cas", "()V")
    val insns = node.instructions
    insns.add(module)
    insns.add(new InsnNode(Opcodes.ACONST_NULL))
    insns.add(new FieldInsnNode(Opcodes.GETSTATIC, owner, offsetName, "J"))
    insns.add(new InsnNode(Opcodes.ACONST_NULL))
    insns.add(new InsnNode(Opcodes.ACONST_NULL))
    insns.add(invoke(Opcodes.INVOKEVIRTUAL, "scala/runtime/LazyVals$", "objCAS", objCas))
    insns.add(new InsnNode(Opcodes.POP))
    List.fill(nops)(new InsnNode(Opcodes.NOP)).foreach(insns.add)
    insns.add(new InsnNode(Opcodes.RETURN))
    node

  private def waiting(owner: String, offsetName: String): MethodNode =
    val node  = method("cas", "()V")
    val insns = node.instructions
    insns.add(module)
    insns.add(new InsnNode(Opcodes.ACONST_NULL))
    insns.add(new FieldInsnNode(Opcodes.GETSTATIC, owner, offsetName, "J"))
    insns.add(new InsnNode(Opcodes.ACONST_NULL))
    insns.add(new TypeInsnNode(Opcodes.NEW, "java/lang/Object"))
    insns.add(new InsnNode(Opcodes.DUP))
    insns.add(invoke(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V"))
    insns.add(invoke(Opcodes.INVOKEVIRTUAL, "scala/runtime/LazyVals$", "objCAS", objCas))
    insns.add(new InsnNode(Opcodes.POP))
    insns.add(new InsnNode(Opcodes.RETURN))
    node

  private def call(name: String, desc: String): MethodNode =
    val node = method("cas", "()V")
    node.instructions.add(invoke(Opcodes.INVOKEVIRTUAL, "scala/runtime/LazyVals$", name, desc))
    node.instructions.add(new InsnNode(Opcodes.RETURN))
    node

  private def module: FieldInsnNode =
    new FieldInsnNode(Opcodes.GETSTATIC, "scala/runtime/LazyVals$", "MODULE$", "Lscala/runtime/LazyVals$;")

  private def invoke(opcode: Int, owner: String, name: String, desc: String): MethodInsnNode =
    new MethodInsnNode(opcode, owner, name, desc, false)
end LazyValsLawSpec
