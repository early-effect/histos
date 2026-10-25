package histos.pack

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.Type
import org.objectweb.asm.tree.*

import scala.jdk.CollectionConverters.*

/** Rewrite Scala 3.3 through 3.7 lazy vals to the VarHandle form Scala 3.8 emits. Those versions call
  * `LazyVals.getOffsetStatic` and `LazyVals.objCAS`, which use `sun.misc.Unsafe`. `scala.runtime.LazyVals` is that
  * implementation, so its class files stay. When the offset lives on another class, that class publishes the VarHandle
  * with `privateLookupIn`.
  */
object LazyVals:
  private val LazyValsClass = "scala/runtime/LazyVals$"
  private val LazyValsType  = s"L$LazyValsClass;"
  private val OffsetStatic  = "(Ljava/lang/reflect/Field;)J"
  private val ObjCas        = "(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z"
  private val DeclaredField = "(Ljava/lang/String;)Ljava/lang/reflect/Field;"
  private val VarHandle     = "Ljava/lang/invoke/VarHandle;"
  private val FindVarHandle = "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/invoke/VarHandle;"
  private val CompareAndSet = "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z"

  /** `OFFSET$` field, the class that declares it, and the VarHandle that replaces it. */
  final case class Handle(owner: String, offset: String, handle: String)

  def catalog(classes: List[(String, Array[Byte])]): Either[LazyValError, Map[(String, String), Handle]] =
    classes.foldLeft[Either[LazyValError, Map[(String, String), Handle]]](Right(Map.empty)): (acc, item) =>
      acc.flatMap: seen =>
        val (entry, bytes) = item
        if !old(bytes) || runtime(entry) then Right(seen)
        else bindings(entry, bytes).map(found => seen ++ found)

  def rewrite(
      entry: String,
      bytes: Array[Byte],
      handles: Map[(String, String), Handle],
      loader: ClassLoader,
  ): Either[LazyValError, Array[Byte]] =
    if !old(bytes) || runtime(entry) then Right(bytes)
    else
      val reader = new org.objectweb.asm.ClassReader(bytes)
      val node   = new ClassNode()
      reader.accept(node, 0)
      for
        _       <- methods(entry, node, handles)
        written <- frames(entry, reader, node, loader)
      yield written

  private def runtime(entry: String): Boolean =
    entry == "scala/runtime/LazyVals.class" || entry == "scala/runtime/LazyVals$.class"

  private def old(bytes: Array[Byte]): Boolean =
    val text = new String(bytes, java.nio.charset.StandardCharsets.ISO_8859_1)
    text.contains("getOffsetStatic") || text.contains("objCAS")

  private def bindings(entry: String, bytes: Array[Byte]): Either[LazyValError, Map[(String, String), Handle]] =
    val node = new ClassNode()
    new org.objectweb.asm.ClassReader(bytes).accept(node, 0)
    method(node, "<clinit>") match
      case None         => Right(Map.empty)
      case Some(clinit) =>
        clinit.instructions.iterator.asScala
          .filter(_.getOpcode >= 0)
          .toList
          .sliding(6, 1)
          .toList
          .foldLeft[Either[LazyValError, Map[(String, String), Handle]]](Right(Map.empty)): (acc, window) =>
            acc.flatMap: seen =>
              offsetInit(window) match
                case None         => Right(seen)
                case Some(handle) => Right(seen.updated((handle.owner, handle.offset), handle))

  private def methods(
      entry: String,
      node: ClassNode,
      handles: Map[(String, String), Handle],
  ): Either[LazyValError, Unit] =
    val own     = handles.values.filter(_.owner == node.name).toList
    val visible = own.map: handle =>
      val flags =
        node.fields.asScala.find(_.name == handle.offset).map(_.access).getOrElse(ACC_PUBLIC | ACC_STATIC | ACC_FINAL)
      handle.handle -> flags
    val dropped = node.fields.asScala.filter(field => handles.contains((node.name, field.name))).toList
    dropped.foreach(field => node.fields.remove(field))
    visible.foreach: (handle, flags) =>
      node.fields.add(new FieldNode(flags, handle, VarHandle, null, null))
    node.methods.asScala.foldLeft[Either[LazyValError, Unit]](Right(())): (acc, method) =>
      acc.flatMap: _ =>
        rewriteMethod(entry, node.name, method, handles)

  private def rewriteMethod(
      entry: String,
      className: String,
      method: MethodNode,
      handles: Map[(String, String), Handle],
  ): Either[LazyValError, Unit] =
    val real    = method.instructions.iterator.asScala.filter(_.getOpcode >= 0).toList
    val matched = (real
      .sliding(6, 1)
      .toList
      .filter(window => window.size == 6 && (offsetInit(window).nonEmpty || objCas(window))) ++
      real.sliding(8, 1).toList.filter(window => window.size == 8 && waiting(window)))
    val covered   = matched.flatten.toSet
    val uncovered = real.filter(insn => callsOld(insn) && !covered.contains(insn))
    if uncovered.nonEmpty then Left(LazyValError.Unhandled(entry, s"${method.name} still calls Unsafe lazy vals"))
    else
      matched.foldLeft[Either[LazyValError, Unit]](Right(())): (acc, window) =>
        acc.flatMap: _ =>
          if offsetInit(window).nonEmpty then
            replaceInit(className, method, window)
            Right(())
          else if window.size == 8 then replaceCas(entry, method, window, handles, invoke = 7)
          else replaceCas(entry, method, window, handles, invoke = 5)

  private def replaceInit(className: String, method: MethodNode, window: List[AbstractInsnNode]): Unit =
    window(2) match
      case name: LdcInsnNode =>
        val storage = name.cst match
          case text: String => text
          case _            => ""
        window(5) match
          case put: FieldInsnNode =>
            val handle = s"$storage$$lzyHandle"
            val list   = method.instructions
            list.set(
              window(0),
              new MethodInsnNode(
                INVOKESTATIC,
                "java/lang/invoke/MethodHandles",
                "lookup",
                "()Ljava/lang/invoke/MethodHandles$Lookup;",
                false,
              ),
            )
            declaredClass(window(1)) match
              case Some(owner) if owner != className =>
                val swap     = new InsnNode(SWAP)
                val lookupIn = new MethodInsnNode(
                  INVOKESTATIC,
                  "java/lang/invoke/MethodHandles",
                  "privateLookupIn",
                  "(Ljava/lang/Class;Ljava/lang/invoke/MethodHandles$Lookup;)Ljava/lang/invoke/MethodHandles$Lookup;",
                  false,
                )
                list.insert(window(1), swap)
                list.insert(swap, lookupIn)
                list.insert(lookupIn, new LdcInsnNode(Type.getObjectType(owner)))
              case _ => ()
            list.set(window(3), new LdcInsnNode(Type.getType("Ljava/lang/Object;")))
            list.set(
              window(4),
              new MethodInsnNode(
                INVOKEVIRTUAL,
                "java/lang/invoke/MethodHandles$Lookup",
                "findVarHandle",
                FindVarHandle,
                false,
              ),
            )
            list.set(window(5), new FieldInsnNode(PUTSTATIC, put.owner, handle, VarHandle))
          case _ => ()
      case _ => ()

  private def declaredClass(insn: AbstractInsnNode): Option[String] =
    insn match
      case ldc: LdcInsnNode =>
        ldc.cst match
          case tpe: Type => Some(tpe.getInternalName)
          case _         => None
      case _ => None

  private def replaceCas(
      entry: String,
      method: MethodNode,
      window: List[AbstractInsnNode],
      handles: Map[(String, String), Handle],
      invoke: Int,
  ): Either[LazyValError, Unit] =
    window(2) match
      case offset: FieldInsnNode =>
        handles.get((offset.owner, offset.name)) match
          case None         => Left(LazyValError.Unhandled(entry, s"${offset.owner}.${offset.name} has no VarHandle"))
          case Some(handle) =>
            val list = method.instructions
            list.set(window.head, new FieldInsnNode(GETSTATIC, handle.owner, handle.handle, VarHandle))
            list.remove(offset)
            list.set(
              window(invoke),
              new MethodInsnNode(INVOKEVIRTUAL, "java/lang/invoke/VarHandle", "compareAndSet", CompareAndSet, false),
            )
            Right(())
      case _ => Left(LazyValError.Unhandled(entry, s"${method.name} has an unexpected offset load"))

  private def frames(
      entry: String,
      reader: org.objectweb.asm.ClassReader,
      node: ClassNode,
      loader: ClassLoader,
  ): Either[LazyValError, Array[Byte]] =
    try
      val writer = new ClassWriter(reader, ClassWriter.COMPUTE_FRAMES):
        override def getCommonSuperClass(type1: String, type2: String): String =
          common(loader, type1, type2)
      node.accept(writer)
      Right(writer.toByteArray)
    catch case error: Exception => Left(LazyValError.Unhandled(entry, error.getMessage))

  private def common(loader: ClassLoader, type1: String, type2: String): String =
    try
      var first  = Class.forName(type1.replace('/', '.'), false, loader)
      val second = Class.forName(type2.replace('/', '.'), false, loader)
      if first.isAssignableFrom(second) then type1
      else if second.isAssignableFrom(first) then type2
      else if first.isInterface || second.isInterface then "java/lang/Object"
      else
        while !first.isAssignableFrom(second) do first = first.getSuperclass
        first.getName.replace('.', '/')
    catch case _: Exception => "java/lang/Object"

  private def offsetInit(window: List[AbstractInsnNode]): Option[Handle] =
    window match
      case List(module, classLiteral, fieldName, declared, offset, put)
          if staticModule(module) &&
            ldcType(classLiteral) &&
            ldcString(fieldName) &&
            method(declared, "java/lang/Class", "getDeclaredField", DeclaredField) &&
            method(offset, LazyValsClass, "getOffsetStatic", OffsetStatic) &&
            putStaticLong(put) =>
        val name  = fieldName.asInstanceOf[LdcInsnNode].cst.toString
        val field = put.asInstanceOf[FieldInsnNode]
        if name.contains("$lzy") then Some(Handle(field.owner, field.name, s"$name$$lzyHandle"))
        else None
      case _ => None

  private def waiting(window: List[AbstractInsnNode]): Boolean =
    window.size == 8 &&
      staticModule(window(0)) &&
      getStaticLong(window(2)) &&
      window(4).getOpcode == NEW &&
      window(5).getOpcode == DUP &&
      method(window(7), LazyValsClass, "objCAS", ObjCas)

  private def objCas(window: List[AbstractInsnNode]): Boolean =
    window match
      case List(module, _, offset, _, _, call) =>
        staticModule(module) && getStaticLong(offset) && method(call, LazyValsClass, "objCAS", ObjCas)
      case _ => false

  private def callsOld(insn: AbstractInsnNode): Boolean =
    method(insn, LazyValsClass, "getOffsetStatic", OffsetStatic) || method(insn, LazyValsClass, "objCAS", ObjCas)

  private def method(node: ClassNode, name: String): Option[MethodNode] =
    node.methods.asScala.find(_.name == name)

  private def staticModule(insn: AbstractInsnNode): Boolean =
    field(insn, GETSTATIC, LazyValsClass, "MODULE$", LazyValsType)

  private def getStaticLong(insn: AbstractInsnNode): Boolean =
    insn match
      case field: FieldInsnNode => field.getOpcode == GETSTATIC && field.desc == "J" && field.name.startsWith("OFFSET$")
      case _                    => false

  private def putStaticLong(insn: AbstractInsnNode): Boolean =
    insn match
      case field: FieldInsnNode => field.getOpcode == PUTSTATIC && field.desc == "J" && field.name.startsWith("OFFSET$")
      case _                    => false

  private def ldcType(insn: AbstractInsnNode): Boolean =
    insn match
      case ldc: LdcInsnNode => ldc.cst.isInstanceOf[Type]
      case _                => false

  private def ldcString(insn: AbstractInsnNode): Boolean =
    insn match
      case ldc: LdcInsnNode => ldc.cst.isInstanceOf[String]
      case _                => false

  private def field(insn: AbstractInsnNode, opcode: Int, owner: String, name: String, desc: String): Boolean =
    insn match
      case node: FieldInsnNode =>
        node.getOpcode == opcode && node.owner == owner && node.name == name && node.desc == desc
      case _ => false

  private def method(insn: AbstractInsnNode, owner: String, name: String, desc: String): Boolean =
    insn match
      case node: MethodInsnNode => node.owner == owner && node.name == name && node.desc == desc
      case _                    => false
end LazyVals

enum LazyValError:
  case Unhandled(entry: String, detail: String)

  def message: String = this match
    case Unhandled(entry, detail) => s"$entry still uses Unsafe lazy vals: $detail"
end LazyValError
