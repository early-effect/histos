package histos.pack

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.Type
import org.objectweb.asm.tree.*
import org.objectweb.asm.{ClassTooLargeException, LimitExceededException, MethodTooLargeException}

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
    val matched =
      real.sliding(6, 1).toList.filter(window => offsetInit(window).nonEmpty || objCas(window)) ++
        real.sliding(8, 1).toList.filter(waiting)
    val covered   = matched.flatten.toSet
    val uncovered = real.filter(insn => callsOld(insn) && !covered.contains(insn))
    if uncovered.nonEmpty then Left(LazyValError.StillCalls(entry, method.name))
    else
      matched.foldLeft[Either[LazyValError, Unit]](Right(())): (acc, window) =>
        acc.flatMap: _ =>
          if offsetInit(window).nonEmpty then
            replaceInit(className, method, window)
            Right(())
          else replaceCas(entry, method, window, handles)

  private def replaceInit(className: String, method: MethodNode, window: List[AbstractInsnNode]): Unit =
    window match
      case List(module, classLiteral, fieldName, declared, offset, put) =>
        (ldcString(fieldName), putStaticLong(put)) match
          case (Some(storage), Some(field)) =>
            val handle = s"$storage$$lzyHandle"
            val list   = method.instructions
            list.set(
              module,
              new MethodInsnNode(
                INVOKESTATIC,
                "java/lang/invoke/MethodHandles",
                "lookup",
                "()Ljava/lang/invoke/MethodHandles$Lookup;",
                false,
              ),
            )
            declaredClass(classLiteral) match
              case Some(owner) if owner != className =>
                val swap     = new InsnNode(SWAP)
                val lookupIn = new MethodInsnNode(
                  INVOKESTATIC,
                  "java/lang/invoke/MethodHandles",
                  "privateLookupIn",
                  "(Ljava/lang/Class;Ljava/lang/invoke/MethodHandles$Lookup;)Ljava/lang/invoke/MethodHandles$Lookup;",
                  false,
                )
                list.insert(classLiteral, swap)
                list.insert(swap, lookupIn)
                list.insert(lookupIn, new LdcInsnNode(Type.getObjectType(owner)))
              case _ => ()
            list.set(declared, new LdcInsnNode(Type.getType("Ljava/lang/Object;")))
            list.set(
              offset,
              new MethodInsnNode(
                INVOKEVIRTUAL,
                "java/lang/invoke/MethodHandles$Lookup",
                "findVarHandle",
                FindVarHandle,
                false,
              ),
            )
            list.set(put, new FieldInsnNode(PUTSTATIC, field.owner, handle, VarHandle))
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
  ): Either[LazyValError, Unit] =
    val shaped = window match
      case List(module, _, offset, _, _, call)       => Some((module, offset, call))
      case List(module, _, offset, _, _, _, _, call) => Some((module, offset, call))
      case _                                         => None
    shaped match
      case Some((module, offset, call)) =>
        getStaticLong(offset) match
          case None        => Left(LazyValError.OffsetShape(entry, method.name))
          case Some(field) =>
            handles.get((field.owner, field.name)) match
              case None         => Left(LazyValError.MissingHandle(entry, field.owner, field.name))
              case Some(handle) =>
                val list = method.instructions
                list.set(module, new FieldInsnNode(GETSTATIC, handle.owner, handle.handle, VarHandle))
                list.remove(field)
                list.set(
                  call,
                  new MethodInsnNode(
                    INVOKEVIRTUAL,
                    "java/lang/invoke/VarHandle",
                    "compareAndSet",
                    CompareAndSet,
                    false,
                  ),
                )
                Right(())
      case None => Left(LazyValError.OffsetShape(entry, method.name))

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
    catch
      case tooBig: MethodTooLargeException =>
        Left(
          LazyValError.Frames(
            entry,
            LazyValFrame.MethodTooLarge(
              tooBig.getClassName,
              tooBig.getMethodName,
              tooBig.getDescriptor,
              tooBig.getCodeSize,
            ),
          )
        )
      case tooBig: ClassTooLargeException =>
        Left(LazyValError.Frames(entry, LazyValFrame.ClassTooLarge(tooBig.getClassName, tooBig.getConstantPoolCount)))
      case _: LimitExceededException    => Left(LazyValError.Frames(entry, LazyValFrame.Limit))
      case _: IllegalArgumentException  => Left(LazyValError.Frames(entry, LazyValFrame.Rejected))
      case _: IndexOutOfBoundsException => Left(LazyValError.Frames(entry, LazyValFrame.Bounds))

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
            method(declared, "java/lang/Class", "getDeclaredField", DeclaredField) &&
            method(offset, LazyValsClass, "getOffsetStatic", OffsetStatic) =>
        (ldcString(fieldName), putStaticLong(put)) match
          case (Some(name), Some(field)) if name.contains("$lzy") =>
            Some(Handle(field.owner, field.name, s"$name$$lzyHandle"))
          case _ => None
      case _ => None

  private def waiting(window: List[AbstractInsnNode]): Boolean =
    window match
      case List(module, _, offset, _, created, duplicated, _, call) =>
        staticModule(module) &&
        getStaticLong(offset).isDefined &&
        created.getOpcode == NEW &&
        duplicated.getOpcode == DUP &&
        method(call, LazyValsClass, "objCAS", ObjCas)
      case _ => false

  private def objCas(window: List[AbstractInsnNode]): Boolean =
    window match
      case List(module, _, offset, _, _, call) =>
        staticModule(module) && getStaticLong(offset).isDefined && method(call, LazyValsClass, "objCAS", ObjCas)
      case _ => false

  private def callsOld(insn: AbstractInsnNode): Boolean =
    method(insn, LazyValsClass, "getOffsetStatic", OffsetStatic) || method(insn, LazyValsClass, "objCAS", ObjCas)

  private def method(node: ClassNode, name: String): Option[MethodNode] =
    node.methods.asScala.find(_.name == name)

  private def staticModule(insn: AbstractInsnNode): Boolean =
    field(insn, GETSTATIC, LazyValsClass, "MODULE$", LazyValsType)

  private def getStaticLong(insn: AbstractInsnNode): Option[FieldInsnNode] =
    insn match
      case field: FieldInsnNode
          if field.getOpcode == GETSTATIC && field.desc == "J" && field.name.startsWith("OFFSET$") =>
        Some(field)
      case _ => None

  private def putStaticLong(insn: AbstractInsnNode): Option[FieldInsnNode] =
    insn match
      case field: FieldInsnNode
          if field.getOpcode == PUTSTATIC && field.desc == "J" && field.name.startsWith("OFFSET$") =>
        Some(field)
      case _ => None

  private def ldcType(insn: AbstractInsnNode): Boolean =
    insn match
      case ldc: LdcInsnNode => ldc.cst.isInstanceOf[Type]
      case _                => false

  private def ldcString(insn: AbstractInsnNode): Option[String] =
    insn match
      case ldc: LdcInsnNode =>
        ldc.cst match
          case text: String => Some(text)
          case _            => None
      case _ => None

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

enum LazyValFrame:
  case Rejected
  case MethodTooLarge(owner: String, method: String, descriptor: String, codeSize: Int)
  case ClassTooLarge(owner: String, pool: Int)
  case Limit
  case Bounds

  def message: String = this match
    case Rejected                                            => "the bytecode was rejected"
    case MethodTooLarge(owner, method, descriptor, codeSize) =>
      s"$owner.$method $descriptor is $codeSize bytes"
    case ClassTooLarge(owner, pool) => s"$owner has $pool constant pool entries"
    case Limit                      => "an ASM limit was exceeded"
    case Bounds                     => "the frame merge was out of bounds"
end LazyValFrame

enum LazyValError:
  case StillCalls(entry: String, method: String)
  case MissingHandle(entry: String, owner: String, offset: String)
  case OffsetShape(entry: String, method: String)
  case Frames(entry: String, failure: LazyValFrame)

  def message: String = this match
    case StillCalls(entry, method)           => s"$entry still calls Unsafe lazy vals from $method"
    case MissingHandle(entry, owner, offset) =>
      s"$entry has no VarHandle for $owner.$offset"
    case OffsetShape(entry, method) => s"$entry cannot rewrite the offset load in $method"
    case Frames(entry, failure)     => s"$entry could not compute stack map frames: ${failure.message}"
end LazyValError
