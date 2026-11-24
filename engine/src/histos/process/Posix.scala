package histos.process

import java.lang.foreign.{FunctionDescriptor, Linker, ValueLayout}
import java.lang.invoke.MethodHandle

import scala.jdk.OptionConverters.*
import scala.util.control.NonFatal

/** `setpgid` and `kill` from libc. A negative pid is the process group, which is how cancel reaches children. */
private[process] object Posix:
  val SigKill: Int = 9
  val SigInt: Int  = 2

  def killGroup(pid: Int): Either[GroupOp, Int] =
    call("kill", -pid, SigKill)

  def signalGroup(pid: Int, signal: Int): Either[GroupOp, Int] =
    call("kill", -pid, signal)

  /** Signal 0 asks whether any member of the group is alive. It does not deliver a signal. */
  def occupied(pid: Int): Boolean =
    if pid <= 0 then false
    else
      signalGroup(pid, 0) match
        case Right(0) => true
        case _        => false

  private def call(name: String, a: Int, b: Int): Either[GroupOp, Int] =
    handle(name).flatMap: method =>
      try
        method.invokeWithArguments(Integer.valueOf(a), Integer.valueOf(b)) match
          case code: Integer => Right(code.intValue)
          case _             => Left(GroupOp.Call)
      catch case NonFatal(_) => Left(GroupOp.Call)

  private def handle(name: String): Either[GroupOp, MethodHandle] =
    Linker.nativeLinker().defaultLookup().find(name).toScala match
      case None         => Left(GroupOp.Lookup)
      case Some(symbol) =>
        Right(
          Linker
            .nativeLinker()
            .downcallHandle(
              symbol,
              FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT),
            )
        )
end Posix
