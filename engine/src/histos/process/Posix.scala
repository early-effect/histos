package histos.process

import java.lang.foreign.{FunctionDescriptor, Linker, ValueLayout}
import java.lang.invoke.MethodHandle

import scala.jdk.OptionConverters.*

/** `setpgid` and `kill` from libc. A negative pid is the process group, which is how cancel reaches children. */
private[process] object Posix:
  val SigKill: Int = 9

  def killGroup(pid: Int): Either[GroupOp, Int] =
    call("kill", -pid, SigKill)

  private def call(name: String, a: Int, b: Int): Either[GroupOp, Int] =
    handle(name).flatMap: method =>
      try
        method.invokeWithArguments(Integer.valueOf(a), Integer.valueOf(b)) match
          case code: Integer => Right(code.intValue)
          case _             => Left(GroupOp.Call)
      catch case _: Throwable => Left(GroupOp.Call)

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
