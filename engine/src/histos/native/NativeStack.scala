package histos.native

import histos.ThreadStack

/** What a linked Native binary reads from its environment. Scala Native sizes every thread it starts from this, and
  * otherwise takes max(1MB, the OS default): 8MB on Linux, 1MB on macOS.
  */
object NativeStack:
  def environment(stack: ThreadStack): Map[String, String] =
    Map("SCALANATIVE_THREAD_STACK_SIZE" -> stack.value)
