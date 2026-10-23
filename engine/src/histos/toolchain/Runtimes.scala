package histos.toolchain

import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.node.{NodeBinary, NodeMiss, NodeToolchain}

import zio.{IO, UIO}

/** What one command links and runs with, read once from [[Machine]]: the
  * environment children start from, the Scala Native origin, and Node. Node is
  * chosen at the first Js launch and kept for the rest of the command, so a
  * command that launches no Js does not look for it.
  */
final class Runtimes private (
    val env: Map[String, String],
    val native: NativeOrigin,
    val node: IO[NodeMiss, NodeBinary]
)

object Runtimes:
  def read(machine: Machine): UIO[Runtimes] =
    NodeToolchain
      .choose(machine)
      .memoize
      .map(node =>
        new Runtimes(machine.env, NativeOrigin.read(machine.env), node)
      )
