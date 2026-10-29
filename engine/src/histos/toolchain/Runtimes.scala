package histos.toolchain

import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.node.{NodeBinary, NodeMiss, NodeToolchain, NodeRuntime}
import histos.process.Process
import histos.Platform
import histos.step.{Input, InputName, InputValue}
import zio.{Chunk, UIO, ZIO}

/** What one command links and runs with, read once from [[Machine]]: the environment children start from, the Scala
  * Native origin, and Node. Node is chosen at the first Js launch and kept for the rest of the command, so a command
  * that launches no Js does not look for it.
  */
final class Runtimes private (
    val env: Map[String, String],
    val native: NativeOrigin,
    val nodeRuntime: ZIO[Process, NodeMiss, NodeRuntime],
):
  def node: ZIO[Process, NodeMiss, NodeBinary]                         = nodeRuntime.map(_.binary)
  def inputs(platform: Platform): ZIO[Process, NodeMiss, Chunk[Input]] = platform match
    case Platform.Js =>
      nodeRuntime.map(runtime =>
        Chunk(
          Input(
            InputName("node"),
            InputValue.Text(Runtimes.identityToken(runtime.identity)),
          )
        )
      )
    case Platform.Jvm | Platform.Native => ZIO.succeed(Chunk.empty)

object Runtimes:
  def identityToken(identity: RuntimeIdentity): String = identity match
    case RuntimeIdentity.Local(home, salt) =>
      s"local:${home.toString}:${salt.value}"

  def read(machine: Machine): UIO[Runtimes] =
    NodeToolchain
      .resolve(machine)
      .memoize
      .map(node => new Runtimes(machine.env, machine.native, node))
end Runtimes
