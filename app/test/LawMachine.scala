package histos

import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Machine, Os, Sha256}
import histos.native.NativeOrigin
import histos.toolchain.{Architecture, RuntimeIdentity}
import histos.zinc.GraphCompile

import java.nio.file.Path

import zio.{UIO, ZIO}
import zio.test.TestSystem

/** The machine a law runs on. The JDK, the OS, and the architecture are the test JVM's own. The home and the content
  * store are under the law's directory and the environment is empty, so nothing reads the developer's home, PATH, or
  * shell.
  */
object LawMachine:
  def at(dir: Path): UIO[Machine] =
    running.map: (javaHome, os, arch) =>
      Machine(
        Map.empty,
        dir.toString,
        Os.from(os),
        Architecture.fromMachine(arch),
        dir.resolve("store"),
        dir,
        Some(Path.of(javaHome)),
        NativeOrigin.Central,
      )

  /** The same machine through `TestSystem`, for product code that calls `Machine.load` itself. */
  def system(dir: Path): UIO[Unit] =
    running.flatMap: (javaHome, os, arch) =>
      TestSystem.putProperty("java.home", javaHome) *>
        TestSystem.putProperty("os.name", os) *>
        TestSystem.putProperty("os.arch", arch) *>
        TestSystem.putProperty("user.home", dir.toString)

  /** The test JVM's JDK, probed like any home. A law that compiles needs a real one. */
  def jdk(machine: Machine): UIO[JdkRuntime] =
    GraphCompile.jdk(machine).orDieWith(error => IllegalStateException(error.message))

  /** A JDK for a law that never compiles. */
  val fixtureJdk: JdkRuntime =
    JdkRuntime(
      Path.of("jdk"),
      JdkVersion.trusted("25", JdkMajor.trusted(25)),
      RuntimeIdentity.Local(Path.of("jdk"), Sha256.trusted("ab")),
    )

  private def running: UIO[(String, String, String)] =
    ZIO.succeed(
      (
        java.lang.System.getProperty("java.home"),
        java.lang.System.getProperty("os.name"),
        java.lang.System.getProperty("os.arch"),
      )
    )
end LawMachine
