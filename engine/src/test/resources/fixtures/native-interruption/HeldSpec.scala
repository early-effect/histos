package demo
import zio.*
import zio.test.*
object HeldSpec extends ZIOSpecDefault:
  enum Failure:
    case Write
  def spec = test("held native process") {
    val marker = java.nio.file.Path.of(sys.env.getOrElse("HISTOS_REPO", "."), "native-pid")
    ZIO
      .attemptBlocking(java.nio.file.Files.writeString(marker, scala.scalanative.posix.unistd.getpid().toString))
      .mapError(_ => Failure.Write) *> ZIO.never
  }
