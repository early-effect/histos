package demo
import zio.*
import zio.test.*
import zio.json.*
object LeftSpec extends ZIOSpecDefault:
  enum Failure:
    case Read, Write
  private def hit = ZIO
    .attemptBlocking:
      val path = java.nio.file.Path.of(sys.env.getOrElse("HISTOS_REPO", "."), "left-counter")
      java.nio.file.Files.writeString(
        path,
        "left\n",
        java.nio.file.StandardOpenOption.CREATE,
        java.nio.file.StandardOpenOption.APPEND,
      )
    .mapError(_ => Failure.Write)
  private def resource = ZIO.scoped:
    ZIO
      .acquireRelease(ZIO.fromOption(Option(getClass.getResourceAsStream("/left/value.txt"))).orElseFail(Failure.Read))(
        input => ZIO.attemptBlocking(input.close()).ignore
      )
      .flatMap(input =>
        ZIO
          .attemptBlocking(new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8))
          .mapError(_ => Failure.Read)
      )

  def spec = test("left member dependency and resource") {
    for
      _    <- hit
      text <- resource
    yield assertTrue("42".fromJson[Int] == Right(42), text.startsWith("left-"))
  }
end LeftSpec
