package demo
import zio.*
import zio.test.*
import zio.stream.*
object RightSpec extends ZIOSpecDefault:
  enum Failure:
    case Read, Write
  private def hit = ZIO
    .attemptBlocking:
      val path = java.nio.file.Path.of(sys.env.getOrElse("HISTOS_REPO", "."), "right-counter")
      java.nio.file.Files.writeString(
        path,
        "right\n",
        java.nio.file.StandardOpenOption.CREATE,
        java.nio.file.StandardOpenOption.APPEND,
      )
    .mapError(_ => Failure.Write)
  private def resource = ZIO.scoped:
    ZIO
      .acquireRelease(
        ZIO.fromOption(Option(getClass.getResourceAsStream("/right/value.txt"))).orElseFail(Failure.Read)
      )(input => ZIO.attemptBlocking(input.close()).ignore)
      .flatMap(input =>
        ZIO
          .attemptBlocking(new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8))
          .mapError(_ => Failure.Read)
      )

  def spec = test("right member dependency and resource") {
    for
      _      <- hit
      text   <- resource
      values <- ZStream(1, 2).runCollect
    yield assertTrue(text == "right", values.toList == List(1, 2))
  }
end RightSpec
