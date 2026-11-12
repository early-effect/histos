package histos.workflow

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** A pull request is driven by the pin's client. No job in this repository's `ci.yml` links this commit's client or
  * fetches one, and no job waits on a link job. `workflow --check` proves the committed file is the render.
  */
object CiClientLawSpec extends ZIOSpecDefault:
  def spec = suite("ci client laws")(
    test("this repository's ci.yml links no client, downloads none, and needs no link job"):
      for
        repo <- ZIO.systemWith(_.env("HISTOS_REPO")).orDie.someOrElseZIO(ZIO.dieMessage("HISTOS_REPO is unset"))
        text <- ZIO.attemptBlocking(Files.readString(Path.of(repo, ".github", "workflows", "ci.yml"))).orDie
        lines = text.linesIterator.map(_.trim).toList
      yield assertTrue(
        !lines.exists(line => line.startsWith("- run:") && line.contains("histos client")),
        !lines.exists(_.contains("histos-client")),
        !lines.exists(_.contains("link-client")),
      )
  ) @@ TestAspect.withLiveSystem
