package histos.splice

import zio.*

import java.nio.file.{Files, Path}

/** Build a tiny page that runs spliced escape-string-regexp. Shared by unit tests and the e2e module. */
object SmokeSite:

  def prepare(publishSmoke: Boolean): Task[Path] =
    val jsBytes = Option(getClass.getResourceAsStream("/vendor/escape-string-regexp@5.0.0.js"))
      .getOrElse(sys.error("missing resource /vendor/escape-string-regexp@5.0.0.js"))
      .readAllBytes()
    for
      dir <- ZIO.attempt(Files.createTempDirectory("sbt-splice-smoke-"))
      js = dir.resolve("escape-string-regexp.js")
      _ <- ZIO.attempt(Files.write(js, jsBytes))
      out = dir.resolve("fast.js")
      _ <- Splice
        .run(
          SpliceInput(
            linker = List(
              LinkerFile(
                "main.js",
                """import * as escapeStringRegexp from "escape-string-regexp";
                  |document.getElementById("out").textContent = escapeStringRegexp.default("hello?");
                  |""".stripMargin,
              )
            ),
            libs = Map("escape-string-regexp" -> js),
            output = out,
          )
        )
        .mapError(e => new RuntimeException(e.message))
      html = dir.resolve("index.html")
      _ <- ZIO.attempt {
        Files.writeString(
          html,
          """<!doctype html>
            |<html lang="en"><head><meta charset="utf-8"><title>splice smoke</title></head>
            |<body>
            |<p id="out">pending</p>
            |<script src="fast.js"></script>
            |</body></html>
            |""".stripMargin,
        )
      }
      _ <- ZIO.when(publishSmoke) {
        ZIO.attempt {
          val smoke = Path.of("target/splice-smoke")
          Files.createDirectories(smoke)
          Files.write(smoke.resolve("fast.js"), Files.readAllBytes(out))
          Files.write(smoke.resolve("index.html"), Files.readAllBytes(html))
          ()
        }
      }
    yield dir
end SmokeSite
