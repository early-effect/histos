package histos.splice

import zio.*
import zio.test.*

import java.nio.file.{Files, Path}
import java.security.MessageDigest

object RunSpec extends ZIOSpecDefault:

  def spec =
    suite("Run")(
      test("executes spliced escape-string-regexp against a document shim") {
        val jsBytes = vendorBytes("escape-string-regexp@5.0.0.js")
        for
          dir <- tempDir
          js = dir.resolve("escape-string-regexp.js")
          _ <- write(js, String(jsBytes, java.nio.charset.StandardCharsets.UTF_8))
          out = dir.resolve("splice.js")
          _ <- Splice.run(
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
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          JsHost.evalExpr(body, "document.getElementById('out').textContent") == "hello\\?",
          !body.contains("""from "escape-string-regexp""""),
        )
      },
      test("executes spliced preact 10.26.4 h() from the published module") {
        val jsBytes = vendorBytes("preact@10.26.4.module.js")
        val pinned  =
          String(vendorBytes("preact@10.26.4.module.js.sha256"), java.nio.charset.StandardCharsets.UTF_8).trim
        for
          dir <- tempDir
          js = dir.resolve("preact.module.js")
          _ <- write(js, String(jsBytes, java.nio.charset.StandardCharsets.UTF_8))
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  """import * as Preact from "preact";
                    |document.getElementById("out").textContent = Preact.h("h1", null, "ok").type;
                    |""".stripMargin,
                )
              ),
              libs = Map("preact" -> js),
              output = out,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          sha256(jsBytes) == pinned,
          pinned == "2ce1b7b810fc14cda3f4242e636311b5a5e6d5a6cb1f3274fe948fe3bba3d32e",
          JsHost.evalExpr(body, "document.getElementById('out').textContent") == "h1",
          !body.contains("""from "preact""""),
        )
      },
      test("executes Closure-optimized splice of a Preact class-extends setState and render") {
        val jsBytes = vendorBytes("preact@10.26.4.module.js")
        val linker  =
          """var $superClass = __splice_preact.Component;
            |class HelloComponent extends $superClass {
            |  constructor(props, context) { super(props, context); }
            |  render() { return "OVERRIDE_RENDER"; }
            |  componentWillMount() { this.setState({ v: 1 }); }
            |}
            |var c = new HelloComponent({}, {});
            |c.componentWillMount();
            |document.getElementById("out").textContent = String(c.render());
            |""".stripMargin
        for
          dir <- tempDir
          js = dir.resolve("preact.module.js")
          _ <- write(js, String(jsBytes, java.nio.charset.StandardCharsets.UTF_8))
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(LinkerFile("main.js", linker)),
              libs = Map("preact" -> js),
              output = out,
              minify = Minify.Closure,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          JsHost.evalExpr(body, "document.getElementById('out').textContent") == "OVERRIDE_RENDER",
          body.contains("setState"),
        )
      },
      test("executes Closure-optimized splice of preact h()") {
        val jsBytes = vendorBytes("preact@10.26.4.module.js")
        for
          dir <- tempDir
          js = dir.resolve("preact.module.js")
          _ <- write(js, String(jsBytes, java.nio.charset.StandardCharsets.UTF_8))
          out = dir.resolve("splice.js")
          _ <- Splice.run(
            SpliceInput(
              linker = List(
                LinkerFile(
                  "main.js",
                  """const Preact = __splice_preact;
                    |document.getElementById("out").textContent = Preact.h("h1", null, "ok").type;
                    |""".stripMargin,
                )
              ),
              libs = Map("preact" -> js),
              output = out,
              minify = Minify.Closure,
            )
          )
          body <- ZIO.attempt(Files.readString(out))
        yield assertTrue(
          JsHost.evalExpr(body, "document.getElementById('out').textContent") == "h1",
          !body.contains("""from "preact""""),
        )
      },
    )

  private def vendorBytes(name: String): Array[Byte] =
    val in = Option(getClass.getResourceAsStream(s"/vendor/$name"))
      .getOrElse(sys.error(s"missing resource /vendor/$name"))
    try in.readAllBytes()
    finally in.close()

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map("%02x".format(_)).mkString

  private def tempDir: UIO[Path] =
    ZIO.attempt(Files.createTempDirectory("sbt-splice-run-")).orDie

  private def write(path: Path, body: String): Task[Unit] =
    ZIO.attempt {
      Files.writeString(path, body)
      ()
    }
end RunSpec
