package histos.load

import histos.plugin.*
import histos.schedule.PluginManifest
import java.nio.file.{Files, LinkOption}
import zio.*
import zio.test.*
import zio.json.*

object PluginProductSafetySpec extends ZIOSpecDefault:
  private val tests = suite("owned product safety review")(
    test("owned cache API retains a committed-generation capability") {
      val errors = scala.compiletime.testing.typeCheckErrors("""
        def capability(
            scheduler: histos.schedule.Scheduler,
            action: histos.schedule.Action,
            digest: histos.schedule.Digest,
            request: histos.load.ProductRequest,
        ) = scheduler.cached(action, digest, request, zio.ZIO.fail(histos.load.ProductError.Changed))
          .map(result => result.outputs.read)
      """)
      assertTrue(errors.isEmpty)
    },
    test("mixed-case engine root is refused by stage and clean even with an unrelated active lease") {
      reserved(".HISTOS")
    },
    test("mixed-case engine descendant is refused by stage and clean even with an unrelated active lease") {
      reserved(".hIsToS/products")
    },
    test("SCM and BSP metadata aliases are reserved while unrelated generations remain leased") {
      reserved(".GiT").zipWith(reserved(".BsP/histos.json"))(_ && _)
    },
    test("File clean unlinks only its file or alias and leaves siblings and Directory trees correctly owned") {
      ProductFixture.use: fixture =>
        for
          _      <- fixture.produce("old")
          _      <- fixture.write(fixture.root.resolve("adjacent.txt"), "unowned")
          _      <- fixture.products.clean(fixture.request)
          absent <- fixture.io(
            Files.notExists(fixture.root.resolve("page.txt"), LinkOption.NOFOLLOW_LINKS) &&
              Files.notExists(fixture.root.resolve("site"), LinkOption.NOFOLLOW_LINKS)
          )
          _ <- fixture.write(fixture.root.resolve("page.txt"), "regular")
          request = fixture.request.copy(task =
            ProductFixture.task.copy(outputs =
              List(
                DeclaredOutput.File(OutputName("page"), RepoPath("page.txt"))
              )
            )
          )
          _        <- fixture.products.clean(request)
          fileGone <- fixture.io(Files.notExists(fixture.root.resolve("page.txt"), LinkOption.NOFOLLOW_LINKS))
          adjacent <- fixture.read("adjacent.txt")
        yield assertTrue(absent, fileGone, adjacent == "unowned")
    },
    test("File clean refuses a physical directory and preserves its children") {
      ProductFixture.use: fixture =>
        val request = fixture.request.copy(task =
          ProductFixture.task.copy(outputs =
            List(
              DeclaredOutput.File(OutputName("page"), RepoPath("page.txt"))
            )
          )
        )
        for
          _        <- fixture.write(fixture.root.resolve("page.txt/child.txt"), "child")
          _        <- fixture.write(fixture.root.resolve("adjacent.txt"), "unowned")
          result   <- fixture.products.clean(request).either
          child    <- optional(fixture, fixture.root.resolve("page.txt/child.txt"))
          adjacent <- fixture.read("adjacent.txt")
        yield assertTrue(
          result == Left(ProductError.Kind(fixture.root.resolve("page.txt"), OutputKind.File)),
          child.contains("child"),
          adjacent == "unowned",
        )
    },
    test("generic entry parsing and stored manifests reject absolute, empty-segment, traversal and NUL paths") {
      ProductFixture.use: fixture =>
        val invalid = List(
          "",
          "/absolute",
          "assets//x",
          "assets/../x",
          "assets/./x",
          "assets/x/",
          "assets\\\\x",
          "C:/x",
          "assets/x\u0000",
        )
        for
          committed <- fixture.produce("old")
          stored  = PluginManifest.store(committed.manifest)
          decoded = invalid.map(raw => PluginManifest.products(stored.replace("\"assets/client.js\"", raw.toJson)))
        yield assertTrue(
          invalid.forall(raw => ProductEntryPath.from(raw) == Left(ProductError.InvalidEntry(raw))),
          decoded.forall(_.isLeft),
          ProductEntryPath("assets/@scope/client+v1.js").value == "assets/@scope/client+v1.js",
        )
    },
    test("generic resource and site names seal, roundtrip and replay without narrowing archive grammar") {
      ProductFixture.use: fixture =>
        for
          _            <- fixture.produce("old")
          sealedResult <- ZIO.scoped:
            for
              staged <- fixture.products.stage(fixture.request)
              _      <- fixture.fill(staged, "new")
              site <- ZIO.fromEither(staged.destinations.directory(OutputName("site"))).mapError(ProductError.Report(_))
              _    <- fixture.write(site.resolve("assets/@scope/client+v1.js"), "client")
              _    <- fixture.write(site.resolve("notes/front page.html"), "page")
              _    <- fixture.io(Files.createDirectories(site.resolve("empty @+dir")))
              result <- fixture.products.seal(staged, ProductFixture.report).either
            yield result
          observed <- sealedResult match
            case Left(error)     => ZIO.succeed(Left(error))
            case Right(manifest) =>
              for
                parsed           <- ZIO.fromEither(PluginManifest.products(PluginManifest.store(manifest)))
                restoredManifest <- ZIO.fromOption(parsed).orElseFail(ProductError.InvalidManifest)
                _                <- PluginProductFiles.delete(fixture.root.resolve("site"))
                _                <- PluginProductFiles.delete(fixture.root.resolve("page.txt"))
                _                <- PluginProductFiles.delete(fixture.root.resolve(".histos/products"))
                _                <- ZIO.scoped(fixture.products.restore(fixture.request, restoredManifest))
                client           <- fixture.read("site/assets/@scope/client+v1.js")
                page             <- fixture.read("site/notes/front page.html")
                empty            <- fixture.io(Files.isDirectory(fixture.root.resolve("site/empty @+dir")))
              yield Right((client, page, empty, manifest == restoredManifest))
        yield assertTrue(
          observed == Right(("client", "page", true, true)),
          ArtifactPath.from("assets/@scope/client+v1.js").map(_.value) == Right("assets/@scope/client+v1.js"),
        )
    },
  )

  private def reserved(raw: String): ZIO[Any, ProductError, TestResult] = ProductFixture.use: fixture =>
    for
      path      <- ZIO.fromEither(RepoPath.from(raw)).mapError(_ => ProductError.InvalidManifest)
      committed <- fixture.produce("old")
      _         <- fixture.write(fixture.root.resolve(".histos/products/engine-sentinel"), "engine")
      observed  <- ZIO.scoped:
        for
          lease <- committed.read
          site  <- ZIO.fromEither(lease.directory(OutputName("site")))
          request = fixture.request.copy(task =
            ProductFixture.task.copy(outputs =
              List(
                DeclaredOutput.Directory(OutputName("reserved"), path)
              )
            )
          )
          staged   <- ZIO.scoped(fixture.products.stage(request)).either.map(_.map(_ => ()))
          cleaned  <- fixture.products.clean(request).either
          sentinel <- optional(fixture, fixture.root.resolve(".histos/products/engine-sentinel"))
          page     <- optional(fixture, site.resolve("index.html"))
          asset    <- optional(fixture, site.resolve("assets/client.js"))
          stamp    <- optional(fixture, site.resolve("stamp"))
          active   <- fixture.products.clean(fixture.request).either
        yield (staged, cleaned, sentinel, page, asset, stamp, active)
    yield assertTrue(
      observed._1 == Left(ProductError.InvalidManifest),
      observed._2 == Left(ProductError.InvalidManifest),
      observed._3.contains("engine"),
      observed._4.contains("old"),
      observed._5.contains("old"),
      observed._6.contains("old"),
      observed._7 == Left(ProductError.ActiveReaders),
    )

  private def optional(fixture: ProductFixture, path: java.nio.file.Path): IO[ProductError, Option[String]] =
    fixture.io:
      if Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS) then Some(Files.readString(path)) else None

  def spec = tests
end PluginProductSafetySpec
