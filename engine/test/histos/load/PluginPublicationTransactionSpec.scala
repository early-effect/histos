package histos.load

import histos.suite.HistosSpec
import java.nio.file.Files
import zio.*
import zio.test.*

object PluginPublicationTransactionSpec extends HistosSpec:
  private val tests = suite("plugin publication transaction")(
    test("failure after staged page, asset and stamp writes leaves the complete prior generation") {
      ProductFixture.use: fixture =>
        for
          _       <- fixture.produce("old")
          wrote   <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          fiber   <- ZIO
            .scoped(
              for
                staged <- fixture.products.stage(fixture.request)
                _      <- fixture.fill(staged, "partial")
                _      <- wrote.succeed(())
                _      <- release.await
                _      <- ZIO.fail(ProductError.Changed)
              yield ()
            )
            .forkScoped
          _      <- wrote.await
          during <- complete(fixture)
          _      <- release.succeed(())
          result <- fiber.await
          after  <- complete(fixture)
          stages <- staging(fixture)
        yield assertTrue(
          result == Exit.fail(ProductError.Changed),
          during == List.fill(4)("old"),
          after == during,
          stages.isEmpty,
        )
    },
    test("interruption after staged writes releases staging and preserves the prior generation") {
      ProductFixture.use: fixture =>
        for
          _     <- fixture.produce("old")
          wrote <- Promise.make[Nothing, Unit]
          hold  <- Promise.make[Nothing, Unit]
          fiber <- ZIO
            .scoped(
              for
                staged <- fixture.products.stage(fixture.request)
                _      <- fixture.fill(staged, "partial")
                _      <- wrote.succeed(())
                _      <- hold.await
              yield ()
            )
            .forkScoped
          _      <- wrote.await
          result <- fiber.interrupt
          after  <- complete(fixture)
          stages <- staging(fixture)
          _      <- fixture.produce("next")
          next   <- complete(fixture)
        yield assertTrue(
          result.isInterrupted,
          after == List.fill(4)("old"),
          stages.isEmpty,
          next == List.fill(4)("next"),
        )
    },
    test("promotion failure against a nonempty managed directory preserves all previous outputs") {
      ProductFixture.use: fixture =>
        for
          _           <- fixture.produce("old")
          beforePaths <- PluginProductFiles.paths(fixture.root.resolve(".histos/products"))
          failing     <- PluginProducts.make(ProductPublication(ZIO.fail(ProductError.Changed)))
          failed      <- fixture.copy(products = failing).produce("new").either
          after       <- complete(fixture)
          afterPaths  <- PluginProductFiles.paths(fixture.root.resolve(".histos/products"))
        yield assertTrue(
          failed == Left(ProductError.Changed),
          after == List.fill(4)("old"),
          beforePaths.toSet == afterPaths.toSet,
        )
    },
    test("interruption after promotion preparation rolls back the unpublished generation") {
      ProductFixture.use: fixture =>
        for
          _          <- fixture.produce("old")
          before     <- PluginProductFiles.paths(fixture.root.resolve(".histos/products"))
          reached    <- Promise.make[Nothing, Unit]
          hold       <- Promise.make[Nothing, Unit]
          publishing <- PluginProducts.make(ProductPublication(reached.succeed(()) *> hold.await))
          fiber      <- fixture.copy(products = publishing).produce("new").forkScoped
          _          <- reached.await
          result     <- fiber.interrupt
          after      <- complete(fixture)
          paths      <- PluginProductFiles.paths(fixture.root.resolve(".histos/products"))
        yield assertTrue(result.isInterrupted, after == List.fill(4)("old"), before.toSet == paths.toSet)
    },
    test("leased readers keep one complete tree during promotion and fresh public reads follow the new pointer") {
      ProductFixture.use: fixture =>
        for
          old        <- fixture.produce("old")
          reached    <- Promise.make[Nothing, Unit]
          release    <- Promise.make[Nothing, Unit]
          publishing <- PluginProducts.make(ProductPublication(reached.succeed(()) *> release.await))
          result     <- ZIO.scoped:
            for
              pinned     <- old.read
              site       <- ZIO.fromEither(pinned.directory(histos.plugin.OutputName("site")))
              fiber      <- fixture.copy(products = publishing).produce("new").forkScoped
              _          <- reached.await
              before     <- complete(fixture)
              _          <- release.succeed(())
              concurrent <- ZIO.foreachPar(1 to 100)(_ => fixture.read("site/index.html"))
              _          <- fiber.join
              after      <- complete(fixture)
              retained   <- ZIO.foreach(List("index.html", "assets/client.js", "stamp"))(name =>
                fixture.io(Files.readString(site.resolve(name)))
              )
            yield (before, concurrent, after, retained)
        yield assertTrue(
          result._1 == List.fill(4)("old"),
          result._2.forall(text => text == "old" || text == "new"),
          result._3 == List.fill(4)("new"),
          result._4 == List.fill(3)("old"),
        )
    },
    test("unmanaged nonempty directories fail before promotion rather than disappearing during adoption") {
      ProductFixture.use: fixture =>
        for
          _      <- fixture.write(fixture.root.resolve("site/index.html"), "legacy")
          _      <- fixture.write(fixture.root.resolve("site/assets/client.js"), "legacy")
          failed <- fixture.produce("new").either
          page   <- fixture.read("site/index.html")
          asset  <- fixture.read("site/assets/client.js")
        yield assertTrue(
          failed == Left(ProductError.Unmanaged(fixture.root.resolve("site"))),
          page == "legacy",
          asset == "legacy",
        )
    },
    test("output ancestors that are links are rejected without writing through them") {
      ProductFixture.use: fixture =>
        for
          _ <- fixture.produce("old")
          _ <- fixture.io(Files.createSymbolicLink(fixture.root.resolve("linked"), fixture.root.resolve("site")))
          task = ProductFixture.task.copy(outputs =
            List(
              histos.plugin.DeclaredOutput
                .File(histos.plugin.OutputName("page"), histos.plugin.RepoPath("linked/page.txt"))
            )
          )
          request = fixture.request.copy(task = task)
          failed <- ZIO.scoped:
            for
              staged <- fixture.products.stage(request)
              path   <- ZIO
                .fromEither(staged.destinations.file(histos.plugin.OutputName("page")))
                .mapError(ProductError.Report(_))
              _ <- fixture.write(path, "escape")
              report = histos.plugin.PluginOutput(
                "",
                List(
                  histos.plugin.WrittenFile(histos.plugin.OutputName("page"), histos.plugin.RepoPath("linked/page.txt"))
                ),
                Nil,
              )
              manifest <- fixture.products.seal(staged, report)
              result   <- fixture.products.commit(staged, manifest).either
            yield result
          after <- complete(fixture)
        yield assertTrue(
          failed == Left(ProductError.Link(fixture.root.resolve("linked"))),
          after == List.fill(4)("old"),
        )
    },
  )

  private def complete(fixture: ProductFixture): IO[ProductError, List[String]] =
    ZIO.foreach(List("page.txt", "site/index.html", "site/assets/client.js", "site/stamp"))(fixture.read)

  private def staging(fixture: ProductFixture): IO[ProductError, List[java.nio.file.Path]] =
    PluginProductFiles
      .paths(fixture.root.resolve(".histos/products"))
      .map(_.filter(path => Option(path.getFileName).exists(_.toString.startsWith(".stage-"))))

  def spec = tests
end PluginPublicationTransactionSpec
