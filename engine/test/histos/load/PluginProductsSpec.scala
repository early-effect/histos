package histos.load

import histos.cache.ContentStore
import histos.plugin.*
import histos.schedule.PluginManifest
import histos.suite.HistosSpec
import histos.workspace.IoFault
import java.nio.file.{Files, Path}
import zio.*
import zio.test.*

object PluginProductsSpec extends HistosSpec:
  private val tests = suite("plugin owned products")(
    test("a declared Directory is accepted with assets, stamp and empty directories") {
      ProductFixture.use: fixture =>
        for
          committed <- fixture.produce("first")
          page      <- fixture.read("site/index.html")
          asset     <- fixture.read("site/assets/client.js")
          stamp     <- fixture.read("site/stamp")
          empty     <- fixture.io(Files.isDirectory(fixture.root.resolve("site/empty")))
          roundtrip = PluginManifest.products(PluginManifest.store(committed.manifest))
        yield assertTrue(
          page == "first",
          asset == "first",
          stamp == "first",
          empty,
          roundtrip == Right(Some(committed.manifest)),
          PluginManifest.output(committed.manifest) == ProductFixture.report.copy(text = "first"),
        )
    },
    test("File destinations and read leases retain the declared filename") {
      ProductFixture.use: fixture =>
        for
          stagedName <- ZIO.scoped:
            for
              staged <- fixture.products.stage(fixture.request)
              path   <- ZIO.fromEither(staged.destinations.file(OutputName("page"))).mapError(ProductError.Report(_))
            yield Option(path.getFileName).map(_.toString)
          committed  <- fixture.produce("old")
          leasedName <- ZIO.scoped:
            for
              lease <- committed.read
              path  <- ZIO.fromEither(lease.file(OutputName("page")))
            yield Option(path.getFileName).map(_.toString)
        yield assertTrue(stagedName.contains("page.txt"), leasedName.contains("page.txt"))
    },
    test("distinct output names have physically distinct staging namespaces") {
      ProductFixture.use: fixture =>
        val task = ProductFixture.task.copy(outputs =
          List(
            DeclaredOutput.File(OutputName("page"), RepoPath("one.scala")),
            DeclaredOutput.File(OutputName("Page"), RepoPath("two.scala")),
          )
        )
        ZIO.scoped:
          for
            staged <- fixture.products.stage(fixture.request.copy(task = task))
            first  <- ZIO.fromEither(staged.destinations.file(OutputName("page"))).mapError(ProductError.Report(_))
            second <- ZIO.fromEither(staged.destinations.file(OutputName("Page"))).mapError(ProductError.Report(_))
            same   <- fixture.io(Files.isSameFile(first.getParent, second.getParent))
          yield assertTrue(!same)
    },
    test("filesystem aliases between declared final paths cannot publish the wrong output") {
      ProductFixture.use: fixture =>
        val task = ProductFixture.task.copy(outputs =
          List(
            DeclaredOutput.File(OutputName("one"), RepoPath("page.txt")),
            DeclaredOutput.File(OutputName("two"), RepoPath("PAGE.txt")),
          )
        )
        val report = PluginOutput(
          "",
          List(
            WrittenFile(OutputName("one"), RepoPath("page.txt")),
            WrittenFile(OutputName("two"), RepoPath("PAGE.txt")),
          ),
          Nil,
        )
        for
          _         <- fixture.write(fixture.root.resolve("case-probe"), "probe")
          sensitive <- fixture.io(Files.notExists(fixture.root.resolve("CASE-PROBE")))
          outcome   <- ZIO.scoped:
            for
              staged   <- fixture.products.stage(fixture.request.copy(task = task))
              first    <- ZIO.fromEither(staged.destinations.file(OutputName("one"))).mapError(ProductError.Report(_))
              second   <- ZIO.fromEither(staged.destinations.file(OutputName("two"))).mapError(ProductError.Report(_))
              _        <- fixture.write(first, "first")
              _        <- fixture.write(second, "second")
              manifest <- fixture.products.seal(staged, report)
              result   <- fixture.products.commit(staged, manifest).either
            yield result
          observed <- outcome match
            case Left(error) => ZIO.succeed(Left(error))
            case Right(_)    => ZIO.foreach(List("page.txt", "PAGE.txt"))(fixture.read).map(Right(_))
        yield assertTrue(if sensitive then observed == Right(List("first", "second"))
        else observed == Left(ProductError.Unmanaged(fixture.root.resolve("PAGE.txt"))))
    },
    test("deleted File and complete Directory replay bytes without the producer") {
      ProductFixture.use: fixture =>
        for
          committed <- fixture.produce("first")
          _         <- PluginProductFiles.delete(fixture.root.resolve("page.txt"))
          _         <- PluginProductFiles.delete(fixture.root.resolve("site"))
          _         <- PluginProductFiles.delete(fixture.root.resolve(".histos/products"))
          restored  <- ZIO.scoped(fixture.products.restore(fixture.request, committed.manifest))
          page      <- fixture.read("page.txt")
          asset     <- fixture.read("site/assets/client.js")
          stamp     <- fixture.read("site/stamp")
          count     <- fixture.count.get
        yield
          val hit = restored match
            case RestoreResult.Hit(_) => true
            case RestoreResult.Miss   => false
          assertTrue(hit, page == "first", asset == "first", stamp == "first", count == 1)
    },
    test("deleting physical generations replays from CAS even when public aliases remain") {
      ProductFixture.use: fixture =>
        for
          committed <- fixture.produce("old")
          paths     <- PluginProductFiles.paths(fixture.root.resolve(".histos/products"))
          generations = paths.filter(path => Option(path.getFileName).exists(_.toString == "generations"))
          _     <- ZIO.foreachDiscard(generations)(PluginProductFiles.delete)
          _     <- ZIO.scoped(fixture.products.restore(fixture.request, committed.manifest))
          page  <- fixture.read("site/index.html")
          count <- fixture.count.get
        yield assertTrue(generations.nonEmpty, page == "old", count == 1)
    },
    test("unchanged replay reuses the current generation") {
      ProductFixture.use: fixture =>
        for
          committed <- fixture.produce("old")
          before    <- PluginProductFiles.paths(fixture.root.resolve(".histos/products"))
          _         <- ZIO.scoped(fixture.products.restore(fixture.request, committed.manifest))
          after     <- PluginProductFiles.paths(fixture.root.resolve(".histos/products"))
          count     <- fixture.count.get
        yield assertTrue(before == after, count == 1)
    },
    test("fresh generation removes stale owned entries and preserves unowned siblings") {
      ProductFixture.use: fixture =>
        for
          _        <- fixture.produce("old")
          _        <- fixture.write(fixture.root.resolve("site/stale.html"), "stale")
          _        <- fixture.write(fixture.root.resolve("adjacent.txt"), "unowned")
          _        <- fixture.produce("new")
          stale    <- fixture.io(Files.exists(fixture.root.resolve("site/stale.html")))
          adjacent <- fixture.read("adjacent.txt")
          page     <- fixture.read("page.txt")
          count    <- fixture.count.get
        yield assertTrue(!stale, adjacent == "unowned", page == "new", count == 2)
    },
    test("missing CAS is a miss, corrupt CAS is a typed failure and neither changes the prior result") {
      ProductFixture.use: fixture =>
        for
          committed <- fixture.produce("old")
          hashes = committed.manifest.products.flatMap(_.entries.collect {
            case ProductEntry.RootFile(hash) => hash
            case ProductEntry.File(_, hash)  => hash
          })
          hash   <- ZIO.fromOption(hashes.headOption).orElseFail(ProductError.InvalidManifest)
          digest <- ZIO.fromOption(histos.schedule.Digest.from(hash.value)).orElseFail(ProductError.InvalidManifest)
          store  <- ZIO.service[ContentStore]
          _      <- store.remove(digest).mapError(ProductError.Cache(_))
          miss   <- ZIO.scoped(fixture.products.restore(fixture.request, committed.manifest))
          now    <- Clock.instant
          _      <- store
            .put(digest, Chunk.fromArray("corrupt".getBytes(java.nio.charset.StandardCharsets.UTF_8)), now)
            .mapError(ProductError.Cache(_))
          corrupt <- ZIO.scoped(fixture.products.restore(fixture.request, committed.manifest)).either
          page    <- fixture.read("site/index.html")
        yield assertTrue(
          miss == RestoreResult.Miss,
          corrupt == Left(ProductError.Cache(histos.schedule.CacheError.Corrupt(digest))),
          page == "old",
        )
    },
    test("embedded links and escaped manifest entries fail with typed errors") {
      ProductFixture.use: fixture =>
        for
          committed <- fixture.produce("old")
          result    <- ZIO.scoped:
            for
              staged <- fixture.products.stage(fixture.request)
              _      <- fixture.fill(staged, "new")
              site <- ZIO.fromEither(staged.destinations.directory(OutputName("site"))).mapError(ProductError.Report(_))
              _    <- fixture.io(Files.createSymbolicLink(site.resolve("escape"), fixture.root))
              result <- fixture.products.seal(staged, ProductFixture.report).either
            yield result
          escaped = PluginManifest.store(committed.manifest).replace("assets/client.js", "../escape")
          page <- fixture.read("site/index.html")
        yield assertTrue(
          result.swap.exists {
            case ProductError.Link(_) => true
            case _                    => false
          },
          PluginManifest.products(escaped) == Left(ProductError.InvalidEntry("../escape")),
          page == "old",
        )
    },
    test("invalid relative entry names fail sealing without changing the prior output") {
      ProductFixture.use: fixture =>
        for
          _      <- fixture.produce("old")
          result <- ZIO.scoped:
            for
              staged <- fixture.products.stage(fixture.request)
              _      <- fixture.fill(staged, "new")
              site <- ZIO.fromEither(staged.destinations.directory(OutputName("site"))).mapError(ProductError.Report(_))
              _    <- fixture.write(site.resolve("bad\\name.html"), "invalid")
              result <- fixture.products.seal(staged, ProductFixture.report).either
            yield result
          after <- fixture.read("site/index.html")
        yield assertTrue(
          result.swap.exists {
            case ProductError.InvalidEntry(_) => true
            case _                            => false
          },
          after == "old",
        )
    },
    test("reported names, paths, kinds, omissions and duplicates are validated") {
      val outputs   = ProductFixture.task.outputs
      val report    = ProductFixture.report
      val wrongKind = PluginOutput("", report.files :+ WrittenFile(OutputName("site"), RepoPath("site")), Nil)
      val wrongName =
        PluginOutput("", List(WrittenFile(OutputName("unknown"), RepoPath("page.txt"))), report.directories)
      val wrongPath =
        PluginOutput("", List(WrittenFile(OutputName("page"), RepoPath("different.txt"))), report.directories)
      val duplicate = PluginOutput("", report.files ++ report.files, report.directories)
      assertTrue(
        PluginProducts.validateReport(outputs, report) == Right(()),
        PluginProducts.validateReport(outputs, wrongKind) == Left(
          PluginRunError.OutputKindMismatch(OutputName("site"), OutputKind.Directory, OutputKind.File)
        ),
        PluginProducts.validateReport(outputs, wrongName) == Left(PluginRunError.ExtraOutput("page.txt")),
        PluginProducts.validateReport(outputs, wrongPath) == Left(PluginRunError.ExtraOutput("different.txt")),
        PluginProducts.validateReport(outputs, duplicate) == Left(PluginRunError.ExtraOutput("page.txt")),
        PluginProducts.validateReport(outputs, PluginOutput("", Nil, Nil)).isLeft,
      )
    },
    test("a lease pins one complete generation and prevents clean until its scope closes") {
      ProductFixture.use: fixture =>
        for
          committed <- fixture.produce("old")
          observed  <- ZIO.scoped:
            for
              lease   <- committed.read
              oldSite <- ZIO.fromEither(lease.directory(OutputName("site")))
              _       <- fixture.produce("new")
              oldPage <- fixture.io(Files.readString(oldSite.resolve("index.html")))
              current <- fixture.read("site/index.html")
              clean   <- fixture.products.clean(fixture.request).either
            yield (oldPage, current, clean)
          _    <- fixture.products.clean(fixture.request)
          gone <- fixture.io(Files.notExists(fixture.root.resolve("site")))
        yield assertTrue(observed == ("old", "new", Left(ProductError.ActiveReaders)), gone)
    },
  )
  def spec = tests

private[load] final case class ProductFixture(root: Path, products: PluginProducts, count: Ref[Int]):
  val request: ProductRequest = ProductRequest(root, ProductFixture.task, InvocationId.derive("products-test"))
  def io[A](body: => A): IO[ProductError, A]                  = PluginProductFiles.io(root, IoFault.Write)(body)
  def write(path: Path, text: String): IO[ProductError, Unit] = io:
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)
    ()
  def read(relative: String): IO[ProductError, String]                  = io(Files.readString(root.resolve(relative)))
  def fill(staged: StagedOutputs, text: String): IO[ProductError, Unit] =
    for
      site <- ZIO.fromEither(staged.destinations.directory(OutputName("site"))).mapError(ProductError.Report(_))
      page <- ZIO.fromEither(staged.destinations.file(OutputName("page"))).mapError(ProductError.Report(_))
      _    <- write(page, text)
      _    <- write(site.resolve("index.html"), text)
      _    <- write(site.resolve("assets/client.js"), text)
      _    <- write(site.resolve("stamp"), text)
      _    <- io(Files.createDirectories(site.resolve("empty")))
    yield ()
  def produce(text: String): ZIO[ContentStore, ProductError, CommittedOutputs] = ZIO.scoped:
    for
      _         <- count.update(_ + 1)
      staged    <- products.stage(request)
      _         <- fill(staged, text)
      manifest  <- products.seal(staged, ProductFixture.report.copy(text = text))
      committed <- products.commit(staged, manifest)
    yield committed

private[load] object ProductFixture:
  val task: DeclaredTask = DeclaredTask(
    TaskName("site"),
    Seal.Hermetic,
    Set.empty,
    Nil,
    List(
      DeclaredOutput.File(OutputName("page"), RepoPath("page.txt")),
      DeclaredOutput.Directory(OutputName("site"), RepoPath("site")),
    ),
    Nil,
  )
  val report: PluginOutput = PluginOutput(
    "site",
    List(WrittenFile(OutputName("page"), RepoPath("page.txt"))),
    List(WrittenDirectory(OutputName("site"), RepoPath("site"))),
  )
  def use[E, A](body: ProductFixture => ZIO[ContentStore & Scope, E, A]): IO[ProductError | E, A] = ZIO.scoped:
    for
      root <- ZIO.acquireRelease(
        PluginProductFiles.io(Path.of("/tmp"), IoFault.Temp)(Files.createTempDirectory("histos-products"))
      )(PluginProductFiles.delete(_).ignore)
      products <- PluginProducts.make()
      count    <- Ref.make(0)
      value    <- body(ProductFixture(root, products, count))
        .provideSomeLayer[Scope](ZLayer.succeed(ContentStore.in(root.resolve("cas"))))
    yield value
