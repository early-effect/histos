package histos.central

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object PortalStoreLawSpec extends ZIOSpecDefault:
  private val gav = Gav("demo", "widget", "1.0.0")

  def spec = suite("portal store")(
    test("validated, published, and failed round-trip, and any other state does not"):
      val program =
        for
          root   <- ZIO.service[Path]
          dir    <- LawDir.child(root, "round")
          misses <- ZIO.foreach(List(DeploymentState.Validated, DeploymentState.Published, DeploymentState.Failed)):
            state =>
              stored(dir, state.toString.toLowerCase, record(state)).flatMap: id =>
                PortalStore.read(dir, id).map(found => Option.when(found != Some(record(state)))(state -> found))
          pending <- stored(dir, "pending", record(DeploymentState.Pending))
          refused <- PortalStore.read(dir, pending).either
        yield
          val folder = deployments(dir, pending)
          assertTrue(misses.flatten == Nil, refused == Left(PortalStore.Error.Io(folder)))
      program
    ,
    test("promote publishes a validated deployment and leaves a published one in place"):
      val program =
        for
          root  <- ZIO.service[Path]
          dir   <- LawDir.child(root, "promote")
          id    <- stored(dir, "validated", record(DeploymentState.Validated))
          _     <- PortalStore.promote(dir, id)
          after <- PortalStore.read(dir, id)
          known <- PortalStore.published(dir)
          _     <- PortalStore.promote(dir, id)
          again <- PortalStore.published(dir)
        yield assertTrue(
          after.map(_.state) == Some(DeploymentState.Published),
          known == Set(gav),
          again == Set(gav),
          Files.isRegularFile(dir.resolve("published").resolve("demo").resolve("widget").resolve("1.0.0")),
        )
      program
    ,
    test("a failed deployment cannot be promoted"):
      val program =
        for
          root    <- ZIO.service[Path]
          dir     <- LawDir.child(root, "failed")
          id      <- stored(dir, "failed", record(DeploymentState.Failed))
          refused <- PortalStore.promote(dir, id).either
          after   <- PortalStore.read(dir, id)
        yield assertTrue(
          refused == Left(PortalStore.Error.NotPromotable(id)),
          after.map(_.state) == Some(DeploymentState.Failed),
        )
      program
    ,
    test("dropping a validated deployment does not unmark a gav a published deployment owns"):
      val program =
        for
          root       <- ZIO.service[Path]
          dir        <- LawDir.child(root, "drop")
          marked     <- stored(dir, "marked", record(DeploymentState.Published))
          held       <- stored(dir, "held", record(DeploymentState.Validated))
          before     <- PortalStore.published(dir)
          _          <- PortalStore.drop(dir, held)
          during     <- PortalStore.published(dir)
          heldRead   <- PortalStore.read(dir, held)
          _          <- PortalStore.drop(dir, marked)
          after      <- PortalStore.published(dir)
          markedRead <- PortalStore.read(dir, marked)
        yield assertTrue(
          before == Set(gav),
          during == Set(gav),
          heldRead == None,
          after == Set.empty,
          markedRead == None,
        )
      program
    ,
    test("a missing deployment is missing on promote and absent on read"):
      val program =
        for
          root    <- ZIO.service[Path]
          dir     <- LawDir.child(root, "missing")
          id      <- ZIO.fromEither(DeploymentId.from("absent"))
          read    <- PortalStore.read(dir, id)
          refused <- PortalStore.promote(dir, id).either
          known   <- PortalStore.published(dir)
        yield assertTrue(
          read == None,
          refused == Left(PortalStore.Error.Missing(id)),
          known == Set.empty,
        )
      program
    ,
    test("a deployment id that is not one path segment is an io error"):
      val names   = List(".", "..", "a/b", "a\\b")
      val program =
        for
          root <- ZIO.service[Path]
          dir  <- LawDir.child(root, "segment")
          rows <- ZIO.foreach(names): name =>
            ZIO
              .fromEither(DeploymentId.from(name))
              .flatMap: id =>
                PortalStore.create(dir, id, record(DeploymentState.Validated)).either.map(id -> _)
        yield
          val misses = rows.collect:
            case (id, found) if found != Left(PortalStore.Error.Io(dir)) => id.value
          assertTrue(misses == Nil)
      program
    ,
    test("a published marker rebuilds a dotted group, and a bad report line does not decode"):
      val dotted  = Gav("demo.group", "widget", "1.0.0")
      val program =
        for
          root  <- ZIO.service[Path]
          dir   <- LawDir.child(root, "marker")
          id    <- stored(dir, "dotted", PortalStore.Record(DeploymentState.Published, Chunk.empty, Chunk(dotted)))
          known <- PortalStore.published(dir)
          _     <- ZIO
            .attemptBlocking(
              Files.writeString(deployments(dir, id).resolve("report"), "nope\n", StandardCharsets.UTF_8)
            )
            .orDie
          refused <- PortalStore.read(dir, id).either
        yield assertTrue(
          known == Set(dotted),
          refused == Left(PortalStore.Error.Io(deployments(dir, id))),
        )
      program,
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def record(state: DeploymentState): PortalStore.Record =
    PortalStore.Record(state, Chunk(Finding.EmptyPom), Chunk(gav))

  private def stored(root: Path, label: String, record: PortalStore.Record) =
    ZIO
      .fromEither(DeploymentId.from(label))
      .flatMap: id =>
        PortalStore.create(root, id, record).as(id)

  private def deployments(root: Path, id: DeploymentId): Path =
    root.resolve("deployments").toAbsolutePath.normalize.resolve(id.value).normalize
end PortalStoreLawSpec
