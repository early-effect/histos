package histos.central

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Scope, UIO, ZIO}
import zio.test.*

object PortalStoreLawSpec extends ZIOSpecDefault:
  private val gav = Gav("demo", "widget", "1.0.0")

  def spec = suite("portal store")(
    test("validated, published, and failed round-trip, and any other state is an unreadable record"):
      val kept = List(
        DeploymentId("validated") -> DeploymentState.Validated,
        DeploymentId("published") -> DeploymentState.Published,
        DeploymentId("failed")    -> DeploymentState.Failed,
      )
      val program =
        for
          root   <- ZIO.service[Path]
          dir    <- LawDir.child(root, "round")
          misses <- ZIO.foreach(kept): (id, state) =>
            stored(dir, id, record(state)) *>
              PortalStore.read(dir, id).map(found => Option.when(found != Some(record(state)))(state -> found))
          pending <- stored(dir, DeploymentId("pending"), record(DeploymentState.Pending))
          refused <- PortalStore.read(dir, pending).either
        yield
          val folder = deployments(dir, pending)
          assertTrue(
            misses.flatten == Nil,
            refused == Left(PortalStore.Error.Unreadable(folder, PortalStore.Fault.State("PENDING"))),
          )
      program
    ,
    test("promote publishes a validated deployment and leaves a published one in place"):
      val program =
        for
          root  <- ZIO.service[Path]
          dir   <- LawDir.child(root, "promote")
          id    <- stored(dir, DeploymentId("validated"), record(DeploymentState.Validated))
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
          id      <- stored(dir, DeploymentId("failed"), record(DeploymentState.Failed))
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
          marked     <- stored(dir, DeploymentId("marked"), record(DeploymentState.Published))
          held       <- stored(dir, DeploymentId("held"), record(DeploymentState.Validated))
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
      val id      = DeploymentId("absent")
      val program =
        for
          root    <- ZIO.service[Path]
          dir     <- LawDir.child(root, "missing")
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
    test("a deployment id that is not one path segment fails a create as a write and a read as a read"):
      val ids     = List(DeploymentId("."), DeploymentId(".."), DeploymentId("a/b"), DeploymentId("a\\b"))
      val program =
        for
          root <- ZIO.service[Path]
          dir  <- LawDir.child(root, "segment")
          rows <- ZIO.foreach(ids): id =>
            for
              created <- PortalStore.create(dir, id, record(DeploymentState.Validated)).either
              read    <- PortalStore.read(dir, id).either
            yield (id, created, read)
        yield
          val misses = rows.collect:
            case (id, created, read)
                if created != Left(PortalStore.Error.Write(dir)) || read != Left(PortalStore.Error.Read(dir)) =>
              id.value
          assertTrue(misses == Nil)
      program
    ,
    test("a file the store cannot read is a read error, and one it cannot write is a write error"):
      val program =
        for
          root     <- ZIO.service[Path]
          readable <- LawDir.child(root, "unreadable")
          id       <- stored(readable, DeploymentId("folded"), record(DeploymentState.Validated))
          state = deployments(readable, id).resolve("state")
          _ <- ZIO.attemptBlocking(Files.delete(state)).orDie *> ZIO.attemptBlocking(Files.createDirectory(state)).orDie
          read    <- PortalStore.read(readable, id).either
          blocked <- LawDir.child(root, "unwritable")
          _       <- written(blocked.resolve("deployments"), "a file where the folder goes")
          made    <- PortalStore.create(blocked, DeploymentId("held"), record(DeploymentState.Validated)).either
        yield assertTrue(
          read == Left(PortalStore.Error.Read(state)),
          made == Left(PortalStore.Error.Write(deployments(blocked, DeploymentId("held")).resolve("state"))),
        )
      program
    ,
    test("a published marker rebuilds a dotted group, and a bad report or gav line names its fault"):
      val dotted  = Gav("demo.group", "widget", "1.0.0")
      val program =
        for
          root <- ZIO.service[Path]
          dir  <- LawDir.child(root, "marker")
          id   <- stored(
            dir,
            DeploymentId("dotted"),
            PortalStore.Record(DeploymentState.Published, Chunk.empty, Chunk(dotted)),
          )
          known  <- PortalStore.published(dir)
          _      <- written(deployments(dir, id).resolve("report"), "nope\n")
          report <- PortalStore.read(dir, id).either
          _      <- written(deployments(dir, id).resolve("report"), "")
          _      <- written(deployments(dir, id).resolve("gavs"), "demo:widget\n")
          gavs   <- PortalStore.read(dir, id).either
        yield
          val folder = deployments(dir, id)
          assertTrue(
            known == Set(dotted),
            report == Left(
              PortalStore.Error.Unreadable(folder, PortalStore.Fault.Findings(FindingError.Unknown("nope")))
            ),
            gavs == Left(
              PortalStore.Error.Unreadable(folder, PortalStore.Fault.Coordinates(GavError.Fields("demo:widget")))
            ),
          )
      program,
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def record(state: DeploymentState): PortalStore.Record =
    PortalStore.Record(state, Chunk(Finding.EmptyPom), Chunk(gav))

  private def stored(root: Path, id: DeploymentId, record: PortalStore.Record): IO[PortalStore.Error, DeploymentId] =
    PortalStore.create(root, id, record).as(id)

  private def written(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking(Files.writeString(path, text, StandardCharsets.UTF_8)).orDie.unit

  private def deployments(root: Path, id: DeploymentId): Path =
    root.resolve("deployments").toAbsolutePath.normalize.resolve(id.value).normalize
end PortalStoreLawSpec
