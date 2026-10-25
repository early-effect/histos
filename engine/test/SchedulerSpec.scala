package histos.schedule

import histos.jdk.Sha256
import histos.plugin.{Need, RepoPath, Seal}
import histos.task.TaskError

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*
import java.security.MessageDigest

import zio.{Chunk, IO, Promise, Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object SchedulerSpec extends ZIOSpecDefault:
  private def aid(raw: String): ActionId = ActionId.trusted(raw)

  private val a = aid("a")
  private val b = aid("b")
  private val c = aid("c")
  private val d = aid("d")

  private def action(id: ActionId, deps: ActionId*): Action =
    Action(
      id,
      List(Input(InputName.trusted("src"), InputValue.Text(id.value))),
      deps.toList,
    )

  private val localOk: Action => IO[TaskError, String] =
    action => ZIO.succeed(action.id.value)

  private def execute(
      root: ActionId,
      actions: List[Action],
      local: Action => IO[TaskError, String] = localOk,
  ) =
    ZIO
      .serviceWithZIO[Scheduler](
        _.execute(root, actions, (action, _) => local(action))
      )
      .provide(
        Scheduler.live,
        ZLayer.succeed(ActionCache.miss),
        RemoteCache.off,
      )

  private val schedule = suite("scheduler")(
    test("a miss runs the action after its dependencies, and only those"):
      for
        seen <- Ref.make(List.empty[String])
        out  <- execute(
          a,
          List(action(a, b), action(b, c), action(c), action(d)),
          action => seen.update(action.id.value :: _) *> ZIO.succeed(action.id.value),
        )
        ran <- seen.get
      yield assertTrue(out == "a" && ran.reverse == List("c", "b", "a"))
    ,
    test("a shared dependency runs once"):
      for
        count <- Ref.make(Map.empty[String, Int])
        _     <- execute(
          a,
          List(action(a, b, c), action(b, d), action(c, d), action(d)),
          action =>
            count.update(m => m + (action.id.value -> (m.getOrElse(action.id.value, 0) + 1))) *> ZIO.succeed("ok"),
        )
        ran <- count.get
      yield assertTrue(ran.getOrElse("d", 0) == 1 && ran.getOrElse("a", 0) == 1)
    ,
    test("independent dependencies overlap")(
      (for
        startedB <- Promise.make[Nothing, Unit]
        startedC <- Promise.make[Nothing, Unit]
        out      <-
          ZIO.serviceWithZIO[Scheduler](
            _.execute(
              a,
              List(action(a, b, c), action(b), action(c)),
              (action, _) =>
                if action.id == b then startedB.succeed(()) *> startedC.await.as("b")
                else if action.id == c then startedC.succeed(()) *> startedB.await.as("c")
                else ZIO.succeed("a"),
            )
          )
      yield assertTrue(out == "a")).provide(
        Scheduler.live,
        ZLayer.succeed(ActionCache.miss),
        RemoteCache.off,
      )
    ) @@ TestAspect.timeout(2.seconds),
    test("a cycle runs nothing"):
      for
        ran <- Ref.make(false)
        out <- execute(
          a,
          List(action(a, b), action(b, a)),
          _ => ran.set(true) *> ZIO.succeed("x"),
        ).either
        did <- ran.get
      yield assertTrue:
        out match
          case Left(ScheduleError.Cycle(path)) =>
            !did && path.map(_.value).contains("a")
          case _ => false
    ,
    test("a failure stops only its dependents"):
      for
        seen <- Ref.make(Set.empty[String])
        out  <- execute(
          a,
          List(action(a, b, c), action(b), action(c)),
          node =>
            if node.id == b then ZIO.fail(TaskError.EmptyId)
            else seen.update(_ + node.id.value).as(node.id.value),
        ).either
        ran <- seen.get
      yield assertTrue(
        out == Left(ScheduleError.Failed(b, TaskError.EmptyId)),
        ran == Set("c"),
      )
    ,
    test("duplicate actions are rejected"):
      execute(a, List(action(a), action(a))).either.map:
        case Left(ScheduleError.DuplicateAction(id)) => assertTrue(id == a)
        case _                                       => assertTrue(false)
    ,
    test("a missing dependency is rejected"):
      execute(a, List(action(a, b))).either.map:
        case Left(ScheduleError.MissingDependency(parent, missing)) =>
          assertTrue(parent == a, missing == b)
        case _ => assertTrue(false)
    ,
    test("duplicate inputs are rejected"):
      val duplicate = action(a).copy(
        inputs = List(
          Input(InputName.trusted("same"), InputValue.Text("one")),
          Input(InputName.trusted("same"), InputValue.Text("two")),
        )
      )
      execute(a, List(duplicate)).either.map:
        case Left(ScheduleError.DuplicateInput(id, name)) =>
          assertTrue(id == a, name.value == "same")
        case _ => assertTrue(false)
    ,
    test("a dependent receives its upstream output"):
      ZIO
        .serviceWithZIO[Scheduler]:
          _.execute(
            a,
            List(action(a, b), action(b)),
            (node, upstream) =>
              if node.id == b then ZIO.succeed("from-b")
              else ZIO.succeed(upstream),
          )
        .provide(
          Scheduler.live,
          ZLayer.succeed(ActionCache.miss),
          RemoteCache.off,
        )
        .map(out => assertTrue(out == "from-b"))
    ,
    test("interrupting the run interrupts the local action"):
      for
        started <- Promise.make[Nothing, Unit]
        fiber   <- execute(
          a,
          List(action(a)),
          _ => started.succeed(()) *> ZIO.never,
        ).fork
        _    <- started.await
        _    <- fiber.interrupt
        exit <- fiber.await
      yield assertTrue(exit.isInterrupted)
    ,
    test("a cache hit does not run locally"):
      val hit: ActionCache = new ActionCache:
        def get(digest: Digest): UIO[Option[String]]      = ZIO.some("cached")
        def put(digest: Digest, value: String): UIO[Unit] = ZIO.unit
      (for
        called <- Ref.make(false)
        out    <- ZIO
          .serviceWithZIO[Scheduler](
            _.execute(
              a,
              List(action(a)),
              (_, _) => called.set(true) *> ZIO.succeed("local"),
            )
          )
          .either
        did <- called.get
      yield assertTrue(out == Right("cached") && !did))
        .provide(Scheduler.live, ZLayer.succeed(hit), RemoteCache.off)
    ,
    test("changing an input changes the digest"):
      val first  = action(a)
      val second = first.copy(inputs = List(Input(InputName.trusted("src"), InputValue.Text("other"))))
      assertTrue(Digest.of(first, Map.empty) != Digest.of(second, Map.empty))
    ,
    test(
      "a core compile digest has no policy line, and a plugin seal changes it"
    ):
      val id     = aid("compile")
      val action = Action(
        id,
        List(Input(InputName.trusted("compile"), InputValue.Text("cell"))),
        Nil,
      )
      val body     = "compile\ncompile=text cell\n"
      val expected = sha256(body)
      val plugin   = action.copy(
        policy = Policy.Plugin(Seal.Hermetic, Set(Need.Network, Need.Docker))
      )
      assertTrue(
        Digest.of(action, Map.empty).value == expected,
        Digest
          .of(action.copy(policy = Policy.Core), Map.empty)
          .value == expected,
        Digest.of(plugin, Map.empty).value != expected,
      )
    ,
    test("an open plugin is not stored"):
      val open = pluginAction("dev.example/echo", Seal.Open, None, Nil)
      for
        (values, cache) <- recording
        calls           <- Ref.make(0)
        out             <- executeCached(
          cache,
          RemoteCache.unconfigured,
          open.id,
          List(open),
          _ => calls.update(_ + 1).as("hello"),
        )
        again <- executeCached(
          cache,
          RemoteCache.unconfigured,
          open.id,
          List(open),
          _ => calls.update(_ + 1).as("hello"),
        )
        stored <- values.get
        ran    <- calls.get
      yield assertTrue(
        out == "hello" && again == "hello" && stored.isEmpty && ran == 2
      )
    ,
    test("a hermetic plugin with no outputs stores its text"):
      val task = pluginAction("dev.example/echo", Seal.Hermetic, None, Nil)
      for
        (values, cache) <- recording
        calls           <- Ref.make(0)
        local = (_: Action) => calls.update(_ + 1).as("hello")
        out <- executeCached(
          cache,
          RemoteCache.unconfigured,
          task.id,
          List(task),
          local,
        )
        again <- executeCached(
          cache,
          RemoteCache.unconfigured,
          task.id,
          List(task),
          local,
        )
        stored <- values.get
        ran    <- calls.get
      yield assertTrue(
        out == "hello" && again == "hello" && stored.values.toList == List(
          "hello"
        ) && ran == 1
      )
    ,
    test(
      "a hermetic file manifest hits while the file matches and misses after it is deleted"
    ):
      ZIO.scoped:
        for
          dir <- directory
          _   <- ZIO
            .attempt(Files.writeString(dir.resolve("out.txt"), "page"))
            .orDie
          manifest <- manifestOf(dir)
          task = pluginAction(
            "dev.example/page",
            Seal.Hermetic,
            Some(dir),
            List(RepoPath("out.txt")),
          )
          digest = Digest.of(task, Map.empty)
          (values, cache) <- recording
          _               <- values.set(Map(digest.value -> manifest))
          calls           <- Ref.make(0)
          local = (_: Action) => calls.update(_ + 1).as(manifest)
          hit <- executeCached(
            cache,
            RemoteCache.unconfigured,
            task.id,
            List(task),
            local,
          )
          afterHit <- calls.get
          _        <- ZIO.attempt(Files.delete(dir.resolve("out.txt"))).orDie
          _        <- executeCached(
            cache,
            RemoteCache.unconfigured,
            task.id,
            List(task),
            local,
          )
          afterMiss <- calls.get
          stored    <- values.get
        yield assertTrue(
          hit == manifest,
          afterHit == 0,
          afterMiss == 1,
          stored.get(digest.value).contains(manifest),
          manifest.startsWith(PluginManifest.Header),
        )
    ,
    test("a remote read whose declared file is missing is a miss"):
      ZIO.scoped:
        for
          dir <- directory
          _   <- ZIO
            .attempt(Files.writeString(dir.resolve("out.txt"), "page"))
            .orDie
          manifest <- manifestOf(dir)
          task = pluginAction(
            "dev.example/page",
            Seal.Hermetic,
            Some(dir),
            List(RepoPath("out.txt")),
          )
          digest = Digest.of(task, Map.empty)
          remote <- TestRemoteCache.memory(RemoteMode.Read)
          _      <- remote.plant(
            digest,
            Chunk.fromArray(manifest.getBytes(StandardCharsets.UTF_8)),
          )
          calls <- Ref.make(0)
          local = (_: Action) => calls.update(_ + 1).as("ran")
          hit <- executeCached(
            ActionCache.miss,
            remote,
            task.id,
            List(task),
            local,
          )
          afterHit <- calls.get
          _        <- ZIO.attempt(Files.delete(dir.resolve("out.txt"))).orDie
          missed   <- executeCached(
            ActionCache.miss,
            remote,
            task.id,
            List(task),
            local,
          )
          afterMiss <- calls.get
        yield assertTrue(
          hit == manifest && afterHit == 0 && missed == "ran" && afterMiss == 1
        ),
  )

  private def pluginAction(
      id: String,
      seal: Seal,
      root: Option[Path],
      outputs: List[RepoPath],
  ): Action =
    Action(
      aid(id),
      Nil,
      Nil,
      Policy.Plugin(seal, Set.empty, root, outputs),
    )

  private def recording: UIO[(Ref[Map[String, String]], ActionCache)] =
    Ref
      .make(Map.empty[String, String])
      .map: values =>
        val cache: ActionCache = new ActionCache:
          def get(digest: Digest): UIO[Option[String]] =
            values.get.map(_.get(digest.value))
          override def put(digest: Digest, value: String): UIO[Unit] =
            values.update(_ + (digest.value -> value))
        (values, cache)

  private def executeCached(
      cache: ActionCache,
      remote: RemoteCache,
      root: ActionId,
      actions: List[Action],
      local: Action => IO[TaskError, String],
  ) =
    ZIO
      .serviceWithZIO[Scheduler](
        _.execute(root, actions, (action, _) => local(action))
      )
      .provide(Scheduler.live, ZLayer.succeed(cache), ZLayer.succeed(remote))

  private def directory: ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-manifest")).orDie
    )(delete)

  private def manifestOf(dir: Path): UIO[String] =
    ZIO.attempt {
      val bytes = Files.readAllBytes(dir.resolve("out.txt"))
      PluginManifest.store(
        "page",
        List(RepoPath("out.txt") -> Sha256.of(Chunk.fromArray(bytes))),
      )
    }.orDie

  private def delete(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def sha256(text: String): String =
    val bytes = MessageDigest
      .getInstance("SHA-256")
      .digest(text.getBytes(StandardCharsets.UTF_8))
    bytes.map(b => f"${b & 0xff}%02x").mkString

  def spec = schedule @@ TestAspect.timeout(10.seconds)
end SchedulerSpec
