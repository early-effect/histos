package histos.schedule

import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{Need, RepoPath, Seal}
import histos.step.{Need as StepNeed, Step, StepId, StepInfo}
import histos.version.PullRequestError
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.Promote

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import slayer.{any, stub, stubbed}

import zio.{Chunk, Promise, Ref, Schedule, Scope, ZIO, ZLayer}
import zio.test.*

import Histos.given

object ScheduleLawSpec extends ZIOSpecDefault:
  private val runners = List(
    Runner.Compile,
    Runner.Inspect,
    Runner.Versions,
    Runner.Test,
    Runner.Publish,
    Runner.Closure,
    Runner.Plugin(Seal.Hermetic),
    Runner.Plugin(Seal.Open),
  )

  private val scheduled =
    (
      stubbed[ActionCache] ++
        stubbed[RemoteCache] ++
        (ZLayer.succeed(Parallelism(4, 16, 8)) >>> Histos.live)
    ) >+> ZLayer.fromFunction(Scheduler.apply)

  def spec = suite("schedule laws")(
    suite("who may leave the machine")(
      test("any declared needs keep every runner on this machine, and the hold names them all"):
        check(Gen.fromIterable(runners), Gen.setOf1(Gen.fromIterable(Need.values.toList))): (runner, needs) =>
          assertTrue(Sendable.decide(runner, needs) == Verdict.Stay(Hold.Needs(needs)))
      ,
      test("an empty need set sends only compile and a hermetic plugin"):
        assertTrue(
          runners.forall { runner =>
            val expected = runner match
              case Runner.Compile               => Verdict.Send
              case Runner.Plugin(Seal.Hermetic) => Verdict.Send
              case Runner.Plugin(Seal.Open)     => Verdict.Stay(Hold.Plugin)
              case Runner.Closure               => Verdict.Stay(Hold.Closure)
              case Runner.Test                  => Verdict.Stay(Hold.Test)
              case Runner.Publish               => Verdict.Stay(Hold.Publish)
              case Runner.Inspect               => Verdict.Stay(Hold.Inspect)
              case Runner.Versions              => Verdict.Stay(Hold.Versions)
            Sendable.decide(runner, Set.empty) == expected
          }
        )
      ,
      test("a core action is sendable and a plugin delegates to its seal and needs"):
        assertTrue(
          Sendable.ofAction(core("pack")) == Verdict.Send,
          Sendable.ofAction(plugin(Seal.Hermetic)) == Verdict.Send,
          Sendable.ofAction(plugin(Seal.Open)) == Verdict.Stay(Hold.Plugin),
          Sendable.ofAction(plugin(Seal.Hermetic, Set(Need.Network))) ==
            Verdict.Stay(Hold.Needs(Set(Need.Network))),
        ),
    ),
    suite("the action digest")(
      test("input order and dependency order do not change the key, and a value does"):
        val b       = ActionId.trusted("b")
        val c       = ActionId.trusted("c")
        val bHash   = Digest.trusted("ab" * 32)
        val cHash   = Digest.trusted("cd" * 32)
        val forward = core("a", List(text("z", "1"), text("m", "2")), List(b, c))
        val flipped = core("a", List(text("m", "2"), text("z", "1")), List(c, b))
        val edited  = core("a", List(text("m", "2"), text("z", "9")), List(b, c))
        val digests = Map(b -> bHash, c -> cHash)
        val swapped = Map(b -> cHash, c -> bHash)
        val sha     = Sha256Digest.of(utf8("2"))
        val asText  = core("a", List(Input(InputName.trusted("m"), InputValue.Text(sha.value))))
        val asHash  = core("a", List(Input(InputName.trusted("m"), InputValue.Hash(sha))))
        val missing = ActionDigest.of(core("a", deps = List(b)), Map.empty)
        val present = ActionDigest.of(core("a", deps = List(b)), Map(b -> bHash))
        val named   = ActionDigest.of(forward, digests)
        assertTrue(
          named == ActionDigest.of(flipped, digests),
          named != ActionDigest.of(edited, digests),
          named != ActionDigest.of(forward, swapped),
          named != ActionDigest.of(core("other", List(text("z", "1"), text("m", "2")), List(b, c)), digests),
          ActionDigest.of(asText, Map.empty) != ActionDigest.of(asHash, Map.empty),
          missing != present,
          missing != ActionDigest.of(core("a"), Map.empty),
          named.value.length == 64,
          Digest.from(named.value) == Some(named),
        )
      ,
      test("plugin seal and needs move the key, and the repo root and output list do not"):
        val bare   = ActionDigest.of(plugin(Seal.Hermetic), Map.empty)
        val rooted = ActionDigest.of(
          plugin(Seal.Hermetic, root = Some(Path.of("/repo")), outputs = List(RepoPath("a.txt"))),
          Map.empty,
        )
        val withOne  = ActionDigest.of(plugin(Seal.Hermetic, Set(Need.Network)), Map.empty)
        val withBoth = ActionDigest.of(plugin(Seal.Hermetic, Set(Need.Docker, Need.Network)), Map.empty)
        val open     = ActionDigest.of(plugin(Seal.Open), Map.empty)
        assertTrue(
          bare == rooted,
          bare != open,
          bare != withOne,
          withOne != withBoth,
          bare != ActionDigest.of(core("pack"), Map.empty),
          Need.values.toList.forall(need => ActionDigest.of(plugin(Seal.Hermetic, Set(need)), Map.empty) != bare),
        )
      ,
      test("bytes hash as their sha256"):
        val bytes = utf8("action")
        val other = utf8("other")
        assertTrue(
          ActionDigest.ofBytes(bytes) == Digest.trusted(Sha256Digest.of(bytes).value),
          ActionDigest.ofBytes(bytes) != ActionDigest.ofBytes(other),
        ),
    ),
    suite("the remote target is data")(
      test("a blank uri still rejects a bad mode or namespace and otherwise stays off"):
        assertTrue(
          CacheConfig.decide(Map.empty) == Right(None),
          CacheConfig.decide(Map(CacheConfig.UriKey -> "   ")) == Right(None),
          CacheConfig.decide(Map(CacheConfig.ModeKey -> "read")) == Right(None),
          CacheConfig.decide(Map(CacheConfig.ModeKey -> "write")) == Right(None),
          CacheConfig.decide(Map(CacheNamespace.Key -> "main")) == Right(None),
          CacheConfig.decide(Map(CacheConfig.ModeKey -> "Write")) == Left(RemoteError.Mode("Write")),
          CacheConfig.decide(Map(CacheConfig.ModeKey -> "both")) == Left(RemoteError.Mode("both")),
          CacheConfig.decide(Map(CacheConfig.UriKey -> "   ", CacheConfig.ModeKey -> "both")) ==
            Left(RemoteError.Mode("both")),
          CacheConfig.decide(Map(CacheNamespace.Key -> "pr/0")) ==
            Left(RemoteError.Namespace(NamespaceError.Number(PullRequestError.NotPositive(0)))),
          CacheConfig.decide(Map(CacheNamespace.Key -> "pr/01")) ==
            Left(RemoteError.Namespace(NamespaceError.Number(PullRequestError.NotANumber("01")))),
          CacheConfig.decide(Map(CacheNamespace.Key -> "nope")) ==
            Left(RemoteError.Namespace(NamespaceError.Shape("nope"))),
          CacheConfig.decide(Map(CacheNamespace.Key -> "dev/a/b")) ==
            Left(RemoteError.Namespace(NamespaceError.Shape("dev/a/b"))),
          CacheConfig.decide(Map(CacheNamespace.Key -> "dev/")) ==
            Left(RemoteError.Namespace(NamespaceError.Name(DevNameError.Segment("")))),
        )
      ,
      test("a bad scheme or address fails and does not fall through to off"):
        val bad = List(
          "grpc://:2020"           -> RemoteError.Address("grpc://:2020"),
          "grpc://localhost:0"     -> RemoteError.Address("grpc://localhost:0"),
          "grpc://localhost:65536" -> RemoteError.Address("grpc://localhost:65536"),
          "grpc://host:2020/extra" -> RemoteError.Address("grpc://host:2020/extra"),
          "grpc://"                -> RemoteError.Address("grpc://"),
          "grpc://host:2020?x"     -> RemoteError.Address("grpc://host:2020?x"),
          "grpc://host:2020#frag"  -> RemoteError.Address("grpc://host:2020#frag"),
          "grpc://user@host:2020"  -> RemoteError.Address("grpc://user@host:2020"),
          "ftp://x"                -> RemoteError.Scheme("ftp"),
          "localhost:2020"         -> RemoteError.Address("localhost:2020"),
          "http://"                -> RemoteError.Address("http://"),
        )
        val decided  = bad.map((raw, _) => CacheConfig.decide(Map(CacheConfig.UriKey -> raw)))
        val expected = bad.map((_, error) => Left(error))
        assertTrue(decided == expected)
      ,
      test("grpc and grpcs keep the host, port, tls bit, mode, and namespace"):
        val plain = CacheConfig.decide(
          Map(CacheConfig.UriKey -> "grpc://localhost:2020", CacheNamespace.Key -> "pr/2")
        )
        val tls = CacheConfig.decide(
          Map(
            CacheConfig.UriKey  -> "grpcs://cache.example:443",
            CacheConfig.ModeKey -> "write",
            CacheNamespace.Key  -> "dev/demo",
          )
        )
        val edge = List("grpc://localhost:1", "grpc://localhost:65535").map { raw =>
          CacheConfig.decide(Map(CacheConfig.UriKey -> raw))
        }
        assertTrue(
          plain == CacheNamespace.from("pr/2").map { namespace =>
            Some(
              CacheConfig.Selected(
                CacheConfig.Target.Grpc("localhost", 2020, tls = false),
                RemoteMode.Read,
                namespace,
              )
            )
          },
          tls == CacheNamespace.from("dev/demo").map { namespace =>
            Some(
              CacheConfig.Selected(
                CacheConfig.Target.Grpc("cache.example", 443, tls = true),
                RemoteMode.Write,
                namespace,
              )
            )
          },
          edge == List(
            Right(
              Some(
                CacheConfig
                  .Selected(CacheConfig.Target.Grpc("localhost", 1, tls = false), RemoteMode.Read, CacheNamespace.Main)
              )
            ),
            Right(
              Some(
                CacheConfig.Selected(
                  CacheConfig.Target.Grpc("localhost", 65535, tls = false),
                  RemoteMode.Read,
                  CacheNamespace.Main,
                )
              )
            ),
          ),
        )
      ,
      test("http and https keep the address they were given, and do not invent a grpc target"):
        val urls  = List("https://cache.example/objects", "http://cache.example")
        val shown = urls.map { raw =>
          CacheConfig.decide(Map(CacheConfig.UriKey -> raw, CacheConfig.ModeKey -> "write")) match
            case Right(Some(CacheConfig.Selected(CacheConfig.Target.Http(url), mode, namespace))) =>
              Some((url.render, mode, namespace))
            case _ => None
        }
        val rendered = List("https://cache.example/objects", "http://cache.example/")
        assertTrue(shown == rendered.map(url => Some((url, RemoteMode.Write, CacheNamespace.Main))))
      ,
      test("main, a pull request, and a dev name read themselves and then main"):
        assertTrue(
          CacheNamespace.Main.reads == List(CacheNamespace.Main),
          CacheNamespace.Main.token == "main",
          CacheNamespace.from("pr/2").map(namespace => namespace.token -> namespace.reads.map(_.token)) ==
            Right("pr/2" -> List("pr/2", "main")),
          CacheNamespace.from("dev/demo").map(namespace => namespace.token -> namespace.reads.map(_.token)) ==
            Right("dev/demo" -> List("dev/demo", "main")),
        )
      ,
      test("an empty environment opens the unconfigured cache"):
        for
          cache  <- CacheConfig.open(Map.empty)
          direct <- ZIO.succeed(RemoteCache.unconfigured)
          digest = Digest.trusted("ab" * 32)
          found        <- cache.find(digest)
          directFound  <- direct.find(digest)
          stored       <- cache.store(digest, Chunk.empty).either
          purged       <- cache.purge.either
          directStored <- direct.store(digest, Chunk.empty).either
          directPurged <- direct.purge.either
        yield assertTrue(
          cache.mode == RemoteMode.Off,
          direct.mode == RemoteMode.Off,
          found == None,
          directFound == None,
          stored == Left(RemoteError.Off),
          purged == Left(RemoteError.Off),
          directStored == Left(RemoteError.Off),
          directPurged == Left(RemoteError.Off),
        )
      ,
      test("an unset cache and a grpc cache refuse promote"):
        for
          unset <- CacheConfig.promote(Map.empty, CacheNamespace.Main, CacheNamespace.Main).either
          grpc  <- CacheConfig
            .promote(
              Map(CacheConfig.UriKey -> "grpc://localhost:2020"),
              CacheNamespace.Main,
              CacheNamespace.Main,
            )
            .either
        yield assertTrue(
          unset == Left(RemoteError.Off),
          grpc == Left(RemoteError.HttpOnly("cache promote")),
        ),
    ),
    suite("a manifest, an action result, and index promotion")(
      test("empty outputs keep the task text, and a bad manifest returns the stored string"):
        val raw   = "histos-manifest v1\nstill text"
        val short = "histos-manifest v1\n2\nonly"
        val zero  = "histos-manifest v1\n0\ntask"
        assertTrue(
          PluginManifest.store(raw, Nil) == raw,
          PluginManifest.text(raw) == raw,
          PluginManifest.text(short) == short,
          PluginManifest.text(zero) == "task",
          PluginManifest.Header == "histos-manifest v1",
        )
      ,
      test("a declared file must match the recorded sha, and empty outputs do not read the disk"):
        Workspace
          .work("histos-manifest")
          .flatMap: root =>
            val file    = root.resolve("out.txt")
            val path    = RepoPath("out.txt")
            val sha     = Sha256Digest.of(utf8("body"))
            val stored  = PluginManifest.store("task", List(path -> sha))
            val changed = PluginManifest.store("task", List(path -> Sha256Digest.of(utf8("BODY"))))
            for
              _       <- Workspace.writeString(file, "body")
              ok      <- PluginManifest.accept(stored, root, List(path))
              extra   <- PluginManifest.accept(stored, root, List(path, RepoPath("other.txt")))
              garbage <- PluginManifest.accept("nope", root, List(path))
              stale   <- PluginManifest.accept(changed, root, List(path))
              skipped <- PluginManifest.accept("nope", Path.of("/missing-manifest-root"), Nil)
            yield assertTrue(
              ok,
              extra == false,
              garbage == false,
              stale == false,
              skipped,
              PluginManifest.text(stored) == "task",
              stored != "task",
            )
      ,
      test("a symlink is not a declared output"):
        Workspace
          .work("histos-manifest-link")
          .flatMap: root =>
            val real = root.resolve("real.txt")
            val link = root.resolve("link.txt")
            val sha  = Sha256Digest.of(utf8("body"))
            for
              _ <- Workspace.writeString(real, "body")
              _ <- ZIO
                .attemptBlocking(Files.createSymbolicLink(link, real))
                .mapError(_ => IoFailure(link, IoFault.Create))
              ok <- PluginManifest.accept(
                PluginManifest.store("task", List(RepoPath("link.txt") -> sha)),
                root,
                List(RepoPath("link.txt")),
              )
            yield assertTrue(ok == false)
      ,
      test("a zero stdout size is omitted and a truncated result does not decode"):
        val digest = Digest.trusted("cd" * 32)
        val zero   = ActionResult.encode(digest, 0)
        val seven  = ActionResult.encode(digest, 7)
        assertTrue(
          ActionResult.decode(zero) == Some(ActionResult.Pointer(digest, 0L)),
          ActionResult.decode(seven) == Some(ActionResult.Pointer(digest, 7L)),
          zero.length + 2 == seven.length,
          ActionResult.decode(Chunk.empty) == None,
          ActionResult.decode(Chunk(1.toByte, 2.toByte, 3.toByte)) == None,
          ActionResult.decode(zero.dropRight(1)) == None,
          ActionResult.decode(seven.dropRight(1)) == None,
        )
      ,
      test("copy index runs the copy, and rebuild does not"):
        val rows = List(Promote.CopyIndex -> true, Promote.Rebuild -> false)
        ZIO
          .foreach(rows): (promote, _) =>
            Ref.make(false).flatMap(copied => IndexPromote.run(promote, copied.set(true)) *> copied.get)
          .map(ran => assertTrue(rows.map(_._1).toSet == Promote.values.toSet, ran == rows.map(_._2))),
    ),
    suite("one permit per need")(
      test("cpu permits exclude, including a zero cpu limit"):
        for
          one  <- exclude(Set(StepNeed.Cpu), Parallelism(1, 8, 8))
          zero <- exclude(Set(StepNeed.Cpu), Parallelism(0, 8, 8))
        yield assertTrue(one, zero)
      ,
      test("a step with no declared need takes the one cpu permit"):
        for
          one  <- exclude(Set.empty, Parallelism(1, 8, 8))
          zero <- exclude(Set.empty, Parallelism(0, 8, 8))
        yield assertTrue(one, zero)
      ,
      test("an io step and a process step each take their own permit"):
        for
          io      <- exclude(Set(StepNeed.Io), Parallelism(8, 1, 8))
          process <- exclude(Set(StepNeed.Process), Parallelism(8, 8, 1))
        yield assertTrue(io, process)
      ,
      test("two io steps overlap while the cpu permit is one"):
        overlap(StepNeed.Io, Parallelism(1, 2, 8))
      ,
      test("two process steps overlap while the cpu permit is one"):
        overlap(StepNeed.Process, Parallelism(1, 8, 2))
      ,
      test("two cached steps with one id run the body once and emit two events"):
        val program =
          for
            runs <- Ref.make(0)
            info = StepInfo(StepId.Work("a"), Chunk.empty, Set(StepNeed.Cpu))
            step = Step.cached[String](info)(runs.updateAndGet(_ + 1).map(_.toString))
            pair <- observe(step.zip(step), StepId.Work("a"))
            (outcome, events) = pair
            called <- runs.get
            sources = finished(events)
          yield assertTrue(
            called == 1,
            outcome == Outcome.Done(("1", "1")),
            events.size == 2,
            sources == Chunk(Source.Ran),
          )
        program.provideSomeLayer[TestEnvironment & Scope](runnerLayer(Parallelism(4, 4, 4)))
      ,
      test("an upstream failure skips the continuation and keeps the upstream cause"):
        val program =
          for
            up   = Step.local[String](StepInfo(StepId.Work("up"), Chunk.empty, Set(StepNeed.Cpu)))(ZIO.fail("boom"))
            down = up.andThenHermetic[String](StepInfo(StepId.Work("down"), Chunk.empty, Set(StepNeed.Cpu)))(_ =>
              ZIO.die(new IllegalStateException("continued"))
            )
            pair <- observe(down, StepId.Work("down"))
          yield assertTrue(
            shown(
              pair._1,
              pair._2,
            ) == "failed up Some(boom) | started up;stopped failed up Some(boom);stopped skipped down up"
          )
        program.provideSomeLayer[TestEnvironment & Scope](runnerLayer(Parallelism(2, 2, 2))),
    ),
    (suite("a cache hit hides nothing")(
      test("prepare rejects a bad plan before the cache or the body"):
        for
          seen <- arm(RemoteMode.Off, _ => ZIO.none, _ => ZIO.some("cached"))
          runs <- Ref.make(0)
          local = (_: Action, _: String) => runs.updateAndGet(_ + 1).as("ran")
          dup <- execute(ActionId.trusted("missing"), List(core("a"), core("a")), local).either
          cyc <- execute(
            ActionId.trusted("a"),
            List(core("a", deps = List(ActionId.trusted("b"))), core("b", deps = List(ActionId.trusted("a")))),
            local,
          ).either
          missed <- execute(ActionId.trusted("a"), List(core("a", deps = List(ActionId.trusted("b")))), local).either
          inputs <- execute(
            ActionId.trusted("a"),
            List(core("a", List(text("k", "1"), text("k", "2")))),
            local,
          ).either
          unknown <- execute(ActionId.trusted("gone"), Nil, local).either
          got     <- seen.gets.get
          looked  <- seen.finds.get
          wrote   <- seen.puts.get
          called  <- runs.get
        yield assertTrue(
          dup == Left(ScheduleError.DuplicateAction(ActionId.trusted("a"))),
          cyc == Left(ScheduleError.Cycle(List(ActionId.trusted("a"), ActionId.trusted("b"), ActionId.trusted("a")))),
          missed == Left(ScheduleError.MissingDependency(ActionId.trusted("a"), ActionId.trusted("b"))),
          inputs == Left(ScheduleError.DuplicateInput(ActionId.trusted("a"), InputName.trusted("k"))),
          unknown == Left(ScheduleError.UnknownAction(ActionId.trusted("gone"))),
          got == 0,
          looked == 0,
          wrote.isEmpty,
          called == 0,
        )
      ,
      test("a remote hit returns the stored text and does not consult the local cache"):
        val action = core("a")
        for
          runs   <- Ref.make(0)
          seen   <- arm(RemoteMode.Read, _ => ZIO.some(utf8("remote")), _ => ZIO.some("local"))
          pair   <- watch(ActionId.trusted("a"), List(action), (_, _) => runs.updateAndGet(_ + 1).as("ran"))
          called <- runs.get
          got    <- seen.gets.get
          looked <- seen.finds.get
          wrote  <- seen.puts.get
          (outcome, events) = pair
        yield assertTrue(
          outcome == Outcome.Done("remote"),
          called == 0,
          got == 0,
          looked == 1,
          wrote.isEmpty,
          events.size == 2,
          finished(events) == Chunk(Source.RemoteHit),
        )
      ,
      test("remote bytes that are not utf-8 fail before the local cache"):
        val action = core("a")
        val digest = ActionDigest.of(action, Map.empty)
        for
          seen   <- arm(RemoteMode.Read, _ => ZIO.some(Chunk.single(0xff.toByte)), _ => ZIO.some("local"))
          result <- execute(ActionId.trusted("a"), List(action), (_, _) => ZIO.succeed("ran")).either
          got    <- seen.gets.get
        yield assertTrue(
          result == Left(ScheduleError.Remote(RemoteError.Text(digest))),
          got == 0,
        )
      ,
      test("a local hit does not run the body or write the cache"):
        val action = core("a")
        for
          runs   <- Ref.make(0)
          seen   <- arm(RemoteMode.Off, _ => ZIO.some(utf8("remote")), _ => ZIO.some("kept"))
          pair   <- watch(ActionId.trusted("a"), List(action), (_, _) => runs.updateAndGet(_ + 1).as("ran"))
          called <- runs.get
          looked <- seen.finds.get
          wrote  <- seen.puts.get
          (outcome, events) = pair
        yield assertTrue(
          outcome == Outcome.Done("kept"),
          called == 0,
          looked == 0,
          wrote.isEmpty,
          events.size == 2,
          finished(events) == Chunk(Source.LocalHit),
        )
      ,
      test("a core miss runs, stores, and records that it ran"):
        val action = core("a")
        for
          runs   <- Ref.make(0)
          seen   <- arm(RemoteMode.Off, _ => ZIO.none, _ => ZIO.none)
          pair   <- watch(ActionId.trusted("a"), List(action), (_, _) => runs.updateAndGet(_ + 1).as("out"))
          called <- runs.get
          looked <- seen.finds.get
          wrote  <- seen.puts.get
          (outcome, events) = pair
        yield assertTrue(
          outcome == Outcome.Done("out"),
          called == 1,
          looked == 0,
          wrote == Chunk("out"),
          events.size == 2,
          finished(events) == Chunk(Source.Ran),
        )
      ,
      test("a remote miss falls through to the local cache and does not upload a read cache"):
        val action = core("a")
        for
          seen   <- arm(RemoteMode.Read, _ => ZIO.none, _ => ZIO.none)
          pair   <- watch(ActionId.trusted("a"), List(action), (_, _) => ZIO.succeed("out"))
          looked <- seen.finds.get
          wrote  <- seen.puts.get
          stored <- seen.stores.get
          (outcome, events) = pair
        yield assertTrue(
          outcome == Outcome.Done("out"),
          looked == 1,
          wrote == Chunk("out"),
          stored.isEmpty,
          finished(events) == Chunk(Source.Ran),
        )
      ,
      test("a changed input is a miss when the cache only holds the previous key"):
        val old = core("a", List(text("k", "1")))
        val now = core("a", List(text("k", "2")))
        val key = ActionDigest.of(old, Map.empty)
        for
          runs  <- Ref.make(0)
          _     <- arm(RemoteMode.Off, _ => ZIO.none, digest => if digest == key then ZIO.some("stale") else ZIO.none)
          hit   <- execute(ActionId.trusted("a"), List(old), (_, _) => runs.updateAndGet(_ + 1).as("fresh"))
          after <- runs.get
          fresh <- execute(ActionId.trusted("a"), List(now), (_, _) => runs.updateAndGet(_ + 1).as("fresh"))
          end   <- runs.get
        yield assertTrue(hit == "stale", after == 0, fresh == "fresh", end == 1, key != ActionDigest.of(now, Map.empty))
      ,
      test("the child sees the parent output, and a parent input change misses both"):
        for
          _      <- arm(RemoteMode.Off, _ => ZIO.none, _ => ZIO.none)
          passed <- execute(
            ActionId.trusted("child"),
            List(core("parent", List(text("n", "1"))), core("child", deps = List(ActionId.trusted("parent")))),
            (action, upstream) =>
              if action.id == ActionId.trusted("child") then ZIO.succeed(s"saw:$upstream")
              else ZIO.succeed("parent-out"),
          )
          saved <- Ref.make(Map.empty[String, String])
          runs  <- Ref.make(0)
          seen  <- arm(RemoteMode.Off, _ => ZIO.none, digest => saved.get.map(_.get(digest.value)))
          _     <- stub[ActionCache](_.put(any[Digest], any[String]))((digest: Digest, value: String) =>
            seen.puts.update(_ :+ value) *> saved.update(_ + (digest.value -> value))
          )
          local = (_: Action, _: String) => runs.updateAndGet(_ + 1).map(_.toString)
          first  <- execute(ActionId.trusted("child"), plan("1"), local)
          once   <- runs.get
          puts1  <- seen.puts.get
          second <- execute(ActionId.trusted("child"), plan("1"), local)
          twice  <- runs.get
          puts2  <- seen.puts.get
          third  <- execute(ActionId.trusted("child"), plan("2"), local)
          end    <- runs.get
          puts3  <- seen.puts.get
        yield assertTrue(
          passed == "saw:parent-out",
          first == "2",
          once == 2,
          puts1 == Chunk("1", "2"),
          second == "2",
          twice == 2,
          puts2 == Chunk("1", "2"),
          third == "4",
          end == 4,
          puts3 == Chunk("1", "2", "3", "4"),
        )
      ,
      test("a hermetic plugin is stored locally in every mode and uploaded only when the cache is writable"):
        val action                 = plugin(Seal.Hermetic)
        val digest                 = ActionDigest.of(action, Map.empty)
        def once(mode: RemoteMode) =
          for
            seen   <- arm(mode, _ => ZIO.none, _ => ZIO.none)
            value  <- ZIO.serviceWithZIO[Scheduler](_.cached(action, digest, ZIO.succeed("out")))
            puts   <- seen.puts.get
            stores <- seen.stores.get
            looked <- seen.finds.get
          yield (value, puts, stores, looked)
        for
          off   <- once(RemoteMode.Off)
          read  <- once(RemoteMode.Read)
          write <- once(RemoteMode.Write)
        yield assertTrue(
          off == ("out", Chunk("out"), Chunk.empty[Chunk[Byte]], 0),
          read == ("out", Chunk("out"), Chunk.empty[Chunk[Byte]], 1),
          write == ("out", Chunk("out"), Chunk(utf8("out")), 1),
        )
      ,
      test("an open plugin is not stored, unless the caller sends it"):
        val action = plugin(Seal.Open)
        val digest = ActionDigest.of(action, Map.empty)
        for
          stayed       <- arm(RemoteMode.Write, _ => ZIO.none, _ => ZIO.none)
          stayedValue  <- ZIO.serviceWithZIO[Scheduler](_.cached(action, digest, ZIO.succeed("out")))
          stayedPuts   <- stayed.puts.get
          stayedStores <- stayed.stores.get
          sent         <- arm(RemoteMode.Write, _ => ZIO.none, _ => ZIO.none)
          sentValue    <- ZIO.serviceWithZIO[Scheduler](
            _.cached(action, digest, ZIO.succeed("out"), _ => Verdict.Send)
          )
          sentPuts   <- sent.puts.get
          sentStores <- sent.stores.get
        yield assertTrue(
          stayedValue == "out",
          stayedPuts.isEmpty,
          stayedStores.isEmpty,
          sentValue == "out",
          sentPuts == Chunk("out"),
          sentStores == Chunk(utf8("out")),
        )
      ,
      test("a core result is stored even when the verdict stays, and that stay is not uploaded"):
        val action = core("a")
        val digest = ActionDigest.of(action, Map.empty)
        for
          seen  <- arm(RemoteMode.Write, _ => ZIO.none, _ => ZIO.none)
          value <- ZIO.serviceWithZIO[Scheduler](
            _.cached(action, digest, ZIO.succeed("out"), _ => Verdict.Stay(Hold.Test))
          )
          puts   <- seen.puts.get
          stores <- seen.stores.get
        yield assertTrue(value == "out", puts == Chunk("out"), stores.isEmpty)
      ,
      test("a plugin with no outputs returns the stored manifest and does not run"):
        val action = plugin(Seal.Hermetic)
        val stored = PluginManifest.store("task", List(RepoPath("a.txt") -> Sha256Digest.of(utf8("x"))))
        val digest = ActionDigest.of(action, Map.empty)
        for
          runs   <- Ref.make(0)
          _      <- arm(RemoteMode.Off, _ => ZIO.none, _ => ZIO.some(stored))
          value  <- ZIO.serviceWithZIO[Scheduler](_.cached(action, digest, runs.updateAndGet(_ + 1).as("fresh")))
          called <- runs.get
        yield assertTrue(
          value == stored,
          called == 0,
          PluginManifest.text(stored) == "task",
          stored != "task",
        )
      ,
      test("a plugin hit whose declared file is gone runs the task"):
        val action =
          plugin(Seal.Hermetic, root = Some(Path.of("/missing-plugin-root")), outputs = List(RepoPath("missing.txt")))
        val stored = PluginManifest.store("task", List(RepoPath("missing.txt") -> Sha256Digest.of(utf8("x"))))
        for
          runs  <- Ref.make(0)
          _     <- arm(RemoteMode.Off, _ => ZIO.none, _ => ZIO.some(stored))
          value <- ZIO.serviceWithZIO[Scheduler](
            _.cached(action, ActionDigest.of(action, Map.empty), runs.updateAndGet(_ + 1).as("fresh"))
          )
          called <- runs.get
        yield assertTrue(value == "fresh", called == 1, value != stored),
    ) @@ TestAspect.sequential).provideSomeLayerShared[TestEnvironment & Scope](scheduled),
  )

  private def runnerLayer(limits: Parallelism): ZLayer[Any, Nothing, Histos] =
    ZLayer.succeed(limits) >>> Histos.live

  private def exclude(needs: Set[StepNeed], limits: Parallelism): ZIO[TestEnvironment & Scope, Nothing, Boolean] =
    val program =
      for
        inside  <- Ref.make(0)
        peak    <- Ref.make(0)
        held    <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        enter =
          inside.updateAndGet(_ + 1).flatMap { n =>
            peak.update(high => if n > high then n else high) *>
              held.succeed(()).unit *>
              release.await *>
              inside.update(_ - 1).unit
          }
        first  = Step.local[Unit](StepInfo(StepId.Work("a"), Chunk.empty, needs))(enter)
        second = Step.local[Unit](StepInfo(StepId.Work("b"), Chunk.empty, needs))(enter)
        high <- ZIO.serviceWithZIO[Histos]: histos =>
          ZIO.scoped:
            for
              running <- histos.start(first.zip(second).map(_ => ()))
              _       <- held.await
              _       <- ZIO.yieldNow.repeat(Schedule.recurs(32))
              found   <- peak.get
              _       <- release.succeed(())
              outcome <- running.outcome
            yield found == 1 && outcome == Outcome.Done(())
      yield high
    program.provideSomeLayer[TestEnvironment & Scope](runnerLayer(limits))

  private def overlap(need: StepNeed, limits: Parallelism): ZIO[TestEnvironment & Scope, Nothing, TestResult] =
    val program =
      for
        arrived <- Ref.make(0)
        both    <- Promise.make[Nothing, Unit]
        enter =
          arrived.updateAndGet(_ + 1).flatMap { n =>
            ZIO.when(n == 2)(both.succeed(())).unit *> both.await
          }
        first  = Step.local[Unit](StepInfo(StepId.Work("a"), Chunk.empty, Set(need)))(enter)
        second = Step.local[Unit](StepInfo(StepId.Work("b"), Chunk.empty, Set(need)))(enter)
        pair <- ZIO.serviceWithZIO[Histos]: histos =>
          ZIO.scoped:
            for
              running <- histos.start(first.zip(second).map(_ => ()))
              outcome <- running.outcome
            yield outcome
        called <- arrived.get
      yield assertTrue(called == 2, pair == Outcome.Done(()))
    program.provideSomeLayer[TestEnvironment & Scope](runnerLayer(limits))

  /** The outcome, and every event up to the one that ends `last`. The goal's own step ends last, so nothing follows. */
  private def observe[E, A](
      step: Step[Any, E, A],
      last: StepId,
  ): ZIO[Histos, Nothing, (Outcome[E, A], Chunk[StepEvent[E]])] =
    ZIO.serviceWithZIO[Histos]: histos =>
      ZIO.scoped:
        for
          running  <- histos.start(step)
          found    <- Ref.make(Chunk.empty[StepEvent[E]])
          recorded <- Promise.make[Nothing, Chunk[StepEvent[E]]]
          _        <- running.events
            .tap(event =>
              found.updateAndGet(_ :+ event).flatMap(chunk => ZIO.when(ends(event, last))(recorded.succeed(chunk)))
            )
            .runDrain
            .fork
          outcome <- running.outcome
          events  <- recorded.await
        yield (outcome, events)

  private def ends[E](event: StepEvent[E], id: StepId): Boolean =
    event match
      case StepEvent.Started(_)                               => false
      case StepEvent.Finished(finished, _, _)                 => finished == id
      case StepEvent.Stopped(StepFailure.Failed(failed, _))   => failed == id
      case StepEvent.Stopped(StepFailure.Skipped(skipped, _)) => skipped == id

  private def shown[E](outcome: Outcome[E, String], events: Chunk[StepEvent[E]]): String =
    val result = outcome match
      case Outcome.Done(value)      => s"done $value"
      case Outcome.Failed(failures) =>
        failures
          .map:
            case StepFailure.Failed(StepId.Work(key), cause) => s"failed $key ${cause.failureOption}"
            case StepFailure.Failed(id, cause)               => s"failed $id ${cause.failureOption}"
            case StepFailure.Skipped(id, upstream)           => s"skipped $id $upstream"
          .mkString(",")
    s"$result | ${events.map(showEvent).mkString(";")}"

  private def showEvent[E](event: StepEvent[E]): String =
    event match
      case StepEvent.Started(StepId.Work(key))                            => s"started $key"
      case StepEvent.Started(id)                                          => s"started $id"
      case StepEvent.Finished(id, source, _)                              => s"finished $id $source"
      case StepEvent.Stopped(StepFailure.Failed(StepId.Work(key), cause)) =>
        s"stopped failed $key ${cause.failureOption}"
      case StepEvent.Stopped(StepFailure.Failed(id, cause)) =>
        s"stopped failed $id ${cause.failureOption}"
      case StepEvent.Stopped(StepFailure.Skipped(StepId.Work(key), upstream)) =>
        val ids = upstream.map:
          case StepId.Work(text) => text
          case other             => other.toString
        s"stopped skipped $key ${ids.mkString(",")}"
      case StepEvent.Stopped(StepFailure.Skipped(id, _)) => s"stopped skipped $id"

  private def finished[E](events: Chunk[StepEvent[E]]): Chunk[Source] =
    events.collect { case StepEvent.Finished(_, source, _) => source }

  /** Started is not enough: the queue still holds the terminal event until a consumer takes it. */
  private def terminal[E](events: Chunk[StepEvent[E]]): Boolean =
    events.exists:
      case StepEvent.Started(_) => false
      case _                    => true

  private final case class Seen(
      gets: Ref[Int],
      puts: Ref[Chunk[String]],
      finds: Ref[Int],
      stores: Ref[Chunk[Chunk[Byte]]],
  )

  private def arm(
      mode: RemoteMode,
      found: Digest => ZIO[Any, RemoteError, Option[Chunk[Byte]]],
      hit: Digest => ZIO[Any, CacheError, Option[String]],
  ): ZIO[
    Scheduler & ActionCache & RemoteCache & Histos & slayer.Stubbed[ActionCache] & slayer.Stubbed[RemoteCache],
    Nothing,
    Seen,
  ] =
    for
      _      <- ZIO.service[Scheduler]
      _      <- ZIO.service[ActionCache]
      _      <- ZIO.service[RemoteCache]
      _      <- ZIO.service[Histos]
      gets   <- Ref.make(0)
      puts   <- Ref.make(Chunk.empty[String])
      finds  <- Ref.make(0)
      stores <- Ref.make(Chunk.empty[Chunk[Byte]])
      _      <- stub[RemoteCache](_.mode)(mode)
      _      <- stub[RemoteCache](_.find(any[Digest]))((digest: Digest) => finds.update(_ + 1) *> found(digest))
      _      <- stub[RemoteCache](_.store(any[Digest], any[Chunk[Byte]]))((_: Digest, bytes: Chunk[Byte]) =>
        stores.update(_ :+ bytes)
      )
      _ <- stub[RemoteCache](_.purge)(ZIO.die(new IllegalStateException("purge")))
      _ <- stub[ActionCache](_.get(any[Digest]))((digest: Digest) => gets.update(_ + 1) *> hit(digest))
      _ <- stub[ActionCache](_.put(any[Digest], any[String]))((_: Digest, value: String) => puts.update(_ :+ value))
    yield Seen(gets, puts, finds, stores)

  private def execute[E](
      root: ActionId,
      actions: List[Action],
      local: (Action, String) => ZIO[Any, E, String],
      verdict: Action => Verdict = Sendable.ofAction,
  ): ZIO[Scheduler, ScheduleError[E], String] =
    ZIO.serviceWithZIO[Scheduler](_.execute(root, actions, local, verdict))

  private def watch[E](
      root: ActionId,
      actions: List[Action],
      local: (Action, String) => ZIO[Any, E, String],
  ): ZIO[Scheduler & Histos, ScheduleError[
    E
  ], (Outcome[ScheduleError[E], String], Chunk[StepEvent[ScheduleError[E]]])] =
    for
      step <- ZIO.serviceWithZIO[Scheduler](_.planned(root, actions, local))
      pair <- ZIO.serviceWithZIO[Histos]: histos =>
        ZIO.scoped:
          for
            running  <- histos.start(step)
            found    <- Ref.make(Chunk.empty[StepEvent[ScheduleError[E]]])
            recorded <- Promise.make[Nothing, Chunk[StepEvent[ScheduleError[E]]]]
            _        <- running.events
              .tap(event =>
                found.updateAndGet(_ :+ event).flatMap { chunk =>
                  ZIO.when(terminal(chunk))(recorded.succeed(chunk)).unit
                }
              )
              .runDrain
              .fork
            outcome <- running.outcome
            events  <- recorded.await
          yield (outcome, events)
    yield pair

  private def plan(input: String): List[Action] =
    List(core("parent", List(text("n", input))), core("child", deps = List(ActionId.trusted("parent"))))

  private def core(name: String, inputs: List[Input] = Nil, deps: List[ActionId] = Nil): Action =
    Action(ActionId.trusted(name), inputs, deps)

  private def plugin(
      seal: Seal,
      needs: Set[Need] = Set.empty,
      root: Option[Path] = None,
      outputs: List[RepoPath] = Nil,
      name: String = "pack",
  ): Action =
    Action(ActionId.trusted(name), Nil, Nil, Policy.Plugin(seal, needs, root, outputs))

  private def text(name: String, value: String): Input =
    Input(InputName.trusted(name), InputValue.Text(value))

  private def utf8(value: String): Chunk[Byte] =
    Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
end ScheduleLawSpec
