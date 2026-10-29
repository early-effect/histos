package histos.schedule

import heddle.client.{Authority, Client, ClientError}
import heddle.http.Url
import heddle.route.PathDsl.*
import heddle.server.Port
import heddle.{Body, Method, Request, Response, Routes, Server, Status, string}
import histos.Promote
import histos.version.PullRequestNumber

import java.nio.charset.StandardCharsets

import zio.{Chunk, Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object HttpCacheSpec extends histos.SuiteClock:
  private val pullRequest = CacheNamespace.PullRequest(PullRequestNumber("1"))

  private val encoded = suite("action result")(
    test("a zero size is omitted and a non-zero size is a varint"):
      val empty  = Digest.ofBytes(Chunk.empty)
      val text   = Chunk.fromArray("hi".getBytes(StandardCharsets.UTF_8))
      val output = Digest.ofBytes(text)
      val wide   = Digest.ofBytes(Chunk.single(9.toByte))
      val zero   = framed(empty.value, Array.empty)
      val two    = framed(output.value, Array(0x10.toByte, 2.toByte))
      val big    = framed(wide.value, Array(0x10.toByte, 0xc8.toByte, 0x01.toByte))
      assertTrue(
        empty.value.length == 64,
        ActionResult.encode(empty, 0) == zero,
        ActionResult.decode(zero) == Some(ActionResult.Pointer(empty, 0L)),
        ActionResult.encode(output, text.length) == two,
        ActionResult.decode(two) == Some(ActionResult.Pointer(output, text.length.toLong)),
        ActionResult.encode(wide, 200) == big,
        ActionResult.decode(big) == Some(ActionResult.Pointer(wide, 200L)),
        ActionResult.decode(Chunk(0x20.toByte, 0x01.toByte) ++ zero) == Some(ActionResult.Pointer(empty, 0L)),
        ActionResult.decode(Chunk(0x4a.toByte, 0x01.toByte, 0x00.toByte) ++ zero) ==
          Some(ActionResult.Pointer(empty, 0L)),
        ActionResult.decode(Chunk.empty) == None,
        ActionResult.decode(Chunk(0xff.toByte)) == None,
        ActionResult.decode(Chunk(0x32.toByte, 0x02.toByte, 0x0a.toByte)) == None,
      )
    ,
    test("an exchange drops the throwable message"):
      val connect = ClientError.Connect(Authority("cache.example", 9), new RuntimeException("s3cret"))
      val trust   = ClientError.InvalidTrust("s3cret")
      val fault   = HttpCache.exchange(connect)
      assertTrue(
        fault == CacheExchange.Connect("cache.example", 9),
        !fault.message.contains("s3cret"),
        HttpCache.exchange(trust) == CacheExchange.Trust,
        !CacheExchange.Trust.message.contains("s3cret"),
      ),
  )

  private val prefixes = suite("namespace")(
    test("a dev name is one path segment"):
      assertTrue(
        DevName.from("") == Left(DevNameError.Segment("")),
        DevName.from("a/b") == Left(DevNameError.Segment("a/b")),
        DevName.from(".") == Left(DevNameError.Segment(".")),
        DevName.from("..") == Left(DevNameError.Segment("..")),
        DevName.from("laptop").map(_.value) == Right("laptop"),
        DevName.from("a-b_1").map(_.value) == Right("a-b_1"),
      )
    ,
    test("the index prefix is the role, then main"):
      val dev = DevName.from("laptop").map(CacheNamespace.Dev(_))
      assertTrue(
        CacheNamespace.Main.segments == List("main"),
        CacheNamespace.Main.reads == List(CacheNamespace.Main),
        pullRequest.segments == List("pr", "1"),
        pullRequest.reads == List(pullRequest, CacheNamespace.Main),
        dev.map(role => role.segments -> role.reads.map(_.segments)) == Right(
          List("dev", "laptop") -> List(List("dev", "laptop"), List("main"))
        ),
      )
    ,
    test("a namespace token is main, pr/<n>, or dev/<name>"):
      val dev     = CacheNamespace.from("dev/laptop")
      val leading = CacheNamespace.from("pr/01")
      assertTrue(
        CacheNamespace.from("main") == Right(CacheNamespace.Main),
        CacheNamespace.from("pr/1") == Right(pullRequest),
        CacheNamespace.from("pr/11").map(_.token) == Right("pr/11"),
        dev.map(_.token) == Right("dev/laptop"),
        CacheNamespace.Main.token == "main",
        pullRequest.token == "pr/1",
        CacheNamespace.from("nope") == Left(NamespaceError.Shape("nope")),
        CacheNamespace.from("pr/1/extra") == Left(NamespaceError.Shape("pr/1/extra")),
        (leading match
          case Left(error) => error.message.contains("01")
          case Right(_)    => false
        ),
      ),
  )

  private val live = suite("loopback")(
    test("put then get returns the same bytes"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          action = digest("action")
          bytes  = utf8("compile")
          output = Digest.ofBytes(bytes)
          _     <- cache.store(action, bytes)
          found <- cache.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /main/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("an index entry that names a missing blob is dangling"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action  = digest("dangling")
          missing = Digest.ofBytes(Chunk.single(7.toByte))
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(missing, 1))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Dangling(action, missing)),
          calls == List(s"GET /main/ac/${action.value}", s"GET /cas/${missing.value}"),
        )
    ,
    test("off does not call the store"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Off, CacheNamespace.Main)
          action = digest("off")
          found   <- cache.find(action)
          refused <- cache.store(action, utf8("nope")).either
          calls   <- stand.calls
        yield assertTrue(found == None, refused == Left(RemoteError.Off), calls.isEmpty)
    ,
    test("read refuses store and still reads"):
      ZIO.scoped:
        for
          stand  <- Stand.open
          writer <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          reader <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("read")
          bytes  = utf8("from-remote")
          output = Digest.ofBytes(bytes)
          _       <- writer.store(action, bytes)
          found   <- reader.find(action)
          refused <- reader.store(action, bytes).either
          calls   <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          refused == Left(RemoteError.ReadOnly),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /main/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a missing action is a miss"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("missing")
          found <- cache.find(action)
          calls <- stand.calls
        yield assertTrue(found == None, calls == List(s"GET /main/ac/${action.value}"))
    ,
    test("bytes that do not match the digest are a mismatch"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("mismatch")
          bytes  = utf8("right")
          output = Digest.ofBytes(bytes)
          _     <- stand.plantCas(output.value, utf8("nope"))
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(output, bytes.length))
          found <- cache.find(action).either
        yield assertTrue(found == Left(RemoteError.Mismatch(action, output)))
    ,
    test("a body that is not an action result is an index error"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("index")
          _     <- stand.plantAc("main/ac", action.value, utf8("nope"))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Index(action)),
          calls == List(s"GET /main/ac/${action.value}"),
        )
    ,
    test("a refused blob put does not write the index"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          action = digest("status")
          bytes  = utf8("blob")
          output = Digest.ofBytes(bytes)
          _       <- stand.fail(500)
          refused <- cache.store(action, bytes).either
          found   <- cache.find(action)
          calls   <- stand.calls
        yield assertTrue(
          refused == Left(RemoteError.Status(500)),
          found == None,
          calls == List(s"PUT /cas/${output.value}", s"GET /main/ac/${action.value}"),
        )
    ,
    test("a write to pr/1 is invisible to main and does not write main"):
      ZIO.scoped:
        for
          stand  <- Stand.open
          writer <- stand.cache(RemoteMode.Write, pullRequest)
          reader <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("pr-only")
          bytes  = utf8("pull-request")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          found <- reader.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == None,
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /pr/1/ac/${action.value}",
            s"GET /main/ac/${action.value}",
          ),
        )
    ,
    test("a read of pr/1 falls through to main on a miss"):
      ZIO.scoped:
        for
          stand  <- Stand.open
          writer <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          reader <- stand.cache(RemoteMode.Read, pullRequest)
          action = digest("shared")
          bytes  = utf8("from-main")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          found <- reader.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /main/ac/${action.value}",
            s"GET /pr/1/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a pull request hit does not read main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Write, pullRequest)
          action = digest("pr-hit")
          bytes  = utf8("own")
          output = Digest.ofBytes(bytes)
          _     <- cache.store(action, bytes)
          found <- cache.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /pr/1/ac/${action.value}",
            s"GET /pr/1/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a dangling pull request entry does not read main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, pullRequest)
          action  = digest("pr-dangling")
          missing = Digest.ofBytes(Chunk.single(7.toByte))
          kept    = utf8("on-main")
          output  = Digest.ofBytes(kept)
          _     <- stand.plantCas(output.value, kept)
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(output, kept.length))
          _     <- stand.plantAc("pr/1/ac", action.value, ActionResult.encode(missing, 1))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Dangling(action, missing)),
          calls == List(s"GET /pr/1/ac/${action.value}", s"GET /cas/${missing.value}"),
        )
    ,
    test("a pull request index that is not an action result does not read main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, pullRequest)
          action = digest("pr-index")
          bytes  = utf8("on-main")
          output = Digest.ofBytes(bytes)
          _     <- stand.plantCas(output.value, bytes)
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(output, bytes.length))
          _     <- stand.plantAc("pr/1/ac", action.value, utf8("nope"))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Index(action)),
          calls == List(s"GET /pr/1/ac/${action.value}"),
        )
    ,
    test("a pull request status other than a miss does not read main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, pullRequest)
          action = digest("pr-status")
          bytes  = utf8("on-main")
          output = Digest.ofBytes(bytes)
          _     <- stand.plantCas(output.value, bytes)
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(output, bytes.length))
          _     <- stand.fail(500)
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Status(500)),
          calls == List(s"GET /pr/1/ac/${action.value}"),
        )
    ,
    test("a dev write is invisible to main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          name  <- ZIO.fromEither(DevName.from("laptop"))
          dev = CacheNamespace.Dev(name)
          writer <- stand.cache(RemoteMode.Write, dev)
          reader <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("dev-only")
          bytes  = utf8("laptop")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          found <- reader.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == None,
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /dev/laptop/ac/${action.value}",
            s"GET /main/ac/${action.value}",
          ),
        )
    ,
    test("a dev read falls through to main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          name  <- ZIO.fromEither(DevName.from("laptop"))
          dev = CacheNamespace.Dev(name)
          writer <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          reader <- stand.cache(RemoteMode.Read, dev)
          action = digest("dev-shared")
          bytes  = utf8("from-main")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          found <- reader.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /main/ac/${action.value}",
            s"GET /dev/laptop/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a pull request purge drops its index and leaves the blob main names"):
      ZIO.scoped:
        for
          stand <- Stand.open
          main  <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          pr    <- stand.cache(RemoteMode.Write, pullRequest)
          action = digest("shared-purge")
          bytes  = utf8("shared")
          output = Digest.ofBytes(bytes)
          _     <- main.store(action, bytes)
          _     <- pr.store(action, bytes)
          _     <- pr.purge
          found <- main.find(action)
          again <- pr.find(action)
          calls <- stand.calls
          kept  <- stand.holdsBlob(output.value)
        yield assertTrue(
          found == Some(bytes),
          again == Some(bytes),
          kept,
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /main/ac/${action.value}",
            s"PUT /cas/${output.value}",
            s"PUT /pr/1/ac/${action.value}",
            "DELETE /pr/1/ac",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
            s"GET /pr/1/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a pull request purge leaves a blob no other index names"):
      ZIO.scoped:
        for
          stand <- Stand.open
          pr    <- stand.cache(RemoteMode.Write, pullRequest)
          action = digest("orphan")
          bytes  = utf8("only-pr")
          output = Digest.ofBytes(bytes)
          _     <- pr.store(action, bytes)
          _     <- pr.purge
          found <- pr.find(action)
          calls <- stand.calls
          kept  <- stand.holdsBlob(output.value)
        yield assertTrue(
          found == None,
          kept,
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /pr/1/ac/${action.value}",
            "DELETE /pr/1/ac",
            s"GET /pr/1/ac/${action.value}",
            s"GET /main/ac/${action.value}",
          ),
        )
    ,
    test("purging pr/1 leaves pr/11"):
      ZIO.scoped:
        for
          stand <- Stand.open
          one   <- stand.cache(RemoteMode.Write, pullRequest)
          other <- stand.cache(RemoteMode.Write, CacheNamespace.PullRequest(PullRequestNumber("11")))
          first  = digest("one")
          second = digest("eleven")
          left   = utf8("pr-1")
          right  = utf8("pr-11")
          _     <- one.store(first, left)
          _     <- other.store(second, right)
          _     <- one.purge
          kept  <- other.find(second)
          gone  <- one.find(first)
          calls <- stand.calls
        yield assertTrue(
          kept == Some(right),
          gone == None,
          calls == List(
            s"PUT /cas/${Digest.ofBytes(left).value}",
            s"PUT /pr/1/ac/${first.value}",
            s"PUT /cas/${Digest.ofBytes(right).value}",
            s"PUT /pr/11/ac/${second.value}",
            "DELETE /pr/1/ac",
            s"GET /pr/11/ac/${second.value}",
            s"GET /cas/${Digest.ofBytes(right).value}",
            s"GET /pr/1/ac/${first.value}",
            s"GET /main/ac/${first.value}",
          ),
        )
    ,
    test("a main purge leaves the pull request index"):
      ZIO.scoped:
        for
          stand <- Stand.open
          main  <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          pr    <- stand.cache(RemoteMode.Write, pullRequest)
          mainAction = digest("main-purge")
          prAction   = digest("pr-kept")
          mainBytes  = utf8("from-main-purge")
          prBytes    = utf8("from-pr-kept")
          _     <- pr.store(prAction, prBytes)
          _     <- main.store(mainAction, mainBytes)
          _     <- main.purge
          kept  <- pr.find(prAction)
          gone  <- main.find(mainAction)
          calls <- stand.calls
        yield assertTrue(
          kept == Some(prBytes),
          gone == None,
          calls == List(
            s"PUT /cas/${Digest.ofBytes(prBytes).value}",
            s"PUT /pr/1/ac/${prAction.value}",
            s"PUT /cas/${Digest.ofBytes(mainBytes).value}",
            s"PUT /main/ac/${mainAction.value}",
            "DELETE /main/ac",
            s"GET /pr/1/ac/${prAction.value}",
            s"GET /cas/${Digest.ofBytes(prBytes).value}",
            s"GET /main/ac/${mainAction.value}",
          ),
        )
    ,
    test("a dev purge drops that index and leaves main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          name  <- ZIO.fromEither(DevName.from("laptop"))
          dev = CacheNamespace.Dev(name)
          writer <- stand.cache(RemoteMode.Write, dev)
          main   <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          action = digest("dev-purge")
          bytes  = utf8("laptop-purge")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          _     <- main.store(action, bytes)
          _     <- writer.purge
          found <- main.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /dev/laptop/ac/${action.value}",
            s"PUT /cas/${output.value}",
            s"PUT /main/ac/${action.value}",
            "DELETE /dev/laptop/ac",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("off and read refuse purge"):
      ZIO.scoped:
        for
          stand       <- Stand.open
          off         <- stand.cache(RemoteMode.Off, pullRequest)
          read        <- stand.cache(RemoteMode.Read, pullRequest)
          refusedOff  <- off.purge.either
          refusedRead <- read.purge.either
          calls       <- stand.calls
        yield assertTrue(
          refusedOff == Left(RemoteError.Off),
          refusedRead == Left(RemoteError.ReadOnly),
          calls.isEmpty,
        )
    ,
    test("a refused purge leaves the index"):
      ZIO.scoped:
        for
          stand <- Stand.open
          pr    <- stand.cache(RemoteMode.Write, pullRequest)
          action = digest("purge-status")
          bytes  = utf8("kept")
          output = Digest.ofBytes(bytes)
          _       <- pr.store(action, bytes)
          _       <- stand.fail(500)
          refused <- pr.purge.either
          found   <- pr.find(action)
          calls   <- stand.calls
        yield assertTrue(
          refused == Left(RemoteError.Status(500)),
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /pr/1/ac/${action.value}",
            "DELETE /pr/1/ac",
            s"GET /pr/1/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("rebuild does not request the index list"):
      ZIO.scoped:
        for
          stand <- Stand.open
          pr    <- stand.cache(RemoteMode.Write, pullRequest)
          action = digest("rebuild")
          bytes  = utf8("kept")
          _     <- pr.store(action, bytes)
          ticks <- Ref.make(0)
          copying = ticks.update(_ + 1) *> stand.copy(pullRequest, CacheNamespace.Main)
          _     <- IndexPromote.run(Promote.Rebuild, copying)
          hits  <- ticks.get
          main  <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          found <- main.find(action)
          calls <- stand.calls
        yield assertTrue(
          hits == 0,
          found == None,
          !calls.contains("GET /pr/1/ac"),
          !calls.exists(_.startsWith("PUT /main/ac/")),
        )
    ,
    test("copy-index writes the pull request action results onto main and does not write a blob"):
      ZIO.scoped:
        for
          stand  <- Stand.open
          pr     <- stand.cache(RemoteMode.Write, pullRequest)
          eleven <- stand.cache(RemoteMode.Write, CacheNamespace.PullRequest(PullRequestNumber("11")))
          left   = digest("left")
          right  = digest("right")
          other  = digest("eleven")
          bytesL = utf8("left-bytes")
          bytesR = utf8("right-bytes")
          bytesE = utf8("eleven-bytes")
          outL   = Digest.ofBytes(bytesL)
          outR   = Digest.ofBytes(bytesR)
          outE   = Digest.ofBytes(bytesE)
          _        <- pr.store(left, bytesL)
          _        <- pr.store(right, bytesR)
          _        <- eleven.store(other, bytesE)
          _        <- IndexPromote.run(Promote.CopyIndex, stand.copy(pullRequest, CacheNamespace.Main))
          main     <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          foundL   <- main.find(left)
          missed   <- main.find(other)
          again    <- pr.find(left)
          mainBody <- stand.indexBytes("main/ac", left.value)
          prBody   <- stand.indexBytes("pr/1/ac", left.value)
          calls    <- stand.calls
          ordered = List(left, right).sortBy(_.value)
          copied  = "GET /pr/1/ac" :: ordered.flatMap { action =>
            List(s"GET /pr/1/ac/${action.value}", s"PUT /main/ac/${action.value}")
          }
        yield assertTrue(
          foundL == Some(bytesL),
          missed == None,
          again == Some(bytesL),
          mainBody == prBody,
          mainBody == Some(ActionResult.encode(outL, bytesL.length)),
          !calls.contains(s"PUT /main/ac/${other.value}"),
          calls == List(
            s"PUT /cas/${outL.value}",
            s"PUT /pr/1/ac/${left.value}",
            s"PUT /cas/${outR.value}",
            s"PUT /pr/1/ac/${right.value}",
            s"PUT /cas/${outE.value}",
            s"PUT /pr/11/ac/${other.value}",
          ) ++ copied ++ List(
            s"GET /main/ac/${left.value}",
            s"GET /cas/${outL.value}",
            s"GET /main/ac/${other.value}",
            s"GET /pr/1/ac/${left.value}",
            s"GET /cas/${outL.value}",
          ),
        )
    ,
    test("an empty index list copies nothing"):
      ZIO.scoped:
        for
          stand <- Stand.open
          _     <- stand.copy(pullRequest, CacheNamespace.Main)
          calls <- stand.calls
        yield assertTrue(calls == List("GET /pr/1/ac"))
    ,
    test("a missing index list is a status, not an empty copy"):
      ZIO.scoped:
        for
          stand   <- Stand.open
          _       <- stand.fail(404)
          refused <- stand.copy(pullRequest, CacheNamespace.Main).either
          calls   <- stand.calls
        yield assertTrue(
          refused == Left(RemoteError.Status(404)),
          calls == List("GET /pr/1/ac"),
        )
    ,
    test("a list line that is not a digest fails and writes nothing"):
      ZIO.scoped:
        for
          stand   <- Stand.open
          _       <- stand.plantList("pr/1/ac", utf8("nope\n"))
          refused <- stand.copy(pullRequest, CacheNamespace.Main).either
          calls   <- stand.calls
        yield assertTrue(
          refused == Left(RemoteError.Catalog("nope")),
          calls == List("GET /pr/1/ac"),
        )
    ,
    test("an index body that is not an action result is not copied"):
      ZIO.scoped:
        for
          stand <- Stand.open
          action = digest("garbage")
          _       <- stand.plantAc("pr/1/ac", action.value, Chunk(1, 2, 3))
          refused <- stand.copy(pullRequest, CacheNamespace.Main).either
          copied  <- stand.indexBytes("main/ac", action.value)
          calls   <- stand.calls
        yield assertTrue(
          refused == Left(RemoteError.Index(action)),
          copied == None,
          calls == List(s"GET /pr/1/ac", s"GET /pr/1/ac/${action.value}"),
        ),
  )

  private def digest(label: String): Digest =
    Digest.ofBytes(utf8(label))

  private def utf8(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

  /** Lengths here stay under 128, so each protobuf length is one byte. */
  private def framed(hash: String, sizeSuffix: Array[Byte]): Chunk[Byte] =
    val hashBytes = hash.getBytes(StandardCharsets.US_ASCII)
    val digest    = Array(0x0a.toByte, hashBytes.length.toByte) ++ hashBytes ++ sizeSuffix
    Chunk.fromArray(Array(0x32.toByte, digest.length.toByte) ++ digest)

  private enum Slot:
    case Index(prefix: String)
    case Blob

    def line(method: String, hash: String): String = this match
      case Index(prefix) => s"$method /$prefix/$hash"
      case Blob          => s"$method /cas/$hash"
  end Slot

  private final case class Note(
      ac: Map[String, Chunk[Byte]],
      cas: Map[String, Chunk[Byte]],
      calls: List[String],
      fail: Option[Int],
      lists: Map[String, Chunk[Byte]],
  )

  private final class Stand(origin: Url, note: Ref[Note]):
    def cache(mode: RemoteMode, namespace: CacheNamespace): ZIO[Client, Nothing, RemoteCache] =
      HttpCache.make(origin, mode, namespace)

    def calls: UIO[List[String]] =
      note.get.map(_.calls)

    def plantAc(prefix: String, hash: String, bytes: Chunk[Byte]): UIO[Unit] =
      note.update(current => current.copy(ac = current.ac + (s"$prefix/$hash" -> bytes)))

    def plantCas(hash: String, bytes: Chunk[Byte]): UIO[Unit] =
      note.update(current => current.copy(cas = current.cas + (hash -> bytes)))

    def fail(code: Int): UIO[Unit] =
      note.update(_.copy(fail = Some(code)))

    def holdsBlob(hash: String): UIO[Boolean] =
      note.get.map(_.cas.contains(hash))

    def copy(from: CacheNamespace, to: CacheNamespace): ZIO[Client, RemoteError, Unit] =
      HttpCache.copy(origin, from, to)

    def plantList(prefix: String, bytes: Chunk[Byte]): UIO[Unit] =
      note.update(current => current.copy(lists = current.lists + (prefix -> bytes)))

    def indexBytes(prefix: String, hash: String): UIO[Option[Chunk[Byte]]] =
      note.get.map(_.ac.get(s"$prefix/$hash"))
  end Stand

  private object Stand:
    def open: ZIO[Scope, heddle.error.ServerError, Stand] =
      val http = Server.Config.default.copy(host = "127.0.0.1", port = Port(0))
      for
        note   <- Ref.make(Note(Map.empty, Map.empty, Nil, None, Map.empty))
        server <- Server.install(routes(note), http)
        port   <- server.port
      yield Stand(Url.parse(s"http://127.0.0.1:$port"), note)

    private def routes(note: Ref[Note]): Routes[Any, Nothing] =
      Routes(
        Method.GET / "main" / "ac" / string("hash") -> { hash => read(note, Slot.Index("main/ac"), hash) },
        Method.PUT / "main" / "ac" / string("hash") handle { (hash, request) =>
          write(note, Slot.Index("main/ac"), hash, request)
        },
        Method.GET / "pr" / string("number") / "ac" / string("hash") -> { (number, hash) =>
          read(note, Slot.Index(s"pr/$number/ac"), hash)
        },
        Method.PUT / "pr" / string("number") / "ac" / string("hash") handle { (number, hash, request) =>
          write(note, Slot.Index(s"pr/$number/ac"), hash, request)
        },
        Method.GET / "dev" / string("name") / "ac" / string("hash") -> { (name, hash) =>
          read(note, Slot.Index(s"dev/$name/ac"), hash)
        },
        Method.PUT / "dev" / string("name") / "ac" / string("hash") handle { (name, hash, request) =>
          write(note, Slot.Index(s"dev/$name/ac"), hash, request)
        },
        Method.GET / "cas" / string("hash") -> { hash => read(note, Slot.Blob, hash) },
        Method.PUT / "cas" / string("hash") handle { (hash, request) => write(note, Slot.Blob, hash, request) },
        Method.GET / "main" / "ac"                     -> { _ => catalog(note, "main/ac") },
        Method.GET / "pr" / string("number") / "ac"    -> { number => catalog(note, s"pr/$number/ac") },
        Method.GET / "dev" / string("name") / "ac"     -> { name => catalog(note, s"dev/$name/ac") },
        Method.DELETE / "main" / "ac"                  -> { _ => drop(note, "main/ac") },
        Method.DELETE / "pr" / string("number") / "ac" -> { number => drop(note, s"pr/$number/ac") },
        Method.DELETE / "dev" / string("name") / "ac"  -> { name => drop(note, s"dev/$name/ac") },
      )

    private def read(note: Ref[Note], slot: Slot, hash: String) =
      note.modify: current =>
        val line = slot.line("GET", hash)
        forced(current, line).getOrElse:
          val stored = slot match
            case Slot.Index(prefix) => current.ac.get(s"$prefix/$hash")
            case Slot.Blob          => current.cas.get(hash)
          val response = stored match
            case Some(bytes) => Response(Status.Ok, Body.fromBytes(bytes))
            case None        => Response.notFound()
          (response, current.copy(calls = current.calls :+ line))

    private def write(note: Ref[Note], slot: Slot, hash: String, request: Request) =
      request.body.collect.foldZIO(
        _ => ZIO.succeed(Response.badRequest("the upload had no body")),
        bytes =>
          note.modify: current =>
            val line = slot.line("PUT", hash)
            forced(current, line).getOrElse:
              val stored = slot match
                case Slot.Index(prefix) => current.copy(ac = current.ac + (s"$prefix/$hash" -> bytes))
                case Slot.Blob          => current.copy(cas = current.cas + (hash -> bytes))
              val status = slot match
                case Slot.Blob     => Status.Ok
                case Slot.Index(_) => Status.NoContent
              (Response.empty(status), stored.copy(calls = stored.calls :+ line)),
      )

    private def catalog(note: Ref[Note], prefix: String) =
      note.modify: current =>
        val line = s"GET /$prefix"
        forced(current, line).getOrElse:
          val body = current.lists.get(prefix).getOrElse(rendered(current, prefix))
          (Response(Status.Ok, Body.fromBytes(body)), current.copy(calls = current.calls :+ line))

    private def rendered(current: Note, prefix: String): Chunk[Byte] =
      val hashes = current.ac.keys
        .collect {
          case key if key.startsWith(s"$prefix/") => key.drop(prefix.length + 1)
        }
        .toList
        .sorted
      val text = if hashes.isEmpty then "" else hashes.mkString("\n") + "\n"
      Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

    private def drop(note: Ref[Note], prefix: String) =
      note.modify: current =>
        val line = s"DELETE /$prefix"
        forced(current, line).getOrElse:
          val kept = current.ac.filterNot((key, _) => key.startsWith(s"$prefix/"))
          (Response.empty(Status.NoContent), current.copy(ac = kept, calls = current.calls :+ line))

    private def forced(current: Note, line: String): Option[(Response, Note)] =
      current.fail.map: code =>
        (Response.empty(Status(code, "forced")), current.copy(calls = current.calls :+ line, fail = None))
  end Stand

  def spec = suite("http remote cache")(
    encoded,
    prefixes,
    live.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      ZLayer.succeed(Client.Config.default) >>> Client.layer
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(1.minute),
  )
end HttpCacheSpec
