package histos.publish

import histos.{PairError, PairProof}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Ref, ZIO}
import zio.test.*

/** A broken handshake refuses the upload. Nothing here calls Central, GitHub, or a container runtime. */
object PairProofLawSpec extends ZIOSpecDefault:
  private val wanted = "histos 0.3.0"
  private val got    = "histos stale"

  def spec = suite("pair proof laws")(
    test("a broken handshake refuses the upload and a proved pair delivers"):
      for
        delivered <- Ref.make(false)
        refused   <- ToolchainPublish
          .commit(DistPlatform.MacosAarch64, ZIO.fail(PairError.Handshake(wanted, got)), delivered.set(true))
          .either
        touched  <- delivered.get
        _        <- delivered.set(false)
        accepted <- ToolchainPublish.commit(DistPlatform.LinuxX86_64, ZIO.unit, delivered.set(true))
        ran      <- delivered.get
      yield assertTrue(
        refused == Left(
          ToolchainPublishError.Proof(DistPlatform.MacosAarch64, PairError.Handshake(wanted, got))
        ),
        !touched,
        accepted == (),
        ran,
        PairError.Handshake(wanted, got).message ==
          s"toolchain handshake wanted $wanted and the client reported $got",
      )
    ,
    test("a version line that is not the packed one does not run graph"):
      for
        ran      <- Ref.make(false)
        refused  <- PairProof.judge(wanted, got, ran.set(true)).either
        touched  <- ran.get
        _        <- ran.set(false)
        accepted <- PairProof.judge(wanted, wanted, ran.set(true))
        again    <- ran.get
      yield assertTrue(
        refused == Left(PairError.Handshake(wanted, got)),
        !touched,
        accepted == (),
        again,
      )
    ,
    test("publish local lays the toolchain under the user home and points the checkout at that id"):
      ZIO.scoped:
        for
          root <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-toolchain")).orDie)(dir =>
            ZIO.attemptBlocking(delete(dir)).orDie
          )
          home   = root.resolve("home")
          repo   = root.resolve("repo")
          jar    = root.resolve("histos.jar")
          client = root.resolve("histos-client")
          id     = "0.3.0-abc"
          placed = DistStock.Placed(id, DistPlatform.MacosAarch64, jar, client, s"""{"version":"$id"}""")
          _ <- ZIO.attemptBlocking {
            Files.writeString(jar, "jar")
            Files.writeString(client, "client")
          }.orDie
          _       <- DistStock.install(home, repo, placed)
          pointer <- ZIO.attemptBlocking(Files.readString(repo.resolve(".histos").resolve("toolchain"))).orDie
          dir = home.resolve(".histos").resolve("toolchains").resolve(id)
          jarThere    <- ZIO.attemptBlocking(Files.isRegularFile(dir.resolve("histos.jar"))).orDie
          clientThere <- ZIO.attemptBlocking(Files.isRegularFile(dir.resolve("histos-client"))).orDie
          manifest    <- ZIO.attemptBlocking(Files.readString(dir.resolve("manifest.json"))).orDie
          bad         <- DistStock.install(home, repo, placed.copy(id = "a/b")).either
        yield assertTrue(
          pointer == s"$id\n",
          jarThere,
          clientThere,
          manifest == s"""{"version":"$id"}""",
          bad.isLeft,
        ),
  )

  private def delete(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
end PairProofLawSpec
