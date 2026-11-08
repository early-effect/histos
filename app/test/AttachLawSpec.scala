package histos.server

import histos.LawDir
import histos.Resolution

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

object AttachLawSpec extends ZIOSpecDefault:
  private val version = "histos fixture"

  def spec = suite("attach laws")(
    test("a record round-trips and a different version does not open"):
      for
        root <- ZIO.service[Path]
        repo <- LawDir.child(root, "record")
        socket      = repo.resolve("server.sock")
        fingerprint = Fingerprint.trusted("abc")
        _     <- Record.write(repo, version, fingerprint, socket)
        read  <- Record.read(repo)
        text  <- ZIO.attemptBlocking(Files.readString(Record.file(repo))).orDie
        open  <- Attach.decide(repo, version)
        other <- Attach.decide(repo, "other fixture")
      yield
        val absolute = socket.toAbsolutePath.normalize
        assertTrue(
          read == Some(Record.Written(version, fingerprint, absolute)),
          open == Decision.Open(absolute),
          other == Decision.DifferentVersion(absolute),
          text.contains("\"version\""),
          text.contains("\"fingerprint\""),
          text.contains("\"socket\""),
          text.contains(version),
        )
    ,
    test("a missing record, a corrupt record, and a directory are absent"):
      for
        root          <- ZIO.service[Path]
        missingRepo   <- LawDir.child(root, "missing")
        corruptRepo   <- LawDir.child(root, "corrupt")
        directoryRepo <- LawDir.child(root, "directory")
        _             <- ZIO.attemptBlocking {
          Files.createDirectories(Record.file(corruptRepo).getParent)
          Files.writeString(Record.file(corruptRepo), "nope")
          Files.createDirectories(Record.file(directoryRepo))
        }.orDie
        missing   <- Attach.decide(missingRepo, version)
        corrupt   <- Attach.decide(corruptRepo, version)
        directory <- Attach.decide(directoryRepo, version)
      yield assertTrue(
        missing == Decision.Absent,
        corrupt == Decision.Absent,
        directory == Decision.Absent,
      )
    ,
    test("a checksum the server did not record is a different driver"):
      val sha = "ab" * 32
      for
        root <- ZIO.service[Path]
        repo <- LawDir.child(root, "mark")
        socket      = repo.resolve("server.sock")
        fingerprint = Fingerprint.trusted("abc")
        _       <- Record.write(repo, version, fingerprint, socket)
        _       <- write(repo.resolve(".histos").resolve("assembly.sha256"), sha + "\n")
        drifted <- Attach.decide(repo, version)
        _       <- Record.write(repo, version, fingerprint, socket, Some(sha))
        matched <- Attach.decide(repo, version)
        _ <- ZIO.attemptBlocking {
          Files.writeString(
            Record.file(repo),
            s"""{"version":"$version","fingerprint":"abc","socket":"${socket.toAbsolutePath.normalize}"}""",
          )
        }.orDie
        legacy <- Record.read(repo)
        again  <- Attach.decide(repo, version)
      yield
        val absolute = socket.toAbsolutePath.normalize
        assertTrue(
          drifted == Decision.DifferentVersion(absolute),
          matched == Decision.Open(absolute),
          legacy.exists(_.assembly.isEmpty),
          again == Decision.DifferentVersion(absolute),
        )
    ,
    test("solve ignores the lock file and lock hashes it"):
      for
        root       <- ZIO.service[Path]
        repo       <- LawDir.child(root, "lock")
        solve0     <- Fingerprint.read(repo, Resolution.Solve)
        lock0      <- Fingerprint.read(repo, Resolution.Lock)
        _          <- write(repo.resolve("dependencies.lock"), "a")
        solveA     <- Fingerprint.read(repo, Resolution.Solve)
        lockA      <- Fingerprint.read(repo, Resolution.Lock)
        _          <- write(repo.resolve("dependencies.lock"), "b")
        solveB     <- Fingerprint.read(repo, Resolution.Solve)
        lockB      <- Fingerprint.read(repo, Resolution.Lock)
        solveRoots <- Fingerprint.roots(repo, Resolution.Solve)
        lockRoots  <- Fingerprint.roots(repo, Resolution.Lock)
      yield assertTrue(
        solve0 == solveA,
        solveA == solveB,
        lock0 != lockA,
        lockA != lockB,
        solve0 != lock0,
        solveRoots == List(repo.resolve("histos.scala")),
        lockRoots == List(repo.resolve("histos.scala"), repo.resolve("dependencies.lock")),
      )
    ,
    test("a path plugin is a root and its file bytes change the fingerprint"):
      for
        root <- ZIO.service[Path]
        repo <- LawDir.child(root, "plugin")
        dir  = repo.resolve("plugins").resolve("demo")
        note = dir.resolve("Note.scala")
        _ <- ZIO.attemptBlocking {
          Files.writeString(repo.resolve("histos.scala"), "plugin(path\"plugins/demo\")\n")
          Files.createDirectories(dir)
          Files.writeString(note, "one")
        }.orDie
        roots  <- Fingerprint.roots(repo, Resolution.Solve)
        first  <- Fingerprint.read(repo, Resolution.Solve)
        _      <- write(note, "two")
        second <- Fingerprint.read(repo, Resolution.Solve)
      yield assertTrue(roots.contains(dir), roots.contains(repo.resolve("histos.scala")), first != second)
    ,
    test("a header that does not parse adds no plugin root"):
      for
        root  <- ZIO.service[Path]
        plain <- LawDir.child(root, "plain")
        extra <- LawDir.child(root, "extra")
        _     <- ZIO.attemptBlocking {
          Files.writeString(plain.resolve("histos.scala"), "plugin(nope)\n")
          Files.writeString(extra.resolve("histos.scala"), "plugin(nope)\n")
          val dir = extra.resolve("plugins").resolve("demo")
          Files.createDirectories(dir)
          Files.writeString(dir.resolve("Note.scala"), "secret")
        }.orDie
        plainRoots <- Fingerprint.roots(plain, Resolution.Solve)
        extraRoots <- Fingerprint.roots(extra, Resolution.Solve)
        plainHash  <- Fingerprint.read(plain, Resolution.Solve)
        extraHash  <- Fingerprint.read(extra, Resolution.Solve)
      yield assertTrue(
        plainRoots == List(plain.resolve("histos.scala")),
        extraRoots == List(extra.resolve("histos.scala")),
        plainHash == extraHash,
      ),
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def write(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      Files.writeString(path, text)
      ()
    }.orDie
end AttachLawSpec
