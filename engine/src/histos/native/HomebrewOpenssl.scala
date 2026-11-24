package histos.native

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Homebrew opt prefixes, not Cellar revisions. Versioned fallbacks have a stable order, not a latest-version policy.
  */
object HomebrewOpenssl:
  def discover(os: String, opt: Path = Path.of("/opt/homebrew/opt")): IO[NativeError, Option[Path]] =
    if !NativeFlags.mac(os) then ZIO.none
    else
      val read = ZIO.scoped:
        ZIO
          .acquireRelease(ZIO.attemptBlocking(Files.list(opt)))(stream => ZIO.attemptBlocking(stream.close()).orDie)
          .flatMap: stream =>
            ZIO.attemptBlocking:
              select(stream.iterator.asScala.toList.filter: prefix =>
                Files.isDirectory(prefix.resolve("include")) && Files.isDirectory(prefix.resolve("lib")))
      ZIO
        .ifZIO(ZIO.attemptBlocking(Files.isDirectory(opt)))(read, ZIO.none)
        .mapError(_ => NativeError.Link(LinkError.Linker(s"could not discover Homebrew OpenSSL in $opt")))

  private[native] def select(installed: List[Path]): Option[Path] =
    installed
      .flatMap(prefix => Option(prefix.getFileName).map(name => (prefix, name.toString)))
      .filter: (_, name) =>
        name == "openssl" || name.matches("openssl@[0-9]+(?:\\.[0-9]+)*")
      .sortBy: (_, name) =>
        val rank = if name == "openssl@3" then 0 else if name == "openssl" then 1 else 2
        (rank, name)
      .headOption
      .map(_._1)
end HomebrewOpenssl
