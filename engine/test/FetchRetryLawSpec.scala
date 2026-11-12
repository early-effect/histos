package histos.resolve

import coursier.cache.ArtifactError

import java.io.File

import zio.*
import zio.test.*

/** A fetch whose every download failed in transit runs again on a backoff. Anything else fails on the first try. */
object FetchRetryLawSpec extends ZIOSpecDefault:
  private val url = "https://repo1.maven.org/maven2/dev/zio/zio_3/2.1.27/zio_3-2.1.27.jar"

  private def failing(fault: DownloadFault): ResolveError =
    ResolveError.Download(::(ArtifactMiss(url, fault, "download error: read timed out"), Nil))

  def spec = suite("fetch retry laws")(
    test("each Coursier artifact error has one fault, and only a fault in transit is transient"):
      val faults = List(
        new ArtifactError.NotFound(url)                                -> DownloadFault.Missing,
        new ArtifactError.FileTooOldOrNotFound(url)                    -> DownloadFault.Missing,
        new ArtifactError.Forbidden(url)                               -> DownloadFault.Refused,
        new ArtifactError.Unauthorized(url, None)                      -> DownloadFault.Refused,
        new ArtifactError.WrongChecksum("SHA-1", "aa", "bb", url, url) -> DownloadFault.Corrupt,
        new ArtifactError.WrongLength(1L, 2L, url)                     -> DownloadFault.Corrupt,
        new ArtifactError.DownloadError("read timed out", None)        -> DownloadFault.Transient,
        new ArtifactError.RequestTimeout(url, None)                    -> DownloadFault.Transient,
        new ArtifactError.InternalServerError(url, 502, None)          -> DownloadFault.Transient,
        new ArtifactError.RetryableHttpError(url, 429, None)           -> DownloadFault.Transient,
        new ArtifactError.Locked(new File(url))                        -> DownloadFault.Transient,
        new ArtifactError.ForbiddenChangingArtifact(url)               -> DownloadFault.Rejected,
      )
      assertTrue(
        faults.forall((error, fault) => FetchRetry.fault(error) == fault),
        failing(DownloadFault.Transient).transient,
        DownloadFault.values.filterNot(_ == DownloadFault.Transient).forall(fault => !failing(fault).transient),
        !ResolveError.Repository(RepositoryIo.Fetch).transient,
      )
    ,
    test("a download that failed in transit twice succeeds on the third try, one then two seconds later"):
      for
        tries   <- Ref.make(0)
        fetched <- FetchRetry(
          tries.updateAndGet(_ + 1).flatMap(n => ZIO.when(n < 3)(ZIO.fail(failing(DownloadFault.Transient))))
        ).fork
        _      <- TestClock.adjust(999.millis)
        early  <- tries.get
        _      <- TestClock.adjust(1.millis)
        second <- tries.get
        _      <- TestClock.adjust(2.seconds)
        done   <- fetched.join
        total  <- tries.get
      yield assertTrue(early == 1, second == 2, total == 3, done.isEmpty)
    ,
    test("a transient failure that never clears stops after three retries with that failure"):
      for
        tries  <- Ref.make(0)
        fetch  <- FetchRetry(tries.update(_ + 1) *> ZIO.fail(failing(DownloadFault.Transient))).flip.fork
        _      <- TestClock.adjust(7.seconds)
        failed <- fetch.join
        total  <- tries.get
      yield assertTrue(total == 4, failed == failing(DownloadFault.Transient))
    ,
    test("a missing artifact fails on the first try and does not wait"):
      for
        tries  <- Ref.make(0)
        failed <- FetchRetry(tries.update(_ + 1) *> ZIO.fail(failing(DownloadFault.Missing))).flip
        total  <- tries.get
      yield assertTrue(total == 1, failed == failing(DownloadFault.Missing))
    ,
    test("the message names each artifact and the repository's reason"):
      assertTrue(
        failing(DownloadFault.Transient).message ==
          s"could not download:\n$url: download error: read timed out"
      ),
  )
end FetchRetryLawSpec
