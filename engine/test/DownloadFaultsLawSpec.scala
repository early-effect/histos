package histos.resolve

import coursier.cache.ArtifactError

import java.io.File

import zio.test.*

/** A failed download names the artifact, its fault, and the repository's reason. */
object DownloadFaultsLawSpec extends ZIOSpecDefault:
  private val url = "https://repo1.maven.org/maven2/dev/zio/zio_3/2.1.27/zio_3-2.1.27.jar"

  private def failing(fault: DownloadFault): ResolveError =
    ResolveError.Download(::(ArtifactMiss(url, fault, "download error: read timed out"), Nil))

  def spec = suite("download fault laws")(
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
        faults.forall((error, fault) => DownloadFaults.of(error) == fault),
        failing(DownloadFault.Transient).transient,
        DownloadFault.values.filterNot(_ == DownloadFault.Transient).forall(fault => !failing(fault).transient),
        !ResolveError.Repository(RepositoryIo.Fetch).transient,
      )
    ,
    test("the message names each artifact and the repository's reason"):
      assertTrue(
        failing(DownloadFault.Transient).message ==
          s"could not download:\n$url: download error: read timed out"
      ),
  )
end DownloadFaultsLawSpec
