package histos.resolve

import coursier.cache.ArtifactError

/** The [[DownloadFault]] for one Coursier artifact error. Coursier's downloader already retries a timeout, a socket or
  * DNS failure, and HTTP 408, 429, and 5xx with backoff, so a `Transient` fault here has outlasted those retries.
  */
object DownloadFaults:
  def of(error: ArtifactError): DownloadFault =
    error match
      case _: ArtifactError.NotFound | _: ArtifactError.FileTooOldOrNotFound          => DownloadFault.Missing
      case _: ArtifactError.Forbidden | _: ArtifactError.Unauthorized                 => DownloadFault.Refused
      case _: ArtifactError.ChecksumNotFound | _: ArtifactError.ChecksumErrors        => DownloadFault.Corrupt
      case _: ArtifactError.ChecksumFormatError | _: ArtifactError.WrongChecksum      => DownloadFault.Corrupt
      case _: ArtifactError.WrongLength                                               => DownloadFault.Corrupt
      case _: ArtifactError.DownloadError | _: ArtifactError.RequestTimeout           => DownloadFault.Transient
      case _: ArtifactError.InternalServerError | _: ArtifactError.RetryableHttpError => DownloadFault.Transient
      case _: ArtifactError.Recoverable                                               => DownloadFault.Transient
      case _                                                                          => DownloadFault.Rejected
