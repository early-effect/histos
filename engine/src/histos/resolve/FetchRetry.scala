package histos.resolve

import coursier.cache.ArtifactError

import zio.{Schedule, ZIO, durationInt}

/** A fetch whose every failed download failed in transit runs again: three more times, one, two, then four seconds
  * apart. A missing, refused, corrupt, or rejected artifact fails at once. Coursier's own retry backs off in
  * milliseconds, which a runner's network blip outlasts.
  */
object FetchRetry:
  val schedule: Schedule[Any, ResolveError, Any] =
    Schedule.recurWhile[ResolveError](_.transient) && Schedule.exponential(1.second) && Schedule.recurs(3)

  def apply[R, A](fetch: ZIO[R, ResolveError, A]): ZIO[R, ResolveError, A] =
    fetch.retry(schedule)

  /** The fault for one Coursier artifact error. */
  def fault(error: ArtifactError): DownloadFault =
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
end FetchRetry
