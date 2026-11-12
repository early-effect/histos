package histos.resolve

enum RepositoryIo:
  case Fetch, Metadata

/** Why one artifact did not download. `Transient` is a fault in transit that the same request can clear. */
enum DownloadFault:
  case Missing
  case Refused
  case Corrupt
  case Transient
  case Rejected

/** One artifact that did not download: its url, the fault, and the repository's own description. */
final case class ArtifactMiss(url: String, fault: DownloadFault, detail: String)
