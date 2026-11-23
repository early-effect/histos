package histos.splice

/** A mapped library: vendor file, CDN coordinate, WebJar, or GitHub tag tarball. */
enum SpliceLib derives CanEqual:
  def specifier: String = this match
    case f: File   => f.spec
    case c: Cdn    => c.spec
    case w: WebJar => w.spec
    case g: GitHub => g.spec

  case File(
      spec: String,
      file: java.io.File,
  )
  case Cdn(
      spec: String,
      name: String,
      version: String,
      path: String,
      sha256: Option[String],
  )
  case WebJar(
      spec: String,
      organization: String,
      name: String,
      version: String,
      path: String,
  )
  case GitHub(
      spec: String,
      repository: String,
      tag: String,
      path: String,
      sha256: Option[String],
  )
end SpliceLib

object SpliceLib:
  extension (c: Cdn) def sha256(hex: String): Cdn       = c.copy(sha256 = Some(hex))
  extension (g: GitHub) def sha256(hex: String): GitHub = g.copy(sha256 = Some(hex))
