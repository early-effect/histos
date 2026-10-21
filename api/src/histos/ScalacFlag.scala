package histos

/** One scalac flag. The compiler text is produced here, not accepted from the caller. */
enum ScalacFlag:
  case EncodingUtf8

opaque type ScalacFlags = List[ScalacFlag]

object ScalacFlags:
  val empty: ScalacFlags = Nil

  /** One call replaces the module's list. It does not append. */
  def of(flag: ScalacFlag, flags: ScalacFlag*): ScalacFlags = flag :: flags.toList

  extension (flags: ScalacFlags)
    def canonical: String =
      flags.map:
        case ScalacFlag.EncodingUtf8 => "encoding-utf8"
      .mkString(",")

    def argv: List[String] =
      flags.flatMap:
        case ScalacFlag.EncodingUtf8 => List("-encoding", "UTF-8")
