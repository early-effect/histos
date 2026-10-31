package histos

/** One scalac flag. The compiler text is produced here, not accepted from the caller. */
enum ScalacFlag:
  case EncodingUtf8

  /** The default inline budget cannot derive the wire sums. */
  case MaxInlines
end ScalacFlag

opaque type ScalacFlags = List[ScalacFlag]

object ScalacFlags:
  val empty: ScalacFlags = Nil

  /** One call replaces the module's list. It does not append. */
  def of(flag: ScalacFlag, flags: ScalacFlag*): ScalacFlags = flag :: flags.toList

  extension (flags: ScalacFlags)
    def canonical: String =
      flags
        .map:
          case ScalacFlag.EncodingUtf8 => "encoding-utf8"
          case ScalacFlag.MaxInlines   => "max-inlines"
        .mkString(",")

    def argv: List[String] =
      flags.flatMap:
        case ScalacFlag.EncodingUtf8 => List("-encoding", "UTF-8")
        case ScalacFlag.MaxInlines   => List("-Xmax-inlines:128")
  end extension
end ScalacFlags
