package histos

/** One scalac flag. The compiler text is produced here, not accepted from the caller. */
enum ScalacFlag:
  case EncodingUtf8

  /** The default inline budget cannot derive the wire sums. */
  case MaxInlines
  case Deprecation
  case Feature
  case UnusedAll
  case ImplicitConversions
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
          case ScalacFlag.EncodingUtf8        => "encoding-utf8"
          case ScalacFlag.MaxInlines          => "max-inlines"
          case ScalacFlag.Deprecation         => "deprecation"
          case ScalacFlag.Feature             => "feature"
          case ScalacFlag.UnusedAll           => "unused-all"
          case ScalacFlag.ImplicitConversions => "implicit-conversions"
        .mkString(",")

    def argv: List[String] =
      flags.flatMap:
        case ScalacFlag.EncodingUtf8        => List("-encoding", "UTF-8")
        case ScalacFlag.MaxInlines          => List("-Xmax-inlines:128")
        case ScalacFlag.Deprecation         => List("-deprecation")
        case ScalacFlag.Feature             => List("-feature")
        case ScalacFlag.UnusedAll           => List("-Wunused:all")
        case ScalacFlag.ImplicitConversions => List("-language:implicitConversions")
  end extension
end ScalacFlags
