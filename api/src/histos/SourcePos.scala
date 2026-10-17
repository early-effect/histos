package histos

/** Where a builder call was written. Lines and columns are 1-based. */
final case class SourcePos(path: String, line: Int, column: Int)
