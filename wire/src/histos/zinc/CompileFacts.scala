package histos.zinc

/** What one compile decided. Absent counts are unknown, not zero. */
final case class CompileFacts(
    changed: Boolean,
    sources: Option[SourceCount],
    classesWritten: Option[Int],
)
