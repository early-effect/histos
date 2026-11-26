package histos

/** The skew between the running jar and this checkout. Doctor reports [[line]]. A command does not print it.
  *
  * A clean commit mismatch is still a rebuild: the JVM command packs this checkout and runs again on the jar it prints.
  * A dirty tree has no commit to pack.
  */
object ToolchainSkew:
  /** The two commits, and whether the pack was dirty. No remedy: the caller adds what it will do. */
  def fact(built: String, dirty: Boolean, checkout: String): String =
    val from = if dirty then s"$built (dirty)" else built
    s"toolchain built from $from; this checkout is $checkout"

  def line(built: String, dirty: Boolean, checkout: String): String =
    val fact = ToolchainSkew.fact(built, dirty, checkout)
    if dirty then s"$fact. commit, then the next command packs this checkout"
    else s"$fact. packing this checkout"

  /** The toolchain id in a `HISTOS_PIN=<id>` line. Paint around the text does not hide it. A line without one is none.
    */
  def pin(line: String): Option[String] =
    val marker = "HISTOS_PIN="
    val at     = line.indexOf(marker)
    if at < 0 then None
    else
      val raw = line.drop(at + marker.length).takeWhile(char => !char.isWhitespace && char != '\u001b')
      if raw.nonEmpty then Some(raw) else None

  /** The commit the next pack has to match. A dirty tree has none. */
  def rebuild(identity: ToolchainIdentity, checkout: String): Option[String] =
    if !identity.dirty && identity.commit != checkout then Some(checkout) else None

  /** None when this jar matches the checkout: the same commit, and it was not dirty. */
  def warn(identity: ToolchainIdentity, checkout: String): Option[String] =
    if identity.dirty || identity.commit != checkout then Some(line(identity.commit, identity.dirty, checkout))
    else None
end ToolchainSkew
