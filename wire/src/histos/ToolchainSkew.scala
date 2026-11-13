package histos

/** One warning when a local toolchain was built from another commit, or from a dirty tree. The command still runs. */
object ToolchainSkew:
  def line(built: String, dirty: Boolean, checkout: String): String =
    val from = if dirty then s"$built (dirty)" else built
    s"local toolchain built from $from; this checkout is $checkout"

  /** None when this jar matches the checkout: the same commit, and it was not dirty. */
  def warn(identity: ToolchainIdentity, checkout: String): Option[String] =
    if identity.dirty || identity.commit != checkout then Some(line(identity.commit, identity.dirty, checkout))
    else None
end ToolchainSkew
