package histos.load

private[load] object ReservedPackage:
  def binary(name: String): Boolean =
    name == "histos" || name.startsWith("histos.") ||
      name == "zio" || name.startsWith("zio.") ||
      name == "scala" || name.startsWith("scala.")

  def path(name: String): Boolean =
    name.startsWith("histos/") || name.startsWith("zio/") || name.startsWith(
      "scala/"
    )
end ReservedPackage
