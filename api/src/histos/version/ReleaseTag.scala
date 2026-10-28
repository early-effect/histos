package histos.version

/** One catalog ship is `v<version>`. More than one is `<ship>/v<version>`. */
object ReleaseTag:
  def of(count: Int, ship: ShipName, version: String): String =
    if count > 1 then s"${ship.value}/v$version" else s"v$version"
