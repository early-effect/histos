package histos

/** `scala"3.9.0"`, `dep"dev.zio::zio:2.1.26"`, and `ship"1.4.2"`. A bad literal fails compilation of the build file. */
extension (inline sc: StringContext)
  inline def scala(inline args: Any*): ScalaVersion             = ${ LiteralMacro.scalaVersion('sc, 'args) }
  inline def dep(inline args: Any*): Dep                        = ${ LiteralMacro.dep('sc, 'args) }
  inline def ship(inline args: Any*): histos.version.ShipNumber = ${ LiteralMacro.shipNumber('sc, 'args) }

/** A catalog row. The number is the next release. Members are the modules that publish it. */
inline def ship(name: String, number: histos.version.ShipNumber)(member: ProjectDef, members: ProjectDef*): ShipDef =
  ${ BuilderMacro.ship('name, 'number, 'member, 'members) }

/** One or more catalog rows. A build that calls neither `ship` nor `ships` has an empty catalog. */
def ships(first: ShipDef, rest: ShipDef*): CatalogSpec =
  CatalogSpec(first, rest*)

