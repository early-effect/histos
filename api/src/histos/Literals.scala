package histos

/** `scala"3.9.0"`, `dep"dev.zio::zio:2.1.26"`, `ship"1.4.2"`, `manual"1.2.3"`, `organization"com.example"`, `artifact"histos-api"`, and `path"example/todo"`. A bad literal fails compilation of the build file. */
extension (inline sc: StringContext)
  inline def scala(inline args: Any*): ScalaVersion                  = ${ LiteralMacro.scalaVersion('sc, 'args) }
  inline def dep(inline args: Any*): Dep                             = ${ LiteralMacro.dep('sc, 'args) }
  inline def ship(inline args: Any*): histos.version.ShipNumber      = ${ LiteralMacro.shipNumber('sc, 'args) }
  inline def manual(inline args: Any*): histos.version.ManualVersion = ${ LiteralMacro.declaredVersion('sc, 'args) }
  inline def organization(inline args: Any*): Organization           = ${ LiteralMacro.organization('sc, 'args) }
  inline def artifact(inline args: Any*): ArtifactBase               = ${ LiteralMacro.artifact('sc, 'args) }
  inline def maven(inline args: Any*): Resolver                      = ${ LiteralMacro.maven('sc, 'args) }
  inline def path(inline args: Any*): ProjectPath                    = ${ LiteralMacro.projectPath('sc, 'args) }

/** An early-effect catalog row. The number is the next release. Members are the modules that publish it. */
inline def ship(name: String, number: histos.version.ShipNumber)(member: ProjectDef, members: ProjectDef*): ShipDef =
  ${ BuilderMacro.ship('name, 'number, 'member, 'members) }

/** A ship whose version is the one written here. Histos does not derive it. */
inline def manual(name: String, version: histos.version.ManualVersion)(member: ProjectDef, members: ProjectDef*): ShipDef =
  ${ BuilderMacro.fixed('name, 'version, 'member, 'members) }

/** One or more catalog rows. A build that calls neither `ship` nor `ships` has an empty catalog. */
def ships(first: ShipDef, rest: ShipDef*): CatalogSpec =
  CatalogSpec(first, rest*)

