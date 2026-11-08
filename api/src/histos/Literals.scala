package histos

/** `scala"3.9.0"`, `dep"dev.zio::zio:2.1.26"`, `ship"1.4.2"`, `organization"com.example"`, `artifact"histos-api"`,
  * `path"example/todo"`, `asset"dist/histos.jar"`, `pom"..."`, `url"https://..."`, `license"Apache-2.0"`,
  * `developer"early-effect"`, `connection"scm:git:..."`, `sbt"2"`, and `promote"rebuild"`. A bad literal fails
  * compilation of the build file. A catalog row is `Ship("histos").next(0, 2, 2)(api, app)`. `ship"..."` is the ship
  * number alone.
  */
extension (inline sc: StringContext)
  inline def scala(inline args: String*): ScalaVersion             = ${ LiteralMacro.scalaVersion('sc, 'args) }
  inline def dep(inline args: String*): Dep                        = ${ LiteralMacro.dep('sc, 'args) }
  inline def ship(inline args: String*): histos.version.ShipNumber = ${ LiteralMacro.shipNumber('sc, 'args) }
  inline def organization(inline args: String*): Organization           = ${ LiteralMacro.organization('sc, 'args) }
  inline def artifact(inline args: String*): ArtifactBase               = ${ LiteralMacro.artifact('sc, 'args) }
  inline def maven(inline args: String*): Resolver                      = ${ LiteralMacro.maven('sc, 'args) }
  inline def path(inline args: String*): ProjectPath                    = ${ LiteralMacro.projectPath('sc, 'args) }
  inline def asset(inline args: String*): histos.plugin.RepoPath        = ${ LiteralMacro.asset('sc, 'args) }
  inline def pom(inline args: String*): PomText                         = ${ LiteralMacro.pomText('sc, 'args) }
  inline def url(inline args: String*): HttpUrl                         = ${ LiteralMacro.httpUrl('sc, 'args) }
  inline def license(inline args: String*): LicenseName                 = ${ LiteralMacro.licenseName('sc, 'args) }
  inline def developer(inline args: String*): DeveloperId               = ${ LiteralMacro.developerId('sc, 'args) }
  inline def connection(inline args: String*): ScmConnection            = ${ LiteralMacro.scmConnection('sc, 'args) }
  inline def sbt(inline args: String*): SbtSeries                       = ${ LiteralMacro.sbtSeries('sc, 'args) }
  inline def promote(inline args: String*): Promote                     = ${ LiteralMacro.promote('sc, 'args) }
  inline def ghsa(inline args: String*): histos.supply.AdvisoryId =
    histos.supply.Interp.ghsa(sc, args*)
  inline def reason(inline args: String*): histos.supply.Reason =
    histos.supply.Interp.reason(sc, args*)
  inline def date(inline args: String*): histos.supply.Date =
    histos.supply.Interp.date(sc, args*)

/** An early-effect catalog row. The number is the next release. Members are the modules that publish it. Prefer
  * `Ship(name).next(major, minor, patch)(member, ...)`.
  */
inline def ship(name: String, number: histos.version.ShipNumber)(member: ProjectDef, members: ProjectDef*): ShipDef =
  ${ BuilderMacro.ship('name, 'number, 'member, 'members) }

/** One or more catalog rows. A build that calls neither `ship` nor `ships` has an empty catalog. */
def ships(first: ShipDef, rest: ShipDef*): CatalogSpec =
  CatalogSpec(first, rest*)
