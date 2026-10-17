package histos

/** Starts a compiling project. One platform and one Scala version are required, so omitting either does not compile. */
inline def module(name: String, platform: Platform, platforms: Platform*)(
    version: ScalaVersion,
    versions: ScalaVersion*,
): ModuleDef =
  ${ BuilderMacro.module('name, 'platform, 'platforms, 'version, 'versions) }

/** A root that does not compile. It aggregates the projects it is given. */
inline def aggregate(name: String, project: ProjectDef, projects: ProjectDef*): AggregateDef =
  ${ BuilderMacro.aggregate('name, 'project, 'projects) }

