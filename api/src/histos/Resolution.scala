package histos

/** Where a build's jars come from. [[Resolution.Solve]] resolves from the build and remembers the result until the
  * build reloads. [[Resolution.Lock]] reads `dependencies.lock` and does not resolve. Omission is [[Resolution.Solve]].
  */
enum Resolution:
  case Solve
  case Lock
