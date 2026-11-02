package histos

/** Where a build's jars come from. [[Solve]] resolves from the build and remembers the result until the build reloads.
  * [[Lock]] reads `dependencies.lock` and does not resolve. Omission is [[Solve]].
  */
enum Resolution:
  case Solve
  case Lock
