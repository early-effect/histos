package histos.splice

/** Post-link pass on the printed spliced file. */
enum Minify derives CanEqual:
  case None
  case Esbuild
  case Closure
