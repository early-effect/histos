package histos.splice

/** What `process.env.NODE_ENV` reads as in the bundled libraries, which many choose their build by. */
private[splice] enum NodeEnv(val value: String) derives CanEqual:
  case Development extends NodeEnv("development")
  case Production  extends NodeEnv("production")

private[splice] object NodeEnv:
  /** Minified builds are production builds. */
  def of(minify: Minify): NodeEnv = minify match
    case Minify.None                     => Development
    case Minify.Esbuild | Minify.Closure => Production
