package histos

/** How a generated workflow installs Histos. The default is [[CiSetup.Remote]]. */
enum CiSetup:

  /** `early-effect/histos/.github/actions/setup` at the rendering ship's full commit. That action runs `install/histos`
    * from the same ref.
    */
  case Remote

  /** Pinned `actions/cache` steps, then `./histos`. No external Histos action. */
  case Wrapper
end CiSetup
