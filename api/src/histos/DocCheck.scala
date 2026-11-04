package histos

/** Whether CI runs scaladoc beside the suite. Off unless a build asks. A local publish and a release still pack the
  * site, and a snapshot still omits it.
  */
enum DocCheck:
  case Off, On
