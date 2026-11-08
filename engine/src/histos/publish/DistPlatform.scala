package histos.publish

/** A platform the dist base carries a prebuilt client for. `token` is the manifest key, the file suffix, and the
  * launcher's platform name.
  */
enum DistPlatform(val token: String):
  case LinuxX86_64  extends DistPlatform("linux-x86_64")
  case MacosAarch64 extends DistPlatform("macos-aarch64")
