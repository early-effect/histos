package histos.workflow

import histos.{Module, NativeLibrary, Platform}

/** System packages a native job installs when a module links OpenSSL. */
object NativeCi:
  val packages: List[String] =
    List("clang", "libstdc++-12-dev", "libgc-dev", "libunwind-dev", "libssl-dev")

  def openssl(modules: List[Module]): Boolean =
    modules.exists: module =>
      module.platforms.all.contains(Platform.Native) &&
        (module.libraries.contains(NativeLibrary.Ssl) || module.libraries.contains(NativeLibrary.Crypto))
end NativeCi
