package histos.native

import histos.NativeLibrary

import java.nio.file.Path

/** The linker flags a set of [[NativeLibrary]] becomes. The openssl prefix is an argument, so a test does not read the
  * machine.
  */
object NativeFlags:
  /** Compile flags, then link flags. An empty library set is empty flags. */
  def of(libraries: Set[NativeLibrary], os: String, opensslHome: Option[Path]): (List[String], List[String]) =
    val brew    = opensslHome.filter(_ => mac(os) && usesOpenssl(libraries))
    val compile = brew.map(home => s"-I${home.resolve("include")}").toList
    val link    =
      brew.toList.flatMap(home => List(s"-L${home.resolve("lib")}")) ++
        libraries.toList.sortBy(_.token).flatMap(flag)
    (compile, link)

  private[native] def usesOpenssl(libraries: Set[NativeLibrary]): Boolean =
    libraries.contains(NativeLibrary.Ssl) || libraries.contains(NativeLibrary.Crypto)

  private[native] def mac(os: String): Boolean =
    val name = os.toLowerCase
    name.contains("mac") || name.contains("darwin")

  private def flag(library: NativeLibrary): List[String] =
    library match
      case NativeLibrary.Ssl    => List("-lssl")
      case NativeLibrary.Crypto => List("-lcrypto")
      case NativeLibrary.Gc     => List("-lgc")
      case NativeLibrary.Unwind => List("-lunwind")
      case NativeLibrary.Stdcxx => List("-lstdc++")
end NativeFlags
