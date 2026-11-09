package histos.workflow

/** SHA-256 of an LLVM release archive, as GitHub records it on the release asset. A name that is not here fails the
  * render. The workflow then checks the downloaded bytes against this value before it extracts them.
  */
object LlvmDigest:
  def of(archive: String): Either[CiError, String] =
    known.get(archive).toRight(CiError.LlvmChecksum(archive))

  private val known: Map[String, String] = Map(
    "LLVM-21.1.8-Linux-X64.tar.xz" ->
      "b3b7f2801d15d50736acea3c73982994d025b01c2f035b91ae3b49d1b575732b",
    "LLVM-21.1.8-macOS-ARM64.tar.xz" ->
      "b95bdd32a33a81ee4d40363aaeb26728a26783fcef26a4d80f65457433ea4669",
    "LLVM-20.1.8-Linux-X64.tar.xz" ->
      "1ead36b3dfcb774b57be530df42bec70ab2d239fbce9889447c7a29a4ddc1ae6",
  )
end LlvmDigest
