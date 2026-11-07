package histos.native

import histos.ClangMajor

object NativeSamples:
  val version: ClangVersion = ClangVersion.trusted("21.1.8", ClangMajor(21))
  val older: ClangVersion   = ClangVersion.trusted("20.1.0", ClangMajor(20))
