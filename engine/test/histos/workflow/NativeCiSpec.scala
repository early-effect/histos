package histos.workflow

import histos.*

import zio.test.*

object NativeCiSpec extends ZIOSpecDefault:
  def spec = suite("native CI packages")(
    test("openssl is installed when a native module links it") {
      assertTrue(NativeCi.openssl(List(native(Set(NativeLibrary.Ssl)))))
    },
    test("a JVM module does not ask for openssl") {
      val jvm = native(Set(NativeLibrary.Ssl)).copy(platforms = Platforms(Platform.Jvm, Nil), libraries = Set.empty)
      assertTrue(!NativeCi.openssl(List(jvm)))
    },
    test("the package list includes the linker and libssl") {
      assertTrue(NativeCi.packages.contains("libssl-dev"), NativeCi.packages.contains("clang"))
    },
  )

  private def native(libraries: Set[NativeLibrary]): Module =
    Module(
      ProjectName("core"),
      Platforms(Platform.Native, Nil),
      ScalaVersions(scala"3.9.0", Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
      libraries = libraries,
    )
end NativeCiSpec
