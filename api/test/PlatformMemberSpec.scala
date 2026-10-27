package outside

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.test.*

/** What a build file sees. The snippets are not in package histos. */
object PlatformMemberSpec extends ZIOSpecDefault:
  private inline def messages(inline code: String): List[String] =
    typeCheckErrors(code).map(_.message).toList

  private inline def missing(method: String, inline code: String): Boolean =
    messages(code) match
      case head :: _ => head.contains(s"$method is not a member")
      case Nil       => false

  private val members = suite("platform members")(
    test("a JVM module has no Js or Native setting"):
      assertTrue(
        missing(
          "jsVersion",
          """import histos.*
          module("api", Jvm)(scala"3.10.0").jsVersion(JsVersion("1.22.0"))
        """,
        ),
        missing(
          "jsEnv",
          """import histos.*
          module("api", Jvm)(scala"3.10.0").jsEnv(JsEnv.Node)
        """,
        ),
        missing(
          "jsModule",
          """import histos.*
          module("api", Jvm)(scala"3.10.0").jsModule(JsModuleKind.CommonJS)
        """,
        ),
        missing(
          "nativeVersion",
          """import histos.*
          module("api", Jvm)(scala"3.10.0").nativeVersion(NativeVersion("0.5.12"))
        """,
        ),
        missing(
          "clang",
          """import histos.*
          module("api", Jvm)(scala"3.10.0").clang(ClangMajor(21))
        """,
        ),
      )
    ,
    test("a Js module has the Js settings and not the Native ones"):
      val legal = messages("""import histos.*
        module("web", Platform.Js)(scala"3.10.0")
          .jsVersion(JsVersion("1.22.0"))
          .jsEnv(JsEnv.Jsdom)
          .jsModule(JsModuleKind.ESModule)
      """)
      assertTrue(
        legal.isEmpty,
        missing(
          "clang",
          """import histos.*
          module("web", Js)(scala"3.10.0").clang(ClangMajor(21))
        """,
        ),
        missing(
          "nativeVersion",
          """import histos.*
          module("web", Js)(scala"3.10.0").nativeVersion(NativeVersion("0.5.12"))
        """,
        ),
      )
    ,
    test("a Native module has clang and nativeVersion and not the Js settings"):
      val legal = messages("""import histos.*
        module("native", Platform.Native)(scala"3.10.0")
          .clang(ClangMajor(21))
          .nativeVersion(NativeVersion("0.5.12"))
      """)
      assertTrue(
        legal.isEmpty,
        missing(
          "jsVersion",
          """import histos.*
          module("native", Native)(scala"3.10.0").jsVersion(JsVersion("1.22.0"))
        """,
        ),
        missing(
          "jsEnv",
          """import histos.*
          module("native", Native)(scala"3.10.0").jsEnv(JsEnv.Jsdom)
        """,
        ),
        missing(
          "jsModule",
          """import histos.*
          module("native", Native)(scala"3.10.0").jsModule(JsModuleKind.ESModule)
        """,
        ),
      )
    ,
    test("Js with Native has both groups, and Jvm with one of them does not have the other"):
      val both = messages("""import histos.*
        module("all", Js, Native)(scala"3.10.0").jsVersion(JsVersion("1.22.0")).clang(ClangMajor(21))
      """)
      val jvmJs = messages("""import histos.*
        module("core", Jvm, Js)(scala"3.10.0").jsEnv(JsEnv.Firefox)
      """)
      val jvmNative = messages("""import histos.*
        module("core", Jvm, Native)(scala"3.10.0").clang(ClangMajor(21))
      """)
      assertTrue(
        both.isEmpty,
        jvmJs.isEmpty,
        jvmNative.isEmpty,
        missing(
          "nativeVersion",
          """import histos.*
          module("core", Jvm, Js)(scala"3.10.0").nativeVersion(NativeVersion("0.5.12"))
        """,
        ),
        missing(
          "clang",
          """import histos.*
          module("core", Jvm, Js)(scala"3.10.0").clang(ClangMajor(21))
        """,
        ),
        missing(
          "jsVersion",
          """import histos.*
          module("core", Jvm, Native)(scala"3.10.0").jsVersion(JsVersion("1.22.0"))
        """,
        ),
        missing(
          "jsEnv",
          """import histos.*
          module("core", Jvm, Native)(scala"3.10.0").clang(ClangMajor(21)).jsEnv(JsEnv.Node)
        """,
        ),
      )
    ,
    test("pipe keeps the module's platforms"):
      val widened = messages("""import histos.*
        module("api", Jvm)(scala"3.10.0").pipe(item => item.jsVersion(JsVersion("1.22.0")))
      """)
      val kept = messages("""import histos.*
        def site[P](module: ModuleDef[P]): ModuleDef[P] = module.artifact(artifact"site")
        module("api", Jvm)(scala"3.10.0").pipe(site)
        module("web", Js)(scala"3.10.0").pipe(item => item.jsVersion(JsVersion("1.22.0")))
      """)
      assertTrue(
        widened match
          case head :: _ => head.contains("jsVersion is not a member")
          case Nil       => false,
        kept.isEmpty,
      )
    ,
    test("copy cannot put a Js setting on a JVM module"):
      val errors = messages("""import histos.*
        module("api", Jvm)(scala"3.10.0").copy(jsVersion = Some(JsVersion("1.22.0")))
      """)
      assertTrue:
        errors match
          case head :: _ => head.contains("copy is not a member")
          case Nil       => false
    ,
    test("a computed platform is not a case"):
      val errors = messages("""import histos.*
        val platform: Platform = Jvm
        module("api", platform)(scala"3.10.0")
      """)
      assertTrue:
        errors match
          case head :: _ => head.contains("module takes Jvm, Js, or Native")
          case Nil       => false
    ,
    test("an aggregate and Projects accept modules that build different platforms"):
      val errors = messages("""import histos.*
        val api    = module("api", Jvm)(scala"3.10.0")
        val web    = module("web", Js)(scala"3.10.0").jsVersion(JsVersion("1.22.0"))
        val native = module("native", Native)(scala"3.10.0").clang(ClangMajor(21))
        aggregate("root", api, web, native)
        Projects(api, web, native)
      """)
      assertTrue(errors.isEmpty),
  )

  def spec = members
end PlatformMemberSpec
