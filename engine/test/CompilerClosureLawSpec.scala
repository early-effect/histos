package histos

import histos.resolve.{DependencyLock, LockedTool, ToolId, ToolRole, ToolVariant}
import histos.workspace.Workspace

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A cold Coursier cache is the named precondition of a compiling law, not a run-order miss. A Native law compiles at
  * the Scala version the lock's Scala Native libraries were published for.
  */
object CompilerClosureLawSpec extends ZIOSpecDefault:
  def spec = suite("compiler closure laws")(
    test("a cache without the locked compiler jars names the jar and says to run histos resolve"):
      ZIO.scoped:
        Workspace
          .work("histos-closure")
          .flatMap: empty =>
            CompilerClosure
              .in(empty)
              .build
              .flip
              .map: missing =>
                assertTrue(
                  missing match
                    case CompilerClosure.Missing.Jar(tool, _, _) => tool.role == ToolRole.Compiler
                    case _                                       => false
                  ,
                  missing.message.endsWith("Run `./histos resolve` in this checkout, then run the law again."),
                )
    ,
    test("a Native cell compiles at the newest Scala 3 nativelibraries row, not at the newest compiler"):
      for
        closure  <- ZIO.service[CompilerClosure]
        previous <- closure.previous
        scala2 = closure.lock.tools.map(_.id.scalaVersion).find(version => !version.value.startsWith("3."))
        named  <- rows(closure, previous :: scala2.toList).nativeScala
        absent <- rows(closure, Nil).nativeScala.flip
        only2  <- ZIO.foreach(scala2)(version => rows(closure, List(version)).nativeScala.flip)
      yield assertTrue(
        named == previous,
        named != closure.scalaVersion,
        absent == CompilerClosure.Missing.Tool(ToolRole.NativeLibraries),
        only2 == Some(CompilerClosure.Missing.Scala3(ToolRole.NativeLibraries)),
      )
    .provide(CompilerClosure.layer),
  )

  /** `closure`'s compilers, and a `nativelibraries` row with no jars at each of `versions`. */
  private def rows(closure: CompilerClosure, versions: List[ScalaVersion]): CompilerClosure =
    val variant = ToolVariant.Native(NativeVersion("0.5.13"))
    closure.copy(lock =
      DependencyLock(
        Nil,
        tools =
          versions.map(version => LockedTool(ToolId(ToolRole.NativeLibraries, version, variant), Nil, Nil, Nil, Nil)),
      )
    )
end CompilerClosureLawSpec
