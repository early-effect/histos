package histos

import histos.resolve.ToolRole
import histos.workspace.Workspace

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A cold Coursier cache is the named precondition of a compiling law, not a run-order miss. */
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
  )
