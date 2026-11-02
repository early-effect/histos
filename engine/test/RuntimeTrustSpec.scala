package histos.archive

import histos.workspace.Workspace

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions

import zio.{Scope, ZIO}
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue}

object RuntimeTrustSpec extends ZIOSpecDefault:
  def spec: zio.test.Spec[TestEnvironment & Scope, Any] = suite("runtime cache trust")(
    test("a private directory prepares"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-trust").mapError(error => new java.io.IOException(error.message))
          prepared <- RuntimeCache.prepare(root.resolve("cache")).either
        yield assertTrue(prepared == Right(()))
    ,
    test("a group-writable ancestor is shared"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-trust-wide").mapError(error => new java.io.IOException(error.message))
          wide = root.resolve("wide")
          _ <- ZIO.attempt {
            Files.createDirectories(wide)
            Files.setPosixFilePermissions(wide, PosixFilePermissions.fromString("rwxrwxr-x"))
          }
          prepared <- RuntimeCache.prepare(wide.resolve("cache")).either
        yield assertTrue(prepared == Left(RuntimeCacheError.Shared(wide)))
    ,
  ) @@ TestAspect.withLiveSystem
end RuntimeTrustSpec
