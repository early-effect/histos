package histos.publish

import histos.archive.{RuntimeCache, RuntimeCacheError}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.Files
import java.nio.file.attribute.{AclEntry, AclEntryPermission, AclEntryType, AclFileAttributeView}
import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object RuntimeTrustWindowsSpec extends ZIOSpecDefault:
  def spec = suite("Windows physical ancestor trust")(
    test(
      "N1 runtime and updater reject owned junctions behind a shared physical parent"
    ):
      ZIO.scoped:
        for
          root   <- Workspace.work("histos-trust-junction")
          shared <- Workspace.createDirectories(root.resolve("shared"))
          target = shared.resolve("private")
          _ <- RuntimeCache.prepare(target)
          alias = root.resolve("alias")
          process <- ZIO.acquireRelease(
            ZIO
              .attemptBlocking(
                new ProcessBuilder(
                  "cmd.exe",
                  "/d",
                  "/c",
                  "mklink",
                  "/J",
                  alias.toString,
                  target.toString,
                )
                  .redirectOutput(ProcessBuilder.Redirect.DISCARD)
                  .redirectError(ProcessBuilder.Redirect.DISCARD)
                  .start()
              )
              .mapError(_ => IoFailure(alias, IoFault.Create))
          )(process => ZIO.attemptBlocking(process.destroyForcibly()).ignore)
          status <- ZIO
            .attemptBlocking(process.waitFor())
            .mapError(_ => IoFailure(alias, IoFault.Create))
          _ <- ZIO.fail(IoFailure(alias, IoFault.Create)).unless(status == 0)
          _ <- ZIO.addFinalizer(
            ZIO.attemptBlocking(Files.deleteIfExists(alias)).ignore
          )
          view <- ZIO
            .attemptBlocking(
              Option(
                Files
                  .getFileAttributeView(shared, classOf[AclFileAttributeView])
              )
            )
            .mapError(_ => IoFailure(shared, IoFault.Stat))
            .someOrFail(IoFailure(shared, IoFault.Stat))
          original <- ZIO
            .attemptBlocking(view.getAcl)
            .mapError(_ => IoFailure(shared, IoFault.Stat))
          everyone <- ZIO
            .attemptBlocking(
              shared.getFileSystem.getUserPrincipalLookupService
                .lookupPrincipalByName("Everyone")
            )
            .mapError(_ => IoFailure(shared, IoFault.Stat))
          rule = AclEntry
            .newBuilder()
            .setType(AclEntryType.ALLOW)
            .setPrincipal(everyone)
            .setPermissions(
              AclEntryPermission.DELETE_CHILD,
              AclEntryPermission.WRITE_DATA,
            )
            .build()
          _ <- ZIO.acquireRelease(
            ZIO
              .attemptBlocking(
                view.setAcl((original.asScala.toList :+ rule).asJava)
              )
              .mapError(_ => IoFailure(shared, IoFault.Write))
          )(_ => ZIO.attemptBlocking(view.setAcl(original)).ignore)
          cache = alias.resolve("new/cache")
          runtime  <- RuntimeCache.prepare(cache).either
          updater  <- LauncherFiles.privateDirectory(cache).either
          physical <- ZIO
            .attemptBlocking(shared.toRealPath())
            .mapError(_ => IoFailure(shared, IoFault.Stat))
          created <- ZIO
            .attemptBlocking(Files.exists(target.resolve("new")))
            .mapError(_ => IoFailure(target, IoFault.Stat))
        yield assertTrue(
          runtime == Left(RuntimeCacheError.Shared(physical)),
          updater == Left(LauncherError.Cache(cache)),
          !created,
        )
  ) @@ TestAspect.windows @@ TestAspect.withLiveSystem @@ TestAspect.timeout(
    30.seconds
  )
