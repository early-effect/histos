package histos.js

import histos.cache.{ContentStore, LinkerStarts}
import histos.jdk.{JdkRuntime, Sha256}
import histos.pack.MainClass
import histos.resolve.LockedTools
import histos.schedule.RemoteCache
import histos.ScalaVersion

import java.nio.file.Path

import zio.ZIO

/** One link of `.sjsir` into a script. `optimize` is the resolved effective mode. */
final case class JsLinkRequest(
    version: ScalaVersion,
    main: MainClass,
    classpath: List[Path],
    work: Path,
    jdk: JdkRuntime,
    kind: histos.JsModuleKind = histos.JsModuleKind.NoModule,
    revision: Option[histos.JsVersion] = None,
    initialize: Boolean = true,
    arguments: List[String] = Nil,
    optimize: histos.JsOptimize = histos.JsOptimize.Fast,
)

final case class JsLinked(script: Path, key: Sha256)

object JsLink:
  def link(
      request: JsLinkRequest
  ): ZIO[LockedTools & ContentStore & LinkerStarts & RemoteCache, JsLinkError, JsLinked] =
    JsProducts.link(request)
