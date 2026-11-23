package histos.zinc

import histos.workspace.Workspace
import histos.{Platform, ScalaVersion}

import zio.ZIO
import zio.test.*

object ScopedRootsSpec extends ZIOSpecDefault:
  def spec = suite("conventional shared platform roots")(
    test("scalajvmnative selects one physical source on JVM and Native, never JS") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-shared-platform-roots")
          shared = root.resolve("scalajvmnative/dev/earlyeffect/heddle/Shared.scala")
          _      <- Workspace.writeString(shared, "package dev.earlyeffect.heddle\nobject Shared\n")
          jvm    <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Jvm)
          native <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Native)
          js     <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Js)
          jar    <- Sources.packaged(root, ScalaVersion("3.10.1"), Platform.Jvm)
        yield assertTrue(
          jvm == List(shared),
          native == List(shared),
          js.isEmpty,
          jar == List(PackagedSource("dev/earlyeffect/heddle/Shared.scala", shared)),
        )
      }
    },
    test("platform names after a package or language root stay in package paths") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-platform-package-paths")
          entries = List(
            "src/main/scala/scalajvmnative/Shared.scala" -> "scalajvmnative/Shared.scala",
            "src/main/scala/jvm/Jvm.scala"               -> "jvm/Jvm.scala",
            "dev/scalajvmnative/Shared.scala"            -> "dev/scalajvmnative/Shared.scala",
            "scala/scalajvmnative/Shared.scala"          -> "scala/scalajvmnative/Shared.scala",
          )
          _ <- ZIO.foreachDiscard(entries) { (path, _) =>
            Workspace.writeString(root.resolve(path), "object Shared\n")
          }
          results <- ZIO.foreach(Platform.values.toList) { platform =>
            for
              selected <- Sources.select(root, ScalaVersion("3.10.1"), platform)
              packaged <- Sources.packaged(root, ScalaVersion("3.10.1"), platform)
            yield assertTrue(
              selected.toSet == entries.map((path, _) => root.resolve(path)).toSet,
              packaged.toSet == entries.map((path, entry) => PackagedSource(entry, root.resolve(path))).toSet,
            )
          }
        yield results.foldLeft(assertTrue(true))(_ && _)
      }
    },
    test("shared platform unions intersect Scala filters and preserve main/test separation") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-platform-filter-intersection")
          paths = List(
            "jvm/Jvm.scala",
            "scalajvm/JvmAlias.scala",
            "js/Js.scala",
            "scalajs/JsAlias.scala",
            "native/Native.scala",
            "scalanative/NativeAlias.scala",
            "scalajvmnative/scala-3/Series.scala",
            "scalajvmnative/scala-3.10.1/Exact.scala",
            "scalajvmnative/scala-2.13/Other.scala",
            "scalajvmnative/scala-3.9.0/OtherExact.scala",
            "scalajvmnative/Top.scala",
            "scalajvmnative/Java.java",
            "scalajvmnative/test/SharedSpec.scala",
          )
          _           <- ZIO.foreachDiscard(paths)(path => Workspace.writeString(root.resolve(path), ""))
          jvm         <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Jvm)
          native      <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Native)
          js          <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Js)
          jvmTests    <- Sources.tests(root, ScalaVersion("3.10.1"), Platform.Jvm)
          nativeTests <- Sources.tests(root, ScalaVersion("3.10.1"), Platform.Native)
          jsTests     <- Sources.tests(root, ScalaVersion("3.10.1"), Platform.Js)
          packaged    <- Sources.packaged(root, ScalaVersion("3.10.1"), Platform.Native)
          shared = Set(
            root.resolve("scalajvmnative/scala-3/Series.scala"),
            root.resolve("scalajvmnative/scala-3.10.1/Exact.scala"),
            root.resolve("scalajvmnative/Top.scala"),
          )
          tests = List(root.resolve("scalajvmnative/test/SharedSpec.scala"))
        yield assertTrue(
          jvm.toSet == shared ++ Set(
            root.resolve("jvm/Jvm.scala"),
            root.resolve("scalajvm/JvmAlias.scala"),
            root.resolve("scalajvmnative/Java.java"),
          ),
          native.toSet == shared ++ Set(
            root.resolve("native/Native.scala"),
            root.resolve("scalanative/NativeAlias.scala"),
          ),
          js.toSet == Set(root.resolve("js/Js.scala"), root.resolve("scalajs/JsAlias.scala")),
          jvmTests == tests,
          nativeTests == tests,
          jsTests.isEmpty,
          packaged.map(_.entry).toSet == Set(
            "Native.scala",
            "NativeAlias.scala",
            "Series.scala",
            "Exact.scala",
            "Top.scala",
          ),
        )
      }
    },
  )
