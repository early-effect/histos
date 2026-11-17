package histos.publish

import histos.jdk.Sha256
import histos.pack.PackageKey
import histos.plugin.ArtifactFormat
import histos.PluginPublication
import histos.workspace.Workspace
import histos.zinc.ProductDigest

import java.nio.file.{Files, Path}

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.ClassNode

import zio.{Chunk, ZIO}
import zio.test.*

object PluginPackageKeySpec extends ZIOSpecDefault:
  def spec = suite("plugin package keys")(
    test("equal class products with different TASTy bytes do not share a key") {
      ZIO.scoped {
        Workspace
          .work("package-key")
          .flatMap: root =>
            val bytes  = classBytes
            val left   = root.resolve("left")
            val right  = root.resolve("right")
            val tastyA = Array[Byte](1, 2, 3, 4)
            val tastyB = Array[Byte](1, 2, 3, 5)
            for
              _ <- ZIO.attemptBlocking {
                writeTree(left, bytes, tastyA, None)
                writeTree(right, bytes, tastyB, None)
              }.orDie
              digestA  <- ZIO.fromEither(ProductDigest.ofDirectory(left, root))
              digestB  <- ZIO.fromEither(ProductDigest.ofDirectory(right, root))
              rebuildA <- ZIO.fromEither(sealTree(entries(bytes, tastyA, None), PluginPublication.Rebuildable))
              rebuildB <- ZIO.fromEither(sealTree(entries(bytes, tastyB, None), PluginPublication.Rebuildable))
              binaryA  <- ZIO.fromEither(sealTree(entries(bytes, tastyA, None), PluginPublication.BinaryOnly))
              binaryB  <- ZIO.fromEither(sealTree(entries(bytes, tastyB, None), PluginPublication.BinaryOnly))
            yield assertTrue(
              digestA == digestB,
              rebuildA != rebuildB,
              binaryA != binaryB,
              rebuildA != binaryA,
            )
      }
    },
    test("one TASTy byte changes the key and no compiler runs") {
      val bytes  = classBytes
      val before = Array[Byte](9, 8, 7)
      val after  = Array[Byte](9, 8, 8)
      for
        first  <- ZIO.fromEither(sealTree(entries(bytes, before, None), PluginPublication.Rebuildable))
        second <- ZIO.fromEither(sealTree(entries(bytes, after, None), PluginPublication.Rebuildable))
      yield assertTrue(first != second)
    },
    test("stamp bytes change the sealed key and the manifest key") {
      ZIO.scoped {
        Workspace
          .work("package-stamp")
          .flatMap: root =>
            val bytes  = classBytes
            val tasty  = Array[Byte](4, 4, 4)
            val left   = root.resolve("left")
            val right  = root.resolve("right")
            val stampA = "stamp-a\n".getBytes(java.nio.charset.StandardCharsets.UTF_8)
            val stampB = "stamp-b\n".getBytes(java.nio.charset.StandardCharsets.UTF_8)
            for
              _ <- ZIO.attemptBlocking {
                writeTree(left, bytes, tasty, Some(stampA))
                writeTree(right, bytes, tasty, Some(stampB))
              }.orDie
              digestA <- ZIO.fromEither(ProductDigest.ofDirectory(left, root))
              digestB <- ZIO.fromEither(ProductDigest.ofDirectory(right, root))
              entriesA = entries(bytes, tasty, Some(stampA))
              entriesB = entries(bytes, tasty, Some(stampB))
              sealedA   <- ZIO.fromEither(sealTree(entriesA, PluginPublication.Rebuildable))
              sealedB   <- ZIO.fromEither(sealTree(entriesB, PluginPublication.Rebuildable))
              manifestA <- ZIO.fromEither(manifestKey(entriesA))
              manifestB <- ZIO.fromEither(manifestKey(entriesB))
            yield assertTrue(digestA == digestB, sealedA != sealedB, manifestA != manifestB)
      }
    },
  )

  private def classBytes: Array[Byte] =
    val node = new ClassNode()
    node.version = Opcodes.V1_8
    node.access = Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER
    node.name = "pkg/A"
    node.superName = "java/lang/Object"
    node.interfaces = new java.util.ArrayList[String]()
    val writer = new org.objectweb.asm.ClassWriter(0)
    node.accept(writer)
    writer.toByteArray

  private def writeTree(root: Path, classFile: Array[Byte], tasty: Array[Byte], stamp: Option[Array[Byte]]): Unit =
    val pkg = root.resolve("pkg")
    Files.createDirectories(pkg)
    Files.write(pkg.resolve("A.class"), classFile)
    Files.write(pkg.resolve("A.tasty"), tasty)
    stamp match
      case None        => ()
      case Some(bytes) =>
        val meta = root.resolve("META-INF").resolve("histos")
        Files.createDirectories(meta)
        Files.write(meta.resolve("version"), bytes)

  private def entries(classFile: Array[Byte], tasty: Array[Byte], stamp: Option[Array[Byte]]): List[EntryBytes] =
    val products = List(
      EntryBytes("pkg/A.class", Chunk.fromArray(classFile)),
      EntryBytes("pkg/A.tasty", Chunk.fromArray(tasty)),
    )
    stamp match
      case None        => products
      case Some(bytes) => products :+ EntryBytes("META-INF/histos/version", Chunk.fromArray(bytes))

  private def sealTree(listed: List[EntryBytes], mode: PluginPublication): Either[PluginPublishFault, Sha256] =
    PluginBundle.manifest(listed).map(found => PackageKey.sealedOf(ArtifactFormat.Jar, mode, found))

  private def manifestKey(listed: List[EntryBytes]): Either[PluginPublishFault, Sha256] =
    PluginBundle.manifest(listed).map(found => PackageKey.manifestOf(ArtifactFormat.Jar, found))
end PluginPackageKeySpec
