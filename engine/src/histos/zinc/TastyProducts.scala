package histos.zinc

import histos.workspace.{IoFailure, IoFault}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** TASTy without its defining class is not an owned compile product. */
private[zinc] object TastyProducts:
  def orphans(root: Path): Either[CompileError, List[Path]] =
    try
      if !Files.isDirectory(root) then Right(Nil)
      else
        val walk = Files.walk(root)
        try
          Right(walk.iterator.asScala.filter { path =>
            Option(path.getFileName).exists { file =>
              val name = file.toString
              val stem = name.stripSuffix(".tasty")
              Files.isRegularFile(path) && name.endsWith(".tasty") &&
              !Files.isRegularFile(path.resolveSibling(stem + ".class")) &&
              !Files.isRegularFile(path.resolveSibling(stem + "$.class"))
            }
          }.toList)
        finally walk.close()
    catch case NonFatal(_) => Left(CompileError.Io(IoFailure(root, IoFault.Walk)))

  def prune(root: Path): Either[CompileError, Unit] =
    orphans(root).flatMap { paths =>
      try
        paths.foreach(Files.deleteIfExists(_))
        Right(())
      catch case NonFatal(_) => Left(CompileError.Io(IoFailure(root, IoFault.Delete)))
    }
end TastyProducts
