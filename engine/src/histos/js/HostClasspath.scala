package histos.js

import histos.process.ChildClasspath

import java.nio.file.Path

/** The classpath this histos process was started with. A child that links or runs Scala.js uses it, so the linker
  * classes are the ones histos was compiled against. The first entry is a class directory in a checkout and
  * `histos.jar` in an installed toolchain.
  */
object HostClasspath:
  def child: Option[ChildClasspath] =
    val raw   = Option(System.getProperty("java.class.path")).getOrElse("")
    val paths = raw.split(java.io.File.pathSeparator).toList.filter(_.nonEmpty).map(Path.of(_))
    paths match
      case head :: tail => Some(ChildClasspath(head, tail))
      case Nil          => None
