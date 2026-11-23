package histos.splice

import scala.util.matching.Regex

/** Module specifiers in Scala.js linker output. No JS parser: Scala.js import lines are regular, whole statements at
  * the start of a line, and that is the form this reads. Libraries are esbuild's to read.
  */
object JsModules:

  // An import or re-export statement: it starts a line, and nothing between its keyword and its specifier is a quote or
  // a `;`, so a string that says "from" is part of some other statement.
  private val fromPat: Regex       = """(?m)^\s*(?:import|export)\s[^'";]*?\bfrom\s*(['"])([^'"]+)\1""".r
  private val sideEffectPat: Regex = """(?m)^\s*import\s*(['"])([^'"]+)\1""".r
  private val requirePat: Regex    = """require\s*\(\s*(['"])([^'"]+)\1\s*\)""".r

  def isBare(specifier: String): Boolean =
    specifier.nonEmpty &&
      !specifier.startsWith(".") &&
      !specifier.startsWith("/") &&
      !specifier.contains(":")

  def ident(specifier: String): String =
    "__splice_" + specifier.replaceAll("[^A-Za-z0-9]", "_")

  /** Bare specifier references in `js`, attributed to `referring`. */
  def bareRefs(js: String, referring: String): List[(String, String)] =
    specifiers(js).distinct.filter(isBare).map(_ -> referring)

  def specifiers(js: String): List[String] =
    fromPat.findAllMatchIn(js).map(_.group(2)).toList ++
      sideEffectPat.findAllMatchIn(js).map(_.group(2)).toList ++
      requirePat.findAllMatchIn(js).map(_.group(2)).toList

  def leftoverBare(js: String, mapped: Iterable[String]): List[String] =
    val found = specifiers(js).toSet
    mapped.toList.filter(spec => isBare(spec) && found.contains(spec))

  /** Drop ES module export lines so Closure can compile a script. */
  def dropExports(js: String): String =
    js.linesIterator
      .filterNot { line =>
        val t = line.trim
        t.startsWith("export ") || t.startsWith("export{") || t.startsWith("export*")
      }
      .mkString("\n")
end JsModules
