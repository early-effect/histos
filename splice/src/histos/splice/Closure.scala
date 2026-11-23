package histos.splice

import com.google.javascript.jscomp.{
  CheckLevel,
  CommandLineRunner,
  CompilationLevel,
  Compiler,
  CompilerOptions,
  DiagnosticGroups,
  JSError,
  PropertyRenamingPolicy,
  SourceFile,
  SourceMap,
}
import zio.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.ArrayList
import java.util.logging.Level
import java.util.regex.Matcher
import scala.annotation.nowarn
import scala.util.matching.Regex

/** Post-link Closure advanced pass. Our pin (`v20260726`), not Scala.js's linker JAR. */
object Closure:

  val Version: String = "v20260726"

  /** Scala.js encoding of a real `_` in a Java name (`ISO_8859_1` → `ISO＿8859＿1`). */
  private[splice] val FullwidthLowLine: Char = '\uFF3F'

  /** ASCII stand-in Closure `v20260726` will lex. Advanced mode then renames the identifier. */
  private[splice] val FullwidthLowLineStandIn: String = "$uFF3F"

  private val fullwidthEscape: Regex = """\\u[Ff][Ff]3[Ff]""".r

  /** Knobs after `ADVANCED_OPTIMIZATIONS`. `programDigest` hashes these so the spliceClosure cache invalidates when
    * they change; there is no bump counter.
    */
  private[splice] object AdvancedPolicy:
    val propertyRenaming: PropertyRenamingPolicy                  = PropertyRenamingPolicy.OFF
    val disambiguateProperties: Boolean                           = false
    val ambiguateProperties: Boolean                              = false
    val collapseProperties: CompilerOptions.PropertyCollapseLevel =
      CompilerOptions.PropertyCollapseLevel.NONE
    val devirtualizeMethods: Boolean           = false
    val inlineProperties: Boolean              = false
    val inlineFunctions: CompilerOptions.Reach = CompilerOptions.Reach.ALL
    val assumeStrictThis: Boolean              = false
    val protectHiddenSideEffects: Boolean      = false
    val optimizeCalls: Boolean                 = false
    // Scala.js minify emits `class Throwable extends Error { constructor() { super(); } }`
    // plus getter-only `@JSExport("message")`. ES5 rewrote that to `Error.call(this);
    // this.message = …`, which is Call not SuperCall and assigns a getter-only export.
    val languageOut: CompilerOptions.LanguageMode        = CompilerOptions.LanguageMode.ECMASCRIPT_2015
    val removeUnusedPrototypeProperties: Boolean         = false
    val assumePropertiesAreStaticallyAnalyzable: Boolean = false
    val computeFunctionSideEffects: Boolean              = false
    val deadPropertyAssignmentElimination: Boolean       = false
    val extractPrototypeMemberDeclarations: CompilerOptions.ExtractPrototypeMemberDeclarationsMode =
      CompilerOptions.ExtractPrototypeMemberDeclarationsMode.OFF

    def fingerprint: String =
      List(
        s"standIn=$FullwidthLowLineStandIn",
        s"fullwidthEscape=${fullwidthEscape.regex}",
        s"propertyRenaming=$propertyRenaming",
        s"disambiguateProperties=$disambiguateProperties",
        s"ambiguateProperties=$ambiguateProperties",
        s"collapseProperties=$collapseProperties",
        s"devirtualizeMethods=$devirtualizeMethods",
        s"inlineProperties=$inlineProperties",
        s"inlineFunctions=$inlineFunctions",
        s"assumeStrictThis=$assumeStrictThis",
        s"protectHiddenSideEffects=$protectHiddenSideEffects",
        s"optimizeCalls=$optimizeCalls",
        s"languageOut=$languageOut",
        s"removeUnusedPrototypeProperties=$removeUnusedPrototypeProperties",
        s"assumePropertiesAreStaticallyAnalyzable=$assumePropertiesAreStaticallyAnalyzable",
        s"computeFunctionSideEffects=$computeFunctionSideEffects",
        s"deadPropertyAssignmentElimination=$deadPropertyAssignmentElimination",
        s"extractPrototypeMemberDeclarations=$extractPrototypeMemberDeclarations",
      ).mkString(";")

    def applyTo(options: CompilerOptions): Unit =
      options.setDisambiguateProperties(disambiguateProperties)
      options.setAmbiguateProperties(ambiguateProperties)
      options.setPropertyRenaming(propertyRenaming)
      options.setCollapsePropertiesLevel(collapseProperties)
      options.setDevirtualizeMethods(devirtualizeMethods)
      options.setInlineProperties(inlineProperties)
      options.setInlineFunctions(inlineFunctions)
      options.setAssumeStrictThis(assumeStrictThis)
      options.setProtectHiddenSideEffects(protectHiddenSideEffects)
      options.setOptimizeCalls(optimizeCalls)
      options.setLanguageOut(languageOut)
      options.setRemoveUnusedPrototypeProperties(removeUnusedPrototypeProperties)
      options.setAssumePropertiesAreStaticallyAnalyzable(assumePropertiesAreStaticallyAnalyzable)
      options.setComputeFunctionSideEffects(computeFunctionSideEffects)
      options.setDeadPropertyAssignmentElimination(deadPropertyAssignmentElimination)
      options.setExtractPrototypeMemberDeclarations(extractPrototypeMemberDeclarations)
  end AdvancedPolicy

  private val lock = new Object

  /** Protected names from `ClosureLinkerBackend.ScalaJSExterns` (Scala.js 1.22). */
  private[splice] val ScalaJSExterns: String =
    """
      |var Object;
      |Object.prototype.constructor;
      |Object.prototype.toString;
      |Object.prototype.$classData;
      |var Array;
      |Array.prototype.length;
      |var Function;
      |Function.prototype.call;
      |Function.prototype.apply;
      |var NaN = 0.0/0.0, Infinity = 1.0/0.0, undefined = void 0;
      |""".stripMargin

  /** Host free-vars Scala.js emits from `js.Dynamic.global` (MacrotaskExecutor, workers). */
  private[splice] val BrowserExterns: String =
    """
      |var globalThis;
      |var onmessage;
      |var attachEvent;
      |var addEventListener;
      |var postMessage;
      |var importScripts;
      |var setImmediate;
      |var setTimeout;
      |var MessageChannel;
      |var navigator;
      |""".stripMargin

  /** Node-shaped free-vars that are not browser host APIs. Compiler inputs, not externs: a `var process;` extern folds
    * `typeof process !== "undefined"` to true and leaves host `process.env` / `process.exitCode`, which throw in a
    * browser. Unknown names still fail Closure; do not grow this from an error dump.
    */
  private[splice] val NodeStubs: String =
    """
      |var process = { env: {}, exitCode: 0, browser: true };
      |""".stripMargin

  final case class Compiled(js: String, sourceMap: Option[String] = None)

  /** Rewrite Scala.js minify identifier encoding so Closure can parse it. */
  private[splice] def rewriteScalaJsNames(js: String): String =
    val escaped = fullwidthEscape.replaceAllIn(js, Matcher.quoteReplacement(FullwidthLowLineStandIn))
    escaped.replace(FullwidthLowLine.toString, FullwidthLowLineStandIn)

  def optimize(
      inputs: List[(String, String)],
      prefix: List[(String, String)] = Nil,
      extraExterns: List[String] = Nil,
      sourceMaps: Boolean = false,
      sourceMapFile: String = "out.js",
  ): IO[SpliceError, Compiled] =
    val prefixJs  = prefix.map(_._2).mkString
    val rewritten = inputs.map((name, code) => name -> rewriteScalaJsNames(code))
    ZIO
      .attemptBlocking(lock.synchronized(compile(rewritten, extraExterns, sourceMaps, sourceMapFile, prefixJs)))
      .mapError(e => SpliceError.Io(s"Closure: ${e.getMessage}"))
      .flatMap {
        case Left(err)       => ZIO.fail(err)
        case Right(compiled) => ZIO.succeed(compiled)
      }

  /** Hashes what Closure is given, and how, so a change anywhere a library reaches rebuilds. */
  def programDigest(libraries: String, linker: String, output: Path, sourceMaps: Boolean): String =
    val md                   = MessageDigest.getInstance("SHA-256")
    def add(s: String): Unit =
      md.update(s.getBytes(StandardCharsets.UTF_8))
    add(Version)
    add(AdvancedPolicy.fingerprint)
    add(ScalaJSExterns)
    add(BrowserExterns)
    add(NodeStubs)
    add(output.toAbsolutePath.normalize.toString)
    add("maps:" + sourceMaps)
    add(libraries)
    add(linker)
    md.digest.map("%02x".format(_)).mkString

  def cacheHit(stamp: Path, digest: String, output: Path, sourceMap: Option[Path] = None): Boolean =
    Files.isRegularFile(output) &&
      sourceMap.forall(p => Files.isRegularFile(p)) &&
      Files.isRegularFile(stamp) &&
      Files.readString(stamp).trim == digest

  def storeCache(stamp: Path, digest: String): Unit =
    Option(stamp.getParent).foreach(Files.createDirectories(_))
    Files.writeString(stamp, digest)
    ()

  private def compile(
      inputs: List[(String, String)],
      extraExterns: List[String],
      sourceMaps: Boolean,
      sourceMapFile: String,
      prefixJs: String,
  ): Either[SpliceError, Compiled] =
    Compiler.setLoggingLevel(Level.OFF)
    val compiler = new Compiler()
    compiler.disableThreads()
    val options = new CompilerOptions()
    CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
    // Scala.js subclasses are a lazy `$a_*()` factory. Closure cannot see that the super is a
    // spliced library, and optional lifecycle (`if (h.componentWillMount) h.componentWillMount()`)
    // unique-folds to the empty base if properties are treated as closed-world. Locals, function
    // inlining, and unused-export DCE stay. `protectHiddenSideEffects` is off so unused wrap-IIFEs
    // still fold after `assumeStrictThis` is false. Passes that require static property analysis
    // are off so `assumePropertiesAreStaticallyAnalyzable` can be false.
    AdvancedPolicy.applyTo(options)
    options.setLanguageIn(CompilerOptions.LanguageMode.ECMASCRIPT_2021)
    options.setPrettyPrint(false)
    options.setRewritePolyfills(false)
    options.setEnvironment(CompilerOptions.Environment.BROWSER)
    options.setWarningLevel(DiagnosticGroups.GLOBAL_THIS, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.DUPLICATE_VARS, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_REGEXP, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_TYPES, CheckLevel.OFF)
    options.setWarningLevel(DiagnosticGroups.CHECK_USELESS_CODE, CheckLevel.OFF)
    if sourceMaps then
      options.setSourceMapOutputPath(sourceMapFile)
      options.setSourceMapFormat(SourceMap.Format.V3)

    val externs = new ArrayList[SourceFile](defaultExterns())
    externs.add(SourceFile.fromCode("ScalaJSExterns.js", ScalaJSExterns))
    externs.add(SourceFile.fromCode("SpliceBrowserExterns.js", BrowserExterns))
    if extraExterns.nonEmpty then
      val decls = extraExterns.map(n => s"var $n;").mkString("\n")
      externs.add(SourceFile.fromCode("SpliceLibExterns.js", decls))

    val sources = new ArrayList[SourceFile](inputs.size + 1)
    sources.add(SourceFile.fromCode("SpliceNodeStubs.js", NodeStubs))
    inputs.foreach { (name, code) =>
      sources.add(SourceFile.fromCode(name, code))
    }

    val result = compiler.compile(externs, sources, options)
    if !result.success then Left(SpliceError.Closure(formatErrors(compiler)))
    else
      val js  = prefixJs + compiler.toSource
      val map =
        if sourceMaps then
          Option(compiler.getSourceMap).map { sm =>
            sm.setStartingPosition(SourceMaps.lineOffset(prefixJs), 0)
            val buf = new java.lang.StringBuilder
            sm.appendTo(buf, sourceMapFile)
            buf.toString
          }
        else None
      Right(Compiled(js, map))
  end compile

  @nowarn("cat=deprecation")
  private def defaultExterns(): java.util.List[SourceFile] =
    CommandLineRunner.getDefaultExterns()

  private def formatErrors(compiler: Compiler): String =
    val errors = compiler.getErrors
    if errors == null || errors.isEmpty then "unknown Closure error"
    else
      val it = errors.iterator()
      val b  = List.newBuilder[String]
      while it.hasNext do b += formatError(it.next())
      b.result().mkString("\n")

  private def formatError(err: JSError): String =
    s"${err.getSourceName}:${err.getLineNumber}: ${err.getDescription}"
end Closure
