package histos.scaffold

import histos.supply.ZipxImport
import histos.{Dep, DepKind, MainClass, Organization, ProjectName, ProjectPath, ScalaVersion}

/** A `histos.scala` draft. A setting that is not mapped stays as a comment.
  */
object ImportDraft:
  def sbt(build: String, plugins: String, fallbackName: String): String =
    val vals     = strings(build + "\n" + plugins)
    val stripped = strip(build)
    val spans    = projects(stripped)
    val scalaAll = scalaPin(stripped, vals)
    val org      = organization(stripped, vals)
    val shared   = thisBuildDeps(stripped, vals)
    val compiled =
      if spans.isEmpty then List(one(stripped, fallbackName, vals, scalaAll))
      else spans.map(span => moduleOf(span, vals, scalaAll))
    val notes = shared.notes ++ pluginNotes(plugins) ++ settingNotes(build) ++ settingNotes(plugins)
    render(compiled, org, notes ++ shared.copied, shared.compile, shared.test, fallbackName, Some(scalaAll))

  def zipx(versions: String, modules: List[String], fallbackName: String): String =
    val vals   = strings(versions)
    val report = ZipxImport.read(versions, "", "")
    val libs   = report.rows.collect { case row if row.kind == "Lib" => lib(row.text, vals) }
    val mapped = libs.collect { case Right(coord) => coord }
    val missed = libs.collect { case Left(note) => note }
    val other  = report.rows.collect {
      case row if row.kind != "Lib" => s"// unmapped: ${row.kind} ${row.text}"
    }
    val assigned = scalaPin(strip(versions), vals)
    val scalaAll = if assigned == ScalaPin.Missing then scalaFromVals(vals) else assigned
    val names    =
      if modules.nonEmpty then modules
      else
        val fallback = if ProjectName.from(fallbackName).isRight then fallbackName else "app"
        List(fallback)
    val extra =
      if modules.isEmpty then List("// unmapped: ZipxVersions does not name modules")
      else if names.length > 1 then List("// unmapped: ZipxVersions does not say which module owns a library")
      else Nil
    val coords = if names.length > 1 then Nil else mapped
    val parked =
      if names.length > 1 then mapped.map(coord => s"// unmapped: ${coord.shown}") else Nil
    val head     = names.head
    val compiled = names.map { name =>
      val own = if name == head then coords else Nil
      Compiled(
        name,
        identFor(name, Set.empty),
        None,
        own.filter(coord => !coord.test).map(_.dep),
        own.filter(_.test).map(_.dep),
        Nil,
        None,
        Nil,
        aggregate = false,
      )
    }
    val idents = assign(compiled.map(_.name))
    render(
      compiled.map(mod => mod.copy(ident = idents(mod.name))),
      None,
      missed ++ other ++ extra ++ parked,
      Nil,
      Nil,
      fallbackName,
      Some(scalaAll),
    )

  private final case class Coord(dep: Dep, test: Boolean):
    def shown: String = renderDep(dep)

  private final case class Shared(compile: List[Dep], test: List[Dep], notes: List[String], copied: List[String])

  private final case class Span(name: String, text: String)

  private final case class Compiled(
      name: String,
      ident: String,
      directory: Option[String],
      compile: List[Dep],
      test: List[Dep],
      edges: List[String],
      main: Option[String],
      notes: List[String],
      aggregate: Boolean,
  )

  private def one(text: String, fallbackName: String, vals: Map[String, String], scalaAll: ScalaPin): Compiled =
    val named = nameOf(text).filter(name => ProjectName.from(name).isRight).getOrElse(fallbackName)
    fill(named, text, vals, scalaAll, aggregate = false)

  private def moduleOf(span: Span, vals: Map[String, String], scalaAll: ScalaPin): Compiled =
    val aggregate = span.text.contains(".aggregate(") && depsIn(span.text, vals)._1.isEmpty && !span.text.contains(
      "libraryDependencies"
    )
    fill(span.name, span.text, vals, scalaAll, aggregate)

  private def fill(
      name: String,
      text: String,
      vals: Map[String, String],
      scalaAll: ScalaPin,
      aggregate: Boolean,
  ): Compiled =
    val (compile, test, depNotes) = depsIn(text, vals)
    val (edges, edgeNotes)        = depends(text)
    val directory                 = directoryOf(name, text)
    val main                      = mainOf(text)
    Compiled(
      name,
      name,
      directory.directory,
      compile,
      test,
      edges,
      main.main,
      depNotes ++ edgeNotes ++ directory.notes ++ main.notes ++ localScala(text, vals, scalaAll),
      aggregate,
    )

  private def localScala(text: String, vals: Map[String, String], scalaAll: ScalaPin): List[String] =
    val chosen = versionOf(scalaAll)
    scalaPin(text, vals) match
      case ScalaPin.Missing                             => Nil
      case ScalaPin.Found(version) if version == chosen => Nil
      case ScalaPin.Found(version)                      => List(s"// unmapped: module scala version $version")
      case ScalaPin.Bad(raw)                            => List(s"// unmapped: scalaVersion $raw")

  private final case class Dir(directory: Option[String], notes: List[String])

  private def directoryOf(name: String, text: String): Dir =
    FilePat.findFirstMatchIn(text).map(_.group(1)).filter(_.nonEmpty) match
      case None | Some(".")           => Dir(None, Nil)
      case Some(path) if path == name => Dir(None, Nil)
      case Some(path)                 =>
        ProjectPath.from(path) match
          case Right(_) => Dir(Some(path), Nil)
          case Left(_)  => Dir(None, List(s"// unmapped: directory $path"))

  private final case class MainFound(main: Option[String], notes: List[String])

  private def mainOf(text: String): MainFound =
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    val raw   = lines.collectFirst {
      case line if line.contains("mainClass") && !line.contains("assembly") => line
    }
    raw match
      case None       => MainFound(None, Nil)
      case Some(line) =>
        MainPat.findFirstMatchIn(line) match
          case Some(hit) =>
            MainClass.from(hit.group(1)) match
              case Right(_) => MainFound(Some(hit.group(1)), Nil)
              case Left(_)  => MainFound(None, List(s"// unmapped: $line"))
          case None => MainFound(None, List(s"// unmapped: $line"))

  private def depends(text: String): (List[String], List[String]) =
    DepsOn
      .findAllMatchIn(text)
      .foldLeft((List.empty[String], List.empty[String])):
        case ((edges, notes), hit) =>
          val body = hit.group(1).trim
          if body.nonEmpty && IdentList.matches(body) then (edges ++ body.split(",").map(_.trim).toList, notes)
          else (edges, notes :+ s"// unmapped: .dependsOn($body)")

  private def depsIn(text: String, vals: Map[String, String]): (List[Dep], List[Dep], List[String]) =
    DepPat
      .findAllMatchIn(text)
      .foldLeft((List.empty[Dep], List.empty[Dep], List.empty[String])):
        case ((compile, test, notes), hit) =>
          val line = lineAt(text, hit.start)
          if line.contains("classifier") || line.contains("exclude(") then
            (compile, test, notes :+ s"// unmapped: ${line.trim}")
          else
            val version = Option(hit.group(4)).orElse(Option(hit.group(5)).flatMap(vals.get))
            val config  = Option(hit.group(6)).orElse(Option(hit.group(7)))
            version match
              case None =>
                (compile, test, notes :+ s"// unmapped: ${line.trim}")
              case Some(_) if config.exists(token => !compileConfig(token) && !testConfig(token)) =>
                (compile, test, notes :+ s"// unmapped: ${line.trim}")
              case Some(revision) =>
                coordinate(hit.group(1), hit.group(2), hit.group(3), revision) match
                  case None      => (compile, test, notes :+ s"// unmapped: ${line.trim}")
                  case Some(dep) =>
                    if config.exists(testConfig) then (compile, test :+ dep, notes)
                    else (compile :+ dep, test, notes)

  private def thisBuildDeps(text: String, vals: Map[String, String]): Shared =
    val lines = text.linesIterator.filter(line => line.contains("ThisBuild") && line.contains("libraryDependencies"))
    val slice = lines.mkString("\n")
    if slice.isEmpty then Shared(Nil, Nil, Nil, Nil)
    else
      val (compile, test, notes) = depsIn(slice, vals)
      val copied                 =
        if compile.isEmpty && test.isEmpty then Nil
        else List("// ThisBuild dependencies were copied onto each module")
      Shared(compile, test, notes, copied)

  private def coordinate(org: String, op: String, name: String, revision: String): Option[Dep] =
    val sep = op match
      case "%%%" => ":::"
      case "%%"  => "::"
      case _     => ":"
    Dep.from(s"$org$sep$name:$revision").toOption

  private def compileConfig(token: String): Boolean =
    token == "Compile" || token == "compile"

  private def testConfig(token: String): Boolean =
    token == "Test" || token == "test"

  private def lib(body: String, vals: Map[String, String]): Either[String, Coord] =
    val trimmed     = body.trim
    val (raw, rest) = trimmed.headOption match
      case Some('s') if trimmed.startsWith("s\"") || trimmed.startsWith("s\"") =>
        quoted(trimmed.drop(1), vals)
      case Some('"') | Some('s') => quoted(trimmed, vals)
      case _                     =>
        val token = trimmed.takeWhile(char => char.isLetterOrDigit || char == '_')
        vals.get(token) match
          case Some(value) => (value, trimmed.drop(token.length))
          case None        => (trimmed, "")
    val resolved = interpolate(raw, vals)
    val test     = rest.toLowerCase.contains("test")
    val other    = rest.trim.nonEmpty && !test
    if other then Left(s"// unmapped: Lib($body)")
    else
      Dep.from(resolved) match
        case Right(dep) => Right(Coord(dep, test))
        case Left(_)    => Left(s"// unmapped: Lib($body)")

  private def quoted(text: String, vals: Map[String, String]): (String, String) =
    val start = text.indexOf('"')
    if start < 0 then (text, "")
    else
      val end = text.indexOf('"', start + 1)
      if end < 0 then (text, "")
      else (interpolate(text.substring(start + 1, end), vals), text.drop(end + 1))

  private def interpolate(raw: String, vals: Map[String, String]): String =
    ValUse.replaceAllIn(
      raw,
      hit =>
        vals.get(hit.group(1)) match
          case Some(value) => java.util.regex.Matcher.quoteReplacement(value)
          case None        => java.util.regex.Matcher.quoteReplacement(hit.matched),
    )

  private def projects(text: String): List[Span] =
    val found = Decl.findAllMatchIn(text).toList
    found.zipWithIndex.map: (hit, index) =>
      val end = if index + 1 == found.length then text.length else found(index + 1).start
      Span(hit.group(1), text.substring(hit.start, end))

  private enum ScalaPin:
    case Missing
    case Found(version: String)
    case Bad(raw: String)

  private def scalaFromVals(vals: Map[String, String]): ScalaPin =
    val attempts = List("scalaVersion", "ScalaVersion", "scala", "scalaV").flatMap(key => vals.get(key))
    attempts.collectFirst { case raw if ScalaVersion.from(raw).isRight => raw } match
      case Some(raw) => ScalaPin.Found(ScalaVersion.from(raw).toOption.get.value)
      case None      => attempts.headOption.fold(ScalaPin.Missing)(ScalaPin.Bad(_))

  private def versionOf(pin: ScalaPin): String = pin match
    case ScalaPin.Found(version) => version
    case _                       => Skeleton.scalaVersion

  private def scalaPin(text: String, vals: Map[String, String]): ScalaPin =
    val found = ScalaPat.findAllMatchIn(text).toList
    if found.isEmpty then ScalaPin.Missing
    else
      val hit = found
        .find(item => text.slice(math.max(0, item.start - 40), item.start).contains("ThisBuild"))
        .getOrElse(found.head)
      val raw = Option(hit.group(1)).orElse(Option(hit.group(2)).flatMap(vals.get)).orElse(Option(hit.group(2)))
      raw match
        case None       => ScalaPin.Bad("")
        case Some(text) =>
          ScalaVersion.from(text) match
            case Right(version) => ScalaPin.Found(version.value)
            case Left(_)        => ScalaPin.Bad(text)

  private def organization(text: String, vals: Map[String, String]): Option[String] =
    OrgPat
      .findAllMatchIn(text)
      .toList
      .headOption
      .flatMap: hit =>
        val raw = Option(hit.group(1)).orElse(Option(hit.group(2)).flatMap(vals.get))
        raw.filter(value => Organization.from(value).isRight)

  private def nameOf(text: String): Option[String] =
    NamePat.findFirstMatchIn(text).map(_.group(1))

  private def strings(text: String): Map[String, String] =
    Assign.findAllMatchIn(strip(text)).map(hit => hit.group(1) -> hit.group(2)).toMap

  private def pluginNotes(plugins: String): List[String] =
    plugins.linesIterator.map(_.trim).filter(_.startsWith("addSbtPlugin")).map(line => s"// unmapped: $line").toList

  private def settingNotes(text: String): List[String] =
    text.linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .filterNot(_.startsWith("//"))
      .filterNot(_.startsWith("/*"))
      .flatMap { line =>
        if Unmapped.exists(line.contains) && !line.contains("scalaVersion") && !line.contains("libraryDependencies")
        then Some(clip(line))
        else None
      }
      .toList

  private def clip(line: String): String =
    val shown = if line.length > 160 then line.take(157) + "..." else line
    s"// unmapped: $shown"

  private def render(
      compiled: List[Compiled],
      organization: Option[String],
      notes: List[String],
      sharedCompile: List[Dep],
      sharedTest: List[Dep],
      fallbackName: String,
      scalaFixed: Option[ScalaPin] = None,
  ): String =
    val usable   = compiled.filter(mod => ProjectName.from(mod.name).isRight)
    val rejected = compiled.filterNot(mod => ProjectName.from(mod.name).isRight).map { mod =>
      s"// unmapped: project ${mod.name}"
    }
    val base       = if usable.nonEmpty then usable else List(fallbackModule(fallbackName))
    val withShared =
      if sharedCompile.isEmpty && sharedTest.isEmpty then base
      else
        base.map(mod =>
          if mod.aggregate then mod else mod.copy(compile = mod.compile ++ sharedCompile, test = mod.test ++ sharedTest)
        )
    val idents = assign(withShared.map(_.name))
    val known  = withShared.map(_.name).toSet
    val named  = withShared.map: mod =>
      val (ok, bad) = mod.edges.partition(known.contains)
      val extra     = bad.map(edge => s"// unmapped: ${mod.name} depends on $edge")
      mod.copy(ident = idents(mod.name), edges = ok, notes = mod.notes ++ extra)
    val modules          = named.filterNot(_.aggregate)
    val aggs             = named.filter(_.aggregate)
    val (ordered, cycle) = schedule(modules)
    val scalaRaw         = scalaFixed.getOrElse(ScalaPin.Missing)
    val scalaNote        = scalaRaw match
      case ScalaPin.Bad(raw) => List(s"// unmapped: scalaVersion $raw")
      case ScalaPin.Missing  => List("// unmapped: scala version was not in the source")
      case ScalaPin.Found(_) => Nil
    val scalaVer    = versionOf(scalaRaw)
    val versions    = List(scalaVer)
    val sharedScala = versions.distinct.length == 1
    val expr        = if sharedScala then "scala3" else s"""scala"$scalaVer""""
    val header      =
      List(
        "// Drafted by histos init. A line marked unmapped was not copied.",
        "import histos.*",
        "",
        "object build extends HistosBuild:",
        "  def graph =",
      )
    val scalaLine = if sharedScala then List(s"""    val scala3 = scala"$scalaVer"""") else Nil
    val bodies    = ordered.flatMap(mod => renderModule(mod, expr, idents))
    val aggLines  =
      if modules.isEmpty then Nil
      else
        aggs.map(mod => s"""    val ${mod.ident} = aggregate("${mod.name}", ${modules.map(_.ident).mkString(", ")})""")
    val aggNote =
      if aggs.nonEmpty && modules.isEmpty then List("// unmapped: aggregate with no modules") else Nil
    val comments =
      (scalaNote ++ notes ++ rejected ++ cycle ++ aggNote ++ named.flatMap(_.notes)).distinct.map(note => s"    $note")
    val listed   = (ordered ++ aggs).map(_.ident)
    val projects = organization match
      case Some(org) =>
        List(s"    Projects", s"""      .organization(organization"$org")(${listed.mkString(", ")})""")
      case None => List(s"    Projects(${listed.mkString(", ")})")
    (header ++ scalaLine ++ bodies ++ aggLines ++ comments ++ projects).mkString("\n") + "\n"

  private def fallbackModule(name: String): Compiled =
    val chosen = if ProjectName.from(name).isRight then name else "app"
    Compiled(chosen, chosen, None, Nil, Nil, Nil, None, Nil, aggregate = false)

  private def renderModule(mod: Compiled, scalaExpr: String, idents: Map[String, String]): List[String] =
    val head   = s"""    val ${mod.ident} = module("${mod.name}", Jvm)($scalaExpr)"""
    val chains = List(
      mod.directory.map(dir => s""".in(path"$dir")"""),
      if mod.edges.isEmpty then None else Some(s".dependsOn(${mod.edges.map(idents).mkString(", ")})"),
      call(".libraryDependencies", mod.compile),
      call(".testDependencies", mod.test),
      mod.main.map(main => s""".main(MainClass("$main"))"""),
    ).flatten
    if chains.isEmpty then List(head) else head :: chains.map(chain => s"      $chain")

  private def call(name: String, deps: List[Dep]): Option[String] =
    deps match
      case Nil => None
      case _   => Some(s"$name(${deps.map(renderDep).mkString(", ")})")

  private def renderDep(dep: Dep): String =
    val sep = dep.kind match
      case DepKind.Java   => ":"
      case DepKind.Binary => "::"
      case DepKind.Full   => ":::"
    s"""dep"${dep.organization}$sep${dep.name}:${dep.revision}""""

  private def schedule(pending: List[Compiled]): (List[Compiled], List[String]) =
    def go(rest: List[Compiled], done: Set[String], notes: List[String]): (List[Compiled], List[String]) =
      rest match
        case Nil => (Nil, notes)
        case _   =>
          val ready = rest.filter(_.edges.forall(done.contains))
          if ready.nonEmpty then
            val (later, more) =
              go(rest.filterNot(mod => ready.exists(_.name == mod.name)), done ++ ready.map(_.name), notes)
            (ready ++ later, more)
          else
            val head          = rest.head
            val dropped       = head.edges.filterNot(done.contains)
            val extra         = dropped.map(edge => s"// unmapped: ${head.name} depends on $edge")
            val fixed         = head.copy(edges = head.edges.filter(done.contains))
            val (later, more) = go(rest.tail, done + head.name, notes ++ extra)
            (fixed :: later, more)
    go(pending, Set.empty, Nil)

  private def assign(names: List[String]): Map[String, String] =
    val (idents, _) =
      names.foldLeft((Map.empty[String, String], Set.empty[String])): (state, name) =>
        val (acc, used) = state
        val ident       = identFor(name, used)
        (acc.updated(name, ident), used + ident)
    idents

  private def identFor(name: String, used: Set[String]): String =
    val raw   = name.map(char => if char == '-' then '_' else char)
    val base0 =
      if raw.nonEmpty && raw.head.isLetter && raw.forall(char => char.isLetterOrDigit || char == '_') then raw
      else "module"
    val base = if Keywords.contains(base0) then s"${base0}Module" else base0
    Iterator.from(0).map(n => if n == 0 then base else s"$base$n").find(ident => !used.contains(ident)).getOrElse(base)

  private def strip(text: String): String =
    Block.replaceAllIn(text, " ").linesIterator.map(line => LineComment.replaceAllIn(line, "")).mkString("\n")

  private def lineAt(text: String, index: Int): String =
    val start = text.lastIndexOf('\n', math.max(0, index - 1)) + 1
    val end   = text.indexOf('\n', index)
    text.substring(start, if end < 0 then text.length else end)

  private val Decl      = """(?:lazy\s+val|val)\s+([A-Za-z_][\w]*)\s*=\s*(?:\(\s*)?project\b""".r
  private val FilePat   = """file\(\s*"([^"]*)"\s*\)""".r
  private val DepsOn    = """\.dependsOn\(\s*([^)]*)\)""".r
  private val IdentList = """[A-Za-z_][\w]*(\s*,\s*[A-Za-z_][\w]*)*""".r
  private val DepPat    =
    """"([^"]+)"\s*(%%%|%%|%)\s*"([^"]+)"\s*%\s*(?:"([^"]+)"|([A-Za-z_][\w]*))(?:\s*%\s*(?:"([^"]+)"|([A-Za-z_][\w]*)))?""".r
  private val ScalaPat    = """scalaVersion\s*:=\s*(?:"([^"]+)"|([A-Za-z_][\w]*))""".r
  private val OrgPat      = """organization\s*:=\s*(?:"([^"]+)"|([A-Za-z_][\w]*))""".r
  private val NamePat     = """(?<![.\w])name\s*:=\s*"([^"]+)"""".r
  private val Assign      = """(?:lazy\s+)?val\s+([A-Za-z_][\w]*)\s*=\s*"([^"]*)"""".r
  private val MainPat     = """Some\(\s*"([^"]+)"\s*\)""".r
  private val Block       = """(?s)/\*.*?\*/""".r
  private val LineComment = """//.*""".r
  private val ValUse      = """\$([A-Za-z_][\w]*)""".r
  private val Unmapped    = List(
    "scalacOptions",
    "resolvers",
    "semanticdb",
    "crossScalaVersions",
    "addSbtPlugin",
    "enablePlugins",
    "crossProject",
    "assembly",
  )
  private val Keywords = Set(
    "type",
    "object",
    "val",
    "def",
    "class",
    "trait",
    "import",
    "package",
    "new",
    "lazy",
    "this",
    "super",
    "null",
    "true",
    "false",
  )
end ImportDraft
