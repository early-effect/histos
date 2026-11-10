package histos.load

import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileSeverity, PositionBase}
import histos.ProjectGraph

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object BuildFileLawSpec extends ZIOSpecDefault:
  def spec = suite("build file laws")(
    test("one build is kept, every other entry is typed, and the temp path is rewritten before its absolute form"):
      val temp      = Path.of("nested/file.scala")
      val absolute  = Path.of("/tmp/histos-build-law.scala")
      val full      = temp.toAbsolutePath.toString
      val relative  = temp.toString
      val separator = java.io.File.separator
      val kept      = full.stripSuffix(relative).stripSuffix(separator)
      val message   = s"$full $relative"
      val program   =
        Workspace
          .work("histos-build-select")
          .flatMap: root =>
            val classes = root.resolve("classes")
            val empty   = root.resolve("empty")
            val missing = root.resolve("missing")
            val out     = Path.of("/work/classes")
            val loader  = Option(getClass.getClassLoader).getOrElse(ClassLoader.getPlatformClassLoader)
            for
              _ <- ZIO.attemptBlocking:
                Files.createDirectories(classes.resolve("demo"))
                Files.createDirectories(empty)
                Files.write(classes.resolve("demo").resolve("A.class"), Array.emptyByteArray)
                Files.write(classes.resolve("demo").resolve("A$.class"), Array.emptyByteArray)
                Files.write(classes.resolve("Anon.class"), Array.emptyByteArray)
                Files.write(classes.resolve("demo").resolve("A$anonfun.class"), Array.emptyByteArray)
                Files.write(classes.resolve("demo").resolve("note.txt"), Array.emptyByteArray)
              chosen   <- BuildFile.select(List(OneBuild))
              none     <- BuildFile.select(Nil).either
              forward  <- BuildFile.select(List(OneBuild, TwoBuild)).either
              backward <- BuildFile.select(List(TwoBuild, OneBuild)).either
              loaded   <- BuildFile.loadOne(loader, out, classFile(out, OneBuild.getClass.getName)).either
              text     <- BuildFile.loadOne(loader, out, out.resolve("java/lang/String.class")).either
              other    <- BuildFile.loadOne(loader, out, classFile(out, NotABuild.getClass.getName)).either
              absent = out.resolve("missing/NotThere.class")
              unread <- BuildFile.loadOne(loader, out, absent).either
              found  <- BuildFile.classFiles(classes)
              vacant <- BuildFile.classFiles(empty)
              walked <- BuildFile.classFiles(missing).either
            yield assertTrue(
              chosen eq OneBuild,
              BuildFile.module(classOf[OneBuild.type]) == Some(OneBuild),
              BuildFile.module(classOf[String]) == None,
              BuildFile.module(classOf[NotABuild.type]) == None,
              none == Left(LoadError.NoEntry),
              forward == Left(
                LoadError.AmbiguousEntry(
                  List(BinaryName.fromClass(OneBuild.getClass), BinaryName.fromClass(TwoBuild.getClass))
                )
              ),
              backward == Left(
                LoadError.AmbiguousEntry(
                  List(BinaryName.fromClass(TwoBuild.getClass), BinaryName.fromClass(OneBuild.getClass))
                )
              ),
              loaded == Right(Some(OneBuild)),
              text == Right(Option.empty[histos.HistosBuild]),
              other == Right(Option.empty[histos.HistosBuild]),
              unread == Left(LoadError.Io(IoFailure(absent, IoFault.Read))),
              BuildFile.rewrite(message, temp) == s"$kept${separator}histos.scala histos.scala",
              BuildFile.rewrite(s"$absolute $absolute", absolute) == "histos.scala histos.scala",
              BuildFile.rewrite("untouched", temp) == "untouched",
              BuildFile.isClass(Path.of("A.class")),
              BuildFile.isClass(Path.of("dir/A$.class")),
              BuildFile.isClass(Path.of("Anon.class")),
              !BuildFile.isClass(Path.of("A$anonfun.class")),
              !BuildFile.isClass(Path.of("A$anon.class")),
              !BuildFile.isClass(Path.of("A.scala")),
              !BuildFile.isClass(Path.of("/")),
              BuildFile.className(out, out.resolve("demo/App.class")) == "demo.App",
              BuildFile.className(out, out.resolve("A$.class")) == "A$",
              found.flatMap(fileName).toSet == Set("A.class", "A$.class", "Anon.class"),
              vacant == Nil,
              walked == Left(LoadError.Io(IoFailure(missing, IoFault.Walk))),
            )
      program
    ,
    test("object Demo compiles, and a type error stays a dotty error"):
      val program =
        Workspace
          .work("histos-build-compile")
          .flatMap: root =>
            val source  = root.resolve("Demo.scala")
            val broken  = root.resolve("Broken.scala")
            val out     = root.resolve("out")
            val display = root.resolve("histos.scala")
            val jars    = scalaLibrary
            for
              _ <- ZIO.attemptBlocking:
                Files.createDirectories(out)
                Files.writeString(source, "object Demo\n")
                Files.writeString(broken, "object Broken { val n: Int = \"no\" }\n")
              compiled <- BuildFile.compile(source, jars, out).either
              reporter <- BuildFile.drive(List(source), jars, out)
              notes = BuildFile.notesOf(reporter, display, source)
              failedDrive <- BuildFile.drive(List(broken), jars, out)
              failedNotes = BuildFile.notesOf(failedDrive, display, broken)
              failed <- BuildFile.compile(broken, jars, out).either
              messages = failed match
                case Left(LoadError.CompileFailed(found)) => found.map(_.text)
                case _                                    => Nil
              demo   <- ZIO.attemptBlocking(Files.isRegularFile(out.resolve("Demo.class")))
              module <- ZIO.attemptBlocking(Files.isRegularFile(out.resolve("Demo$.class")))
            yield assertTrue(
              jars.nonEmpty,
              compiled == Right(()),
              !reporter.hasErrors,
              notes == BuildNotes(false, Nil),
              demo,
              module,
              failedDrive.hasErrors,
              failedNotes.failed,
              failedNotes.problems.nonEmpty,
              failedNotes.problems.map(_.message) == messages,
              failedNotes.problems.map(_.severity) == List.fill(failedNotes.problems.length)(CompileSeverity.Error),
              failedNotes.problems.map(_.base) == List.fill(failedNotes.problems.length)(PositionBase.Dotty),
              failedNotes.problems.exists(problem => problem.startLine.isDefined && problem.path.contains(display)),
              failedNotes.problems.forall(_.path.forall(_ == display)),
            )
      program,
  )

  private def scalaLibrary: List[Path] =
    List(classOf[scala.Predef.type], classOf[scala.collection.immutable.Nil.type])
      .flatMap: loaded =>
        Option(loaded.getProtectionDomain)
          .flatMap(domain => Option(domain.getCodeSource))
          .flatMap(source => Option(source.getLocation))
          .flatMap(url => scala.util.Try(Path.of(url.toURI)).toOption)
      .distinct

  private def classFile(out: Path, binary: String): Path =
    out.resolve(binary.replace('.', java.io.File.separatorChar) + ".class")

  private def fileName(path: Path): Option[String] =
    Option(path.getFileName).map(_.toString)

  object OneBuild extends histos.HistosBuild:
    def graph: Either[histos.BuildError, ProjectGraph] =
      Right(ProjectGraph.trusted(Nil))

  object TwoBuild extends histos.HistosBuild:
    def graph: Either[histos.BuildError, ProjectGraph] =
      Right(ProjectGraph.trusted(Nil))

  object NotABuild
end BuildFileLawSpec
