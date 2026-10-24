package histos.publish

import histos.version.Catalog
import histos.workspace.{IoFailure, IoFault}
import histos.ProjectGraph

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** `histos` and `histos.bat`, rendered from the manual ship the way a workflow file is. */
object LauncherScript:
  def check(repo: Path, graph: ProjectGraph): IO[ScriptError, Unit] =
    ZIO.fromEither(ship(graph)).flatMap:
      case None => ZIO.unit
      case Some(version) =>
        agree(repo.resolve("histos"), posix(version)) *>
          agree(repo.resolve("histos.bat"), windows(version))

  def update(repo: Path, graph: ProjectGraph): IO[ScriptError, Unit] =
    ZIO.fromEither(ship(graph)).flatMap:
      case None => ZIO.unit
      case Some(version) =>
        write(repo.resolve("histos"), posix(version), executable = true) *>
          write(repo.resolve("histos.bat"), windows(version), executable = false)

  def posix(version: String): String =
    Posix.replace("__VERSION__", version)

  def windows(version: String): String =
    Windows.replace("__VERSION__", version)

  private def ship(graph: ProjectGraph): Either[ScriptError, Option[String]] =
    graph.assembly match
      case None => Right(None)
      case Some(_) =>
        graph.catalog match
          case Catalog.Manual(rows) =>
            rows.toList.map(_.version.value).distinct match
              case List(version) => Right(Some(version))
              case _             => Left(ScriptError.Revision)
          case _ => Left(ScriptError.Revision)

  private def agree(path: Path, body: String): IO[ScriptError, Unit] =
    ZIO.attemptBlocking {
      if !Files.isRegularFile(path) then Left(ScriptError.Missing(path))
      else if Files.readString(path) == body then Right(())
      else Left(ScriptError.Drift(path))
    }.mapError(_ => ScriptError.Io(IoFailure(path, IoFault.Read))).flatMap(ZIO.fromEither)

  private def write(path: Path, body: String, executable: Boolean): IO[ScriptError, Unit] =
    ZIO.attemptBlocking {
      val same = Files.isRegularFile(path) && Files.readString(path) == body
      if !same then Files.writeString(path, body)
      if executable then path.toFile.setExecutable(true)
    }.mapError(_ => ScriptError.Io(IoFailure(path, IoFault.Write)))

  private val Posix: String = """#!/bin/sh
# Generated from the manual ship. `histos workflow --update` rewrites this file.
# Verifies the pinned JVM assembly and execs it.
# When java is not on PATH, the locked JDK is fetched, checked, and unpacked first.
# HISTOS_ASSEMBLY overrides the assembly path so a fixture can be used before a release exists.
# When dist/histos.jar is absent and the version is a snapshot, the script downloads
# rocks.earlyeffect:histos from Central snapshots. HISTOS_SNAPSHOTS overrides that repository.
set -eu

dir=$(dirname "$0") || exit 1
root=$(CDPATH= cd -- "$dir" && pwd) || exit 1
version=__VERSION__
shafile=$root/.histos/assembly.sha256
cache=${XDG_CACHE_HOME:-$HOME/.cache}

hash_of() {
  file=$1
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$file" | awk '{print $1}'
  else
    shasum -a 256 "$file" | awk '{print $1}'
  fi
}

fetch_snapshot() {
  snapshots=${HISTOS_SNAPSHOTS:-https://central.sonatype.com/repository/maven-snapshots}
  snapshots=${snapshots%/}
  meta_url=$snapshots/rocks/earlyeffect/histos/$version/maven-metadata.xml
  meta=$(curl -fsSL "$meta_url" 2>/dev/null) || {
    echo "histos $version: no snapshot at $meta_url" >&2
    return 1
  }
  timestamp=$(printf '%s\n' "$meta" | awk -F '[<>]' '/<timestamp>/ { print $3; exit }')
  build=$(printf '%s\n' "$meta" | awk -F '[<>]' '/<buildNumber>/ { print $3; exit }')
  if [ -z "$timestamp" ] || [ -z "$build" ]; then
    echo "histos $version: snapshot metadata has no timestamp" >&2
    return 1
  fi
  base=${version%-SNAPSHOT}
  unique=$base-$timestamp-$build
  dest=$cache/histos/launchers/$version/$unique/histos.jar
  jar_url=$snapshots/rocks/earlyeffect/histos/$version/histos-$unique.jar
  sha_url=$jar_url.sha256
  expected=$(curl -fsSL "$sha_url" 2>/dev/null | awk 'NR == 1 { print $1 }') || true
  if [ -z "${expected:-}" ]; then
    echo "histos $version: no checksum at $sha_url" >&2
    return 1
  fi
  if [ -f "$dest" ]; then
    actual=$(hash_of "$dest")
    if [ "$actual" = "$expected" ]; then
      printf '%s\n' "$dest"
      return 0
    fi
    rm -f "$dest"
  fi
  mkdir -p "$(dirname "$dest")"
  part=$dest.partial
  curl -fsSL "$jar_url" -o "$part" 2>/dev/null || {
    rm -f "$part"
    echo "histos $version: could not download $jar_url" >&2
    return 1
  }
  actual=$(hash_of "$part")
  if [ "$expected" != "$actual" ]; then
    rm -f "$part"
    echo "histos $version: checksum mismatch" >&2
    return 1
  fi
  mv "$part" "$dest"
  printf '%s\n' "$dest"
}

if [ -n "${HISTOS_ASSEMBLY:-}" ]; then
  jar=$HISTOS_ASSEMBLY
  check_sha=yes
elif [ -f "$root/dist/histos.jar" ]; then
  jar=$root/dist/histos.jar
  check_sha=yes
else
  case $version in
    *-SNAPSHOT)
      jar=$(fetch_snapshot) || exit 1
      check_sha=no
      ;;
    *)
      jar=$cache/histos/launchers/$version/histos.jar
      check_sha=yes
      ;;
  esac
fi

if [ ! -f "$jar" ]; then
  echo "histos $version: no assembly at $jar" >&2
  exit 1
fi

if [ "$check_sha" = yes ]; then
  if [ ! -f "$shafile" ]; then
    echo "histos $version: missing $shafile" >&2
    exit 1
  fi

  expected=$(tr -d '[:space:]' < "$shafile")
  actual=$(hash_of "$jar")

  if [ "$expected" != "$actual" ]; then
    echo "histos $version: checksum mismatch" >&2
    exit 1
  fi
fi

provision_jdk() {
  lock=$root/.histos/jdk.lock
  if [ ! -f "$lock" ]; then
    echo "histos $version: java is not on PATH and $lock is missing" >&2
    return 1
  fi
  url=$(awk '$1 == "url" { print $2 }' "$lock")
  sha=$(awk '$1 == "sha256" { print $2 }' "$lock" | tr '[:upper:]' '[:lower:]')
  if [ -z "${url:-}" ] || [ -z "${sha:-}" ]; then
    echo "histos $version: $lock needs url and sha256" >&2
    return 1
  fi
  dest=$cache/histos/jdks/$sha
  archive=$dest/archive
  unpack=$dest/unpack
  mkdir -p "$dest"
  if [ ! -f "$archive" ]; then
    curl -fsSL "$url" -o "$archive"
  fi
  actual=$(hash_of "$archive" | tr '[:upper:]' '[:lower:]')
  if [ "$actual" != "$sha" ]; then
    rm -f "$archive"
    echo "histos $version: jdk checksum mismatch" >&2
    return 1
  fi
  if [ ! -d "$unpack" ]; then
    mkdir -p "$unpack"
    tar -xf "$archive" -C "$unpack"
  fi
  java_bin=$(find "$unpack" -type f -name java | awk '/\/bin\/java$/ { print; exit }')
  if [ -z "${java_bin:-}" ]; then
    echo "histos $version: locked jdk has no bin/java" >&2
    return 1
  fi
  chmod +x "$java_bin"
  printf '%s\n' "$java_bin"
}

if command -v java >/dev/null 2>&1; then
  java_bin=$(command -v java)
else
  java_bin=$(provision_jdk) || exit 1
fi

exec "$java_bin" --enable-native-access=ALL-UNNAMED -jar "$jar" "$@"
"""

  private val Windows: String = """@echo off
setlocal EnableExtensions
rem Generated from the manual ship. histos workflow --update rewrites this file.
rem Not executed by the POSIX test.
set "ROOT=%~dp0"
set "VERSION=__VERSION__"
if defined HISTOS_ASSEMBLY (
  set "JAR=%HISTOS_ASSEMBLY%"
) else if exist "%ROOT%dist\histos.jar" (
  set "JAR=%ROOT%dist\histos.jar"
) else (
  if defined XDG_CACHE_HOME (
    set "JAR=%XDG_CACHE_HOME%\histos\launchers\%VERSION%\histos.jar"
  ) else (
    set "JAR=%USERPROFILE%\.cache\histos\launchers\%VERSION%\histos.jar"
  )
)
if not exist "%JAR%" (
  echo histos %VERSION%: no assembly at %JAR% 1>&2
  exit /b 1
)
if not exist "%ROOT%.histos\assembly.sha256" (
  echo histos %VERSION%: missing assembly.sha256 1>&2
  exit /b 1
)
set /p EXPECTED=<"%ROOT%.histos\assembly.sha256"
certutil -hashfile "%JAR%" SHA256 | findstr /v "hash" | findstr /v "CertUtil" > "%TEMP%\histos-sha.txt"
set /p ACTUAL=<"%TEMP%\histos-sha.txt"
if /I not "%EXPECTED%"=="%ACTUAL%" (
  echo histos %VERSION%: checksum mismatch 1>&2
  exit /b 1
)
if defined XDG_CACHE_HOME (
  set "CACHE=%XDG_CACHE_HOME%"
) else (
  set "CACHE=%USERPROFILE%\.cache"
)
where java >nul 2>&1
if errorlevel 1 (
  call :provision
  if errorlevel 1 exit /b 1
) else (
  set "JAVA_BIN=java"
)
"%JAVA_BIN%" --enable-native-access=ALL-UNNAMED -jar "%JAR%" %*
exit /b %ERRORLEVEL%

:provision
set "LOCK=%ROOT%.histos\jdk.lock"
if not exist "%LOCK%" (
  echo histos %VERSION%: java is not on PATH and %LOCK% is missing 1>&2
  exit /b 1
)
set "JDK_URL="
set "JDK_SHA="
for /f "usebackq tokens=1,2" %%A in ("%LOCK%") do (
  if "%%A"=="url" set "JDK_URL=%%B"
  if "%%A"=="sha256" set "JDK_SHA=%%B"
)
if "%JDK_URL%"=="" (
  echo histos %VERSION%: jdk.lock needs url and sha256 1>&2
  exit /b 1
)
if "%JDK_SHA%"=="" (
  echo histos %VERSION%: jdk.lock needs url and sha256 1>&2
  exit /b 1
)
set "DEST=%CACHE%\histos\jdks\%JDK_SHA%"
if not exist "%DEST%" mkdir "%DEST%"
set "ARCHIVE=%DEST%\archive"
if not exist "%ARCHIVE%" (
  curl -fsSL "%JDK_URL%" -o "%ARCHIVE%"
  if errorlevel 1 exit /b 1
)
certutil -hashfile "%ARCHIVE%" SHA256 | findstr /v "hash" | findstr /v "CertUtil" > "%TEMP%\histos-jdk-sha.txt"
set /p ACTUAL=<"%TEMP%\histos-jdk-sha.txt"
set "ACTUAL=%ACTUAL: =%"
if /I not "%JDK_SHA%"=="%ACTUAL%" (
  echo histos %VERSION%: jdk checksum mismatch 1>&2
  exit /b 1
)
set "UNPACK=%DEST%\unpack"
if not exist "%UNPACK%" mkdir "%UNPACK%"
tar -xf "%ARCHIVE%" -C "%UNPACK%"
if errorlevel 1 exit /b 1
set "JAVA_BIN="
for /f "delims=" %%J in ('dir /s /b "%UNPACK%\java.exe"') do (
  if not defined JAVA_BIN set "JAVA_BIN=%%J"
)
if not defined JAVA_BIN (
  echo histos %VERSION%: locked jdk has no bin\java.exe 1>&2
  exit /b 1
)
exit /b 0
"""

enum ScriptError:
  case Drift(path: Path)
  case Missing(path: Path)
  case Io(failure: IoFailure)
  case Revision

  def message: String = this match
    case Drift(path)    => s"launcher script does not match the ship: $path"
    case Missing(path)  => s"no launcher script at $path"
    case Io(failure)    => failure.message
    case Revision       => "the launcher script needs one manual ship version"
