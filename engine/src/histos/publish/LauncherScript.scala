package histos.publish

import histos.version.Catalog
import histos.workspace.{IoFailure, IoFault}
import histos.{Module, ProjectGraph, Publication}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** `histos` and `histos.bat`, rendered from the manual ship the way a workflow file is. Each script finds `java` and,
  * when the checkout has no local assembly, asks `cs fetch` for `.histos/build.properties`. JVM arguments come from a
  * repository `.jvmopts`, one per line. Histos does not install a JDK or Coursier.
  */
object LauncherScript:
  def check(repo: Path, graph: ProjectGraph): IO[ScriptError, Unit] =
    ZIO
      .fromEither(ship(graph))
      .flatMap:
        case None          => ZIO.unit
        case Some(version) =>
          LauncherPin
            .read(repo)
            .mapError(ScriptError.Pin(_))
            .flatMap: pin =>
              if !boots(version, pin.version) then ZIO.fail(ScriptError.Pin(LauncherError.Version(pin.version)))
              else
                agree(repo.resolve("histos"), posix(version)) *>
                  agree(repo.resolve("histos.bat"), windows(version))

  def update(repo: Path, graph: ProjectGraph): IO[ScriptError, Unit] =
    ZIO
      .fromEither(ship(graph))
      .flatMap:
        case None          => ZIO.unit
        case Some(version) =>
          LauncherPin
            .read(repo)
            .mapError(ScriptError.Pin(_))
            .flatMap: pin =>
              if !boots(version, pin.version) then ZIO.fail(ScriptError.Pin(LauncherError.Version(pin.version)))
              else
                write(
                  repo.resolve("histos"),
                  posix(version),
                  executable = true,
                ) *>
                  write(
                    repo.resolve("histos.bat"),
                    windows(version),
                    executable = false,
                  )

  def posix(version: String): String =
    Posix.replace("__VERSION__", version)

  def windows(version: String): String =
    Windows.replace("__VERSION__", version)

  /** A pointer or a timestamp boots when its channel is the script version, or the snapshot of a release ship. */
  def boots(ship: String, line: String): Boolean =
    LauncherPin.channel(line).exists: pointer =>
      pointer == ship || (!ship.endsWith("-SNAPSHOT") && pointer == s"$ship-SNAPSHOT")

  private def ship(graph: ProjectGraph): Either[ScriptError, Option[String]] =
    if !launcher(graph) then Right(None)
    else
      graph.catalog match
        case Catalog.Manual(rows) =>
          rows.toList.map(_.version.value).distinct match
            case List(version) => Right(Some(version))
            case _             => Left(ScriptError.Revision)
        case _ => Left(ScriptError.Revision)

  private def launcher(graph: ProjectGraph): Boolean =
    graph.projects.exists:
      case module: Module =>
        module.publication match
          case Publication.Assembly(_) => true
          case _                       => false
      case _ => false

  private def agree(path: Path, body: String): IO[ScriptError, Unit] =
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(path) then Left(ScriptError.Missing(path))
        else if Files.readString(path) == body then Right(())
        else Left(ScriptError.Drift(path))
      }
      .mapError(_ => ScriptError.Io(IoFailure(path, IoFault.Read)))
      .flatMap(ZIO.fromEither)

  private def write(
      path: Path,
      body: String,
      executable: Boolean,
  ): IO[ScriptError, Unit] =
    ZIO
      .attemptBlocking {
        val same = Files.isRegularFile(path) && Files.readString(path) == body
        if !same then Files.writeString(path, body)
        if executable then path.toFile.setExecutable(true)
      }
      .mapError(_ => ScriptError.Io(IoFailure(path, IoFault.Write)))

  private val Posix: String = """#!/bin/sh
# Generated from the manual ship. `histos workflow --update` rewrites this file.
# A local assembly is checked against .histos/assembly.sha256. Otherwise `cs fetch` reads the version file.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
version=__VERSION__
fail() { echo "histos $version: $*" >&2; exit 1; }
hash_of() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | awk '{print $1}'
  else shasum -a 256 "$1" | awk '{print $1}'; fi
}
valid_sha() { printf '%s\n' "$1" | awk 'length($0)==64 && $0 !~ /[^0-9a-fA-F]/ {ok=1} END {exit !ok}'; }
java_guide='A JDK 21 or newer is required. Histos does not install it.
macOS: brew install --cask temurin@21
Linux and Windows: https://adoptium.net/temurin/releases?version=21'
cs_guide='The cs command from Coursier 2.1 or newer is required. Histos does not install it.
macOS: brew install coursier/formulas/coursier
The Homebrew core formula provides coursier and does not.
Linux and Windows: https://get-coursier.io/docs/cli-installation and put the cs binary on PATH.'
java_bin=$(command -v java) || fail "java is not on PATH
$java_guide"
java_text=$("$java_bin" -version 2>&1 || true)
if ! java_found=$(printf '%s\n' "$java_text" | awk '
  /version "/ {
    if (match($0, /"[^"]+"/)) {
      print substr($0, RSTART + 1, RLENGTH - 2)
      found = 1
      exit
    }
  }
  END { if (!found) exit 1 }
'); then
  fail "java version is unreadable
$java_guide"
fi
java_major=$(printf '%s\n' "$java_found" | awk -F. '{ if ($1 == "1" && NF >= 2) print $2 + 0; else print $1 + 0 }')
[ "$java_major" -ge 21 ] || fail "java $java_found is older than 21
$java_guide"
if [ -n "${HISTOS_ASSEMBLY:-}" ]; then jar=$HISTOS_ASSEMBLY
elif [ -f "$root/dist/histos.jar" ]; then jar=$root/dist/histos.jar
else jar=; fi
if [ -n "$jar" ]; then
  [ -f "$root/.histos/assembly.sha256" ] || fail "missing assembly.sha256"
  expected=$(tr -d '[:space:]' < "$root/.histos/assembly.sha256")
  valid_sha "$expected" || fail "invalid assembly checksum"
  expected=$(printf '%s' "$expected" | tr '[:upper:]' '[:lower:]')
  [ -f "$jar" ] || fail "no assembly at $jar"
  [ "$(hash_of "$jar")" = "$expected" ] || fail "checksum mismatch"
else
  props=$root/.histos/build.properties
  [ -f "$props" ] || fail "missing build.properties; restore the committed version or use a source assembly"
  parse_version() {
    LC_ALL=C awk -v version="$version" '
      function bad() { failed = 1; exit 1 }
      function pointer(s) {
        return s ~ /^[0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9]+([.-][A-Za-z0-9]+)*)?-SNAPSHOT$/
      }
      function stamp_ok(s, y, m, d, h, n, t, days) {
        if (length(s) != 15 || substr(s, 9, 1) != ".") return 0
        if (substr(s, 1, 8) !~ /^[0-9]+$/ || substr(s, 10, 6) !~ /^[0-9]+$/) return 0
        y = substr(s, 1, 4) + 0; m = substr(s, 5, 2) + 0; d = substr(s, 7, 2) + 0
        h = substr(s, 10, 2) + 0; n = substr(s, 12, 2) + 0; t = substr(s, 14, 2) + 0
        days = (m == 2 ? 28 + (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)) : (m == 4 || m == 6 || m == 9 || m == 11 ? 30 : 31))
        return y >= 1 && m >= 1 && m <= 12 && d >= 1 && d <= days && h < 24 && n < 60 && t < 60
      }
      function channel_of(s, rest, stamp, ship) {
        if (pointer(s)) return s
        if (match(s, /-[1-9][0-9]*$/)) {
          rest = substr(s, 1, RSTART - 1)
          if (length(rest) >= 16 && substr(rest, length(rest) - 15, 1) == "-") {
            stamp = substr(rest, length(rest) - 14)
            ship = substr(rest, 1, length(rest) - 16)
            if (stamp_ok(stamp) && pointer(ship "-SNAPSHOT")) return ship "-SNAPSHOT"
          }
        }
        return ""
      }
      { sub(/^[ \t\r]+/, ""); sub(/[ \t\r]+$/, "") }
      /^$/ || /^#/ { next }
      {
        eq = index($0, "=")
        if (eq <= 1) bad()
        key = substr($0, 1, eq - 1)
        value = substr($0, eq + 1)
        gsub(/^[ \t]+|[ \t]+$/, "", key)
        gsub(/^[ \t]+|[ \t]+$/, "", value)
        if (key != "histos.version" || value == "") bad()
        if (seen++) bad()
        line = value
      }
      END {
        if (failed || line == "") exit 1
        ch = channel_of(line)
        if (ch == "") exit 1
        if (!(ch == version || (version !~ /-SNAPSHOT$/ && ch == version "-SNAPSHOT"))) exit 1
        print line
        if (line ~ /-SNAPSHOT$/) print "pointer"
        else print "revision"
      }
    ' "$1"
  }
  if ! parsed=$(parse_version "$props"); then fail "invalid build.properties"; fi
  line=$(printf '%s\n' "$parsed" | awk 'NR==1 { print; exit }')
  kind=$(printf '%s\n' "$parsed" | awk 'NR==2 { print; exit }')
  [ -n "$line" ] && [ -n "$kind" ] || fail "invalid build.properties"
  repo=${HISTOS_LAUNCHER_REPO:-https://central.sonatype.com/repository/maven-snapshots}
  case $repo in
    *[[:space:]]*) fail "launcher repository is not a supported base: $repo" ;;
    https://*|http://127.0.0.1|http://127.0.0.1/*|http://127.0.0.1:*|http://localhost|http://localhost/*|http://localhost:*|file:///*) ;;
    *) fail "launcher repository is not a supported base: $repo" ;;
  esac
  cs_bin=$(command -v cs) || fail "coursier is not on PATH
$cs_guide"
  cs_text=$("$cs_bin" version 2>&1 || true)
  if ! cs_found=$(printf '%s\n' "$cs_text" | awk '
    match($0, /[0-9]+\.[0-9]+(\.[0-9]+)?/) { print substr($0, RSTART, RLENGTH); found = 1; exit }
    END { if (!found) exit 1 }
  '); then
    fail "coursier version is unreadable
$cs_guide"
  fi
  cs_major=$(printf '%s\n' "$cs_found" | awk -F. '{ print $1 + 0 }')
  cs_minor=$(printf '%s\n' "$cs_found" | awk -F. '{ print $2 + 0 }')
  if [ "$cs_major" -lt 2 ] || { [ "$cs_major" -eq 2 ] && [ "$cs_minor" -lt 1 ]; }; then
    fail "coursier $cs_found is older than 2.1
$cs_guide"
  fi
  if ! output=$(
    if [ "$kind" = pointer ]; then
      COURSIER_TTL=0s
      export COURSIER_TTL
    else
      unset COURSIER_TTL
    fi
    "$cs_bin" fetch --no-default -r "$repo" "rocks.earlyeffect:histos:$line"
  ); then
    fail "launcher fetch failed"
  fi
  paths=$(printf '%s\n' "$output" | awk 'NF { c++ } END { print c + 0 }')
  [ "$paths" -eq 1 ] || fail "launcher fetch did not print one jar"
  jar=$(printf '%s\n' "$output" | awk 'NF { print; exit }')
fi
shell_quote() {
  out="'"
  rest=$1
  while :; do
    case $rest in
      *\'*)
        out="$out${rest%%\'*}'\\''"
        rest=${rest#*\'}
        ;;
      *)
        printf '%s' "$out$rest'"
        return
        ;;
    esac
  done
}
jvm_extra=
opts=$root/.jvmopts
if [ -e "$opts" ] || [ -L "$opts" ]; then
  [ -f "$opts" ] && [ -r "$opts" ] || fail "invalid file path: $opts"
  cr=$(printf '\r')
  while IFS= read -r opt || [ -n "$opt" ]; do
    opt=${opt%"$cr"}
    case $opt in
      ''|\#*) continue ;;
      -jar|-m|--module) fail "jvm option selects the program: $opt" ;;
      @*) fail "jvm option selects the program: $opt" ;;
    esac
    jvm_extra="$jvm_extra $(shell_quote "$opt")"
  done < "$opts"
fi
quoted_args=
i=1
while [ "$i" -le "$#" ]; do
  eval "one=\${$i}"
  quoted_args="$quoted_args $(shell_quote "$one")"
  i=$((i + 1))
done
eval "set -- ${jvm_extra} --enable-native-access=ALL-UNNAMED -jar $(shell_quote "$jar")${quoted_args}"
exec "$java_bin" "$@"
"""

  private val Windows: String = """@echo off
setlocal EnableExtensions
rem Generated from the manual ship. Finds java.exe and, without a local assembly, asks cs fetch.
set "HISTOS_BOOT_ROOT=%~dp0"
set "HISTOS_BOOT_VERSION=__VERSION__"
for /f "delims=" %%T in ('powershell.exe -NoLogo -NoProfile -Command "[IO.Path]::Combine([IO.Path]::GetTempPath(), 'histos-' + [Guid]::NewGuid().ToString('N'))"') do set "HISTOS_BOOT_TEMP=%%T"
if not defined HISTOS_BOOT_TEMP exit /b 1
mkdir "%HISTOS_BOOT_TEMP%"
powershell.exe -NoLogo -NoProfile -Command "$ErrorActionPreference='Stop'; $text=[IO.File]::ReadAllText($env:HISTOS_BOOT_ROOT+'histos.bat').Replace([string][char]13,''); $marker=[char]10+'# HISTOS_POWERSHELL'+[char]10; $body=$text.Substring($text.LastIndexOf($marker)+$marker.Length); [IO.File]::WriteAllText($env:HISTOS_BOOT_TEMP+'\bootstrap.ps1',$body)"
if errorlevel 1 (
  if exist "%HISTOS_BOOT_TEMP%" rmdir /s /q "%HISTOS_BOOT_TEMP%"
  exit /b 1
)
powershell.exe -NoLogo -NoProfile -ExecutionPolicy Bypass -File "%HISTOS_BOOT_TEMP%\bootstrap.ps1" %*
set "HISTOS_EXIT=%ERRORLEVEL%"
if exist "%HISTOS_BOOT_TEMP%" rmdir /s /q "%HISTOS_BOOT_TEMP%"
exit /b %HISTOS_EXIT%
# HISTOS_POWERSHELL
$ErrorActionPreference = 'Stop'
$launchArgs = $args
$root = $env:HISTOS_BOOT_ROOT
$version = $env:HISTOS_BOOT_VERSION
$javaGuide = "A JDK 21 or newer is required. Histos does not install it.`nmacOS: brew install --cask temurin@21`nLinux and Windows: https://adoptium.net/temurin/releases?version=21"
$csGuide = "The cs command from Coursier 2.1 or newer is required. Histos does not install it.`nmacOS: brew install coursier/formulas/coursier`nThe Homebrew core formula provides coursier and does not.`nLinux and Windows: https://get-coursier.io/docs/cli-installation and put the cs binary on PATH."
function Fail([string] $message) {
  [Console]::Error.WriteLine('histos ' + $version + ': ' + $message)
  exit 1
}
function Hash([string] $path) { (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant() }
function Pointer([string] $value) {
  return $value -match '^[0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9]+([.-][A-Za-z0-9]+)*)?-SNAPSHOT$'
}
function StampOk([string] $value) {
  if ($value -notmatch '^(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})(\d{2})$') { return $false }
  try { $parsed = [datetime]::ParseExact($value, 'yyyyMMdd.HHmmss', $null); return $parsed.Year -gt 0 } catch { return $false }
}
$java = $null
if ($env:JAVA_HOME) {
  $candidate = Join-Path $env:JAVA_HOME 'bin\java.exe'
  if (Test-Path -LiteralPath $candidate -PathType Leaf) { $java = $candidate }
}
if (-not $java) {
  $foundJava = Get-Command java.exe -ErrorAction SilentlyContinue
  if ($foundJava) { $java = $foundJava.Source }
}
if (-not $java) { Fail "java is not on PATH`n$javaGuide" }
$javaText = & $java -version 2>&1 | Out-String
if ($javaText -match 'version "([^"]+)"') {
  $javaFound = $Matches[1]
  if ($javaFound -match '^1\.(\d+)') { $javaMajor = [int]$Matches[1] }
  elseif ($javaFound -match '^(\d+)') { $javaMajor = [int]$Matches[1] }
  else { Fail "java version is unreadable`n$javaGuide" }
  if ($javaMajor -lt 21) { Fail "java $javaFound is older than 21`n$javaGuide" }
} else { Fail "java version is unreadable`n$javaGuide" }
$jar = $null
if ($env:HISTOS_ASSEMBLY) { $jar = $env:HISTOS_ASSEMBLY }
elseif (Test-Path -LiteralPath (Join-Path $root 'dist\histos.jar') -PathType Leaf) {
  $jar = Join-Path $root 'dist\histos.jar'
}
if ($jar) {
  $sumPath = Join-Path $root '.histos\assembly.sha256'
  if (-not (Test-Path -LiteralPath $sumPath -PathType Leaf)) { Fail 'missing assembly.sha256' }
  $expected = ([IO.File]::ReadAllText($sumPath) -replace '\s','').ToLowerInvariant()
  if ($expected -notmatch '^[0-9a-f]{64}$') { Fail 'invalid assembly checksum' }
  if ((Hash $jar) -ne $expected) { Fail 'checksum mismatch' }
} else {
  $props = Join-Path $root '.histos\build.properties'
  if (-not (Test-Path -LiteralPath $props -PathType Leaf)) {
    Fail 'missing build.properties; restore the committed version or use a source assembly'
  }
  $line = $null
  foreach ($raw in [IO.File]::ReadAllLines($props)) {
    $trim = $raw.Trim().TrimEnd([char]13)
    if ($trim -eq '' -or $trim.StartsWith('#')) { continue }
    $eq = $trim.IndexOf('=')
    if ($eq -le 0) { Fail 'invalid build.properties' }
    $key = $trim.Substring(0, $eq).Trim()
    $value = $trim.Substring($eq + 1).Trim()
    if ($key -ne 'histos.version' -or $value -eq '' -or $null -ne $line) { Fail 'invalid build.properties' }
    $line = $value
  }
  if (-not $line) { Fail 'invalid build.properties' }
  $channel = $null
  if (Pointer $line) { $channel = $line }
  elseif ($line -match '^(.+)-(\d{8}\.\d{6})-([1-9]\d*)$' -and (StampOk $Matches[2]) -and (Pointer ($Matches[1] + '-SNAPSHOT'))) {
    $channel = $Matches[1] + '-SNAPSHOT'
  }
  if (-not $channel) { Fail 'invalid build.properties' }
  $release = -not $version.EndsWith('-SNAPSHOT')
  if (-not ($channel -eq $version -or ($release -and $channel -eq ($version + '-SNAPSHOT')))) {
    Fail 'invalid build.properties'
  }
  $repo = if ($env:HISTOS_LAUNCHER_REPO) { $env:HISTOS_LAUNCHER_REPO } else { 'https://central.sonatype.com/repository/maven-snapshots' }
  if ($repo -match '\s' -or -not ($repo -match '^https://' -or $repo -match '^http://127\.0\.0\.1(:|/|$)' -or $repo -match '^http://localhost(:|/|$)' -or $repo -match '^file:///')) {
    Fail "launcher repository is not a supported base: $repo"
  }
  $csCmd = Get-Command cs -ErrorAction SilentlyContinue
  if (-not $csCmd) { Fail "coursier is not on PATH`n$csGuide" }
  $csText = & $csCmd.Source version 2>&1 | Out-String
  if ($csText -match '(\d+)\.(\d+)') {
    $csMajor = [int]$Matches[1]
    $csMinor = [int]$Matches[2]
    $csFound = $Matches[0]
    if ($csText -match '(\d+\.\d+\.\d+)') { $csFound = $Matches[1] }
    if ($csMajor -lt 2 -or ($csMajor -eq 2 -and $csMinor -lt 1)) { Fail "coursier $csFound is older than 2.1`n$csGuide" }
  } else { Fail "coursier version is unreadable`n$csGuide" }
  if ($line.EndsWith('-SNAPSHOT')) { $env:COURSIER_TTL = '0s' } else { Remove-Item Env:COURSIER_TTL -ErrorAction SilentlyContinue }
  $coord = 'rocks.earlyeffect:histos:' + $line
  $raw = & $csCmd.Source fetch --no-default -r $repo $coord | Out-String
  if ($LASTEXITCODE -ne 0) { Fail 'launcher fetch failed' }
  $paths = @($raw -split "`r?`n" | Where-Object { $_.Trim() -ne '' })
  if ($paths.Count -ne 1) { Fail 'launcher fetch did not print one jar' }
  $jar = $paths[0].Trim()
}
$jvmArgs = @()
$opts = Join-Path $root '.jvmopts'
if (Test-Path -LiteralPath $opts) {
  if (-not (Test-Path -LiteralPath $opts -PathType Leaf)) { Fail "invalid file path: $opts" }
  try { $lines = [IO.File]::ReadAllLines($opts) } catch { Fail "invalid file path: $opts" }
  foreach ($optLine in $lines) {
    $text = $optLine.TrimEnd([char]13)
    if ($text -eq '' -or $text.StartsWith('#')) { continue }
    if ($text -eq '-jar' -or $text -eq '-m' -or $text -eq '--module' -or $text.StartsWith('@')) {
      Fail "jvm option selects the program: $text"
    }
    $jvmArgs += $text
  }
}
& $java @jvmArgs --enable-native-access=ALL-UNNAMED -jar $jar @launchArgs
exit $LASTEXITCODE
"""
end LauncherScript

enum ScriptError:
  case Drift(path: Path)
  case Missing(path: Path)
  case Io(failure: IoFailure)
  case Revision
  case Pin(error: LauncherError)

  def message: String = this match
    case Drift(path)   => s"launcher script does not match the ship: $path"
    case Missing(path) => s"no launcher script at $path"
    case Io(failure)   => failure.message
    case Pin(error)    => error.message
    case Revision      => "the launcher script needs one manual ship version"
end ScriptError
