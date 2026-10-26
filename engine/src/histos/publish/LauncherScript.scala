package histos.publish

import histos.version.Catalog
import histos.workspace.{IoFailure, IoFault}
import histos.ProjectGraph

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** `histos` and `histos.bat`, rendered from the manual ship the way a workflow file is. Each script finds `java` and
  * runs the pinned launcher jar. Scalac runs in that process, so the thread stack is 4MB: the Linux default overflows
  * in CrossStageSafety. Histos does not install a JDK.
  */
object LauncherScript:
  val ThreadStack: String = "-Xss4m"

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
              if pin.version != version then ZIO.fail(ScriptError.Pin(LauncherError.Version(pin.version)))
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
              if pin.version != version then ZIO.fail(ScriptError.Pin(LauncherError.Version(pin.version)))
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
    render(Posix, version)

  def windows(version: String): String =
    render(Windows, version)

  private def render(template: String, version: String): String =
    template.replace("__VERSION__", version).replace("__STACK__", ThreadStack)

  private def ship(graph: ProjectGraph): Either[ScriptError, Option[String]] =
    graph.assembly match
      case None    => Right(None)
      case Some(_) =>
        graph.catalog match
          case Catalog.Manual(rows) =>
            rows.toList.map(_.version.value).distinct match
              case List(version) => Right(Some(version))
              case _             => Left(ScriptError.Revision)
          case _ => Left(ScriptError.Revision)

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
# Source assemblies and remote launchers have independent committed digests.
set -eu
umask 077
root=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
version=__VERSION__
cache=${XDG_CACHE_HOME:-${HOME:?HOME is unset}/.cache}/histos
work=
cleanup() { if [ -n "$work" ]; then rm -rf "$work"; fi; }
trap cleanup EXIT
trap 'exit 1' HUP INT TERM
fail() { echo "histos $version: $*" >&2; exit 1; }
hash_of() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | awk '{print $1}'
  else shasum -a 256 "$1" | awk '{print $1}'; fi
}
valid_sha() { printf '%s\n' "$1" | awk 'length($0)==64 && $0 !~ /[^0-9a-fA-F]/ {ok=1} END {exit !ok}'; }
file_mode() {
  if stat -c '%u %a' "$1" >/dev/null 2>&1; then stat -c '%u %a' "$1"; else stat -f '%u %Lp' "$1"; fi
}
private_dir() {
  [ ! -L "$1" ] || fail "unsafe cache link: $1"
  mkdir -p "$1"
  file_mode "$1" | awk -v uid="$(id -u)" '{g=substr($2,length($2)-1,1)+0; o=substr($2,length($2),1)+0; if($1!=uid || int(g/2)%2 || int(o/2)%2) exit 1}' || fail "cache is not owner-private: $1"
  chmod 700 "$1"
  ancestor=$(CDPATH= cd -P -- "$(dirname "$1")" && pwd) || fail "invalid cache ancestor"
  while :; do
    if stat -Lc '%u %a' "$ancestor" >/dev/null 2>&1; then info=$(stat -Lc '%u %a' "$ancestor"); else info=$(stat -Lf '%u %Lp' "$ancestor"); fi
    printf '%s\n' "$info" | awk -v uid="$(id -u)" '{m=$2; g=substr(m,length(m)-1,1)+0; o=substr(m,length(m),1)+0; special=length(m)>3 ? substr(m,length(m)-3,1)+0 : 0; if(($1!=uid && $1!=0) || ((int(g/2)%2 || int(o/2)%2) && special%2!=1)) exit 1}' || fail "unsafe cache ancestor: $ancestor"
    [ "$ancestor" != / ] || break
    ancestor=$(dirname "$ancestor")
  done
}
present() { [ -e "$1" ] || [ -L "$1" ]; }
regular() { [ -f "$1" ] && [ -r "$1" ] || fail "invalid file path: $1"; }
valid_url() {
  printf '%s\n' "$1" | LC_ALL=C awk -v kind="$2" '
    function ipv4(s, p,n,i) {n=split(s,p,"."); if(n!=4) return 0; for(i=1;i<=4;i++) if(p[i]!~/^[0-9]+$/ || p[i]+0>255) return 0; return 1}
    function hostok(s,p,n,i,c,compressed) {
      if(substr(s,1,1)=="[") {
        if(substr(s,length(s),1)!="]") return 0
        s=substr(s,2,length(s)-2); compressed=index(s,"::"); if(compressed && index(substr(s,compressed+2),"::")) return 0
        n=split(s,p,":"); c=0
        for(i=1;i<=n;i++) if(p[i]!="") {if(index(p[i],".")) {if(i!=n || !ipv4(p[i])) return 0; c+=2} else {if(length(p[i])>4 || p[i]!~/^[0-9a-fA-F]+$/) return 0; c++}}
        return compressed ? c<8 : c==8 && substr(s,1,1)!=":" && substr(s,length(s),1)!=":"
      }
      if(s~/^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$/) return ipv4(s)
      sub(/\.$/,"",s); n=split(s,p,"."); for(i=1;i<=n;i++) if(p[i]!~/^[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?$/) return 0
      return n>0
    }
    {
      if($0 ~ /[^!-~]/ || $0 ~ /["<>\\^`{|}#]/) exit 1
      raw=$0
      for(i=1;i<=length(raw);i++) if(substr(raw,i,1)=="%") {if(substr(raw,i+1,2)!~/^[0-9a-fA-F][0-9a-fA-F]$/) exit 1; i+=2}
      scheme=raw; sub(/:.*/,"",scheme)
      if(scheme!="https" && !(scheme=="http" && kind=="repository")) exit 1
      rest=raw; if(rest!~/^[a-z]+:\/\//) exit 1; sub(/^[a-z]+:\/\//,"",rest)
      authority=rest; sub(/[\/?].*$/,"",authority); suffix=substr(rest,length(authority)+1)
      if(authority ~ /[@%]/ || suffix~/[\[\]]/) exit 1
      host=authority; port=""
      if(substr(host,1,1)=="[") {bracket=index(host,"]"); if(!bracket) exit 1; port=substr(host,bracket+1); host=substr(host,1,bracket)}
      else if(index(host,":")) {port=substr(host,index(host,":")); host=substr(host,1,index(host,":")-1)}
      if(port!="" && (port!~/^:[0-9]+$/ || substr(port,2)+0<1 || substr(port,2)+0>65535)) exit 1
      if(!hostok(host)) exit 1
      if(scheme=="http" && host!="localhost" && host!="127.0.0.1" && host!="[::1]") exit 1
      if(kind=="repository" && (suffix~/[?%]/ || suffix~/(^|\/)\.\.(\/|$)/)) exit 1
      ok=1
    }
    END {exit !ok}'
}
parse_lock() {
  LC_ALL=C awk '
    function bad() {failed=1; exit 1}
    function integer(s) {return s ~ /^[+]?[0-9]+$/ && s+0>0 && s+0<=2147483647}
    function stamp(s, y,m,d,h,n,t,days) {
      if(length(s)!=15 || substr(s,9,1)!="." || substr(s,1,8) !~ /^[0-9]+$/ || substr(s,10,6) !~ /^[0-9]+$/) return 0
      y=substr(s,1,4)+0; m=substr(s,5,2)+0; d=substr(s,7,2)+0
      h=substr(s,10,2)+0; n=substr(s,12,2)+0; t=substr(s,14,2)+0
      days=(m==2 ? 28+(y%4==0 && (y%100!=0 || y%400==0)) : (m==4 || m==6 || m==9 || m==11 ? 30 : 31))
      return y>=1 && m>=1 && m<=12 && d>=1 && d<=days && h<24 && n<60 && t<60
    }
    {sub(/^[ \t\r]+/,""); sub(/[ \t\r]+$/,"")}
    /^$/ || /^#/ {next}
    {
      if($0 !~ /^[a-z0-9]+[ \t]+[^ \t].*$/) bad()
      name=$1; value=$0; sub(/^[^ \t]+[ \t]+/,"",value)
      if(name!="repository" && name!="version" && name!="revision" && name!="sha256") bad()
      if(seen[name]++) bad()
      fields[name]=value
    }
    END {
      if(failed) exit 1
      if(length(fields["sha256"])!=64 || fields["sha256"] ~ /[^0-9a-fA-F]/) exit 1
      if(fields["repository"]=="" || fields["version"] !~ /^[0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9]+([.-][A-Za-z0-9]+)*)?-SNAPSHOT$/) exit 1
      base=fields["version"]; sub(/-SNAPSHOT$/,"",base); prefix=base "-"
      if(substr(fields["revision"],1,length(prefix))!=prefix) exit 1
      tail=substr(fields["revision"],length(prefix)+1); build=substr(tail,17)
      if(substr(tail,16,1)!="-" || build !~ /^[1-9][0-9]*$/ || !integer(build) || !stamp(substr(tail,1,15))) exit 1
      print fields["repository"]; print fields["version"]; print fields["revision"]; print tolower(fields["sha256"])
    }' "$1" > "$work/fields" || fail "invalid launcher lock"
}
private_dir "$cache"
work=$(mktemp -d "$cache/.bootstrap.XXXXXX")
fetch() {
  valid_url "$1" repository || fail "invalid download url"
  curl -fsSL --proto '=https,http' --proto-redir '=https' "$1" -o "$2"
}
if present "$root/.histos"; then
  [ -d "$root/.histos" ] && [ -r "$root/.histos" ] && [ -x "$root/.histos" ] || fail "invalid configuration directory"
fi
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
  regular "$root/.histos/launcher.lock"
  [ -f "$root/.histos/launcher.lock" ] || fail "missing launcher.lock; restore the committed pin or use a source assembly"
  parse_lock "$root/.histos/launcher.lock"
  { IFS= read -r repository; IFS= read -r locked_version; IFS= read -r revision; IFS= read -r expected; } < "$work/fields"
  [ "$version" = "$locked_version" ] || fail "launcher lock version does not match script"
  valid_url "$repository" repository || fail "invalid launcher repository"
  local_jar=$root/.histos/launcher-cache/$expected/histos.jar
  dest=$cache/launchers/$revision/$expected
  private_dir "$dest"
  jar=$dest/histos.jar
  if [ -f "$local_jar" ] && [ "$(hash_of "$local_jar")" = "$expected" ]; then jar=$local_jar
  elif [ ! -f "$jar" ] || [ "$(hash_of "$jar")" != "$expected" ]; then
    fetch "${repository%/}/rocks/earlyeffect/histos/$locked_version/histos-$revision.jar" "$work/launcher.jar" || fail "launcher download failed"
    [ "$(hash_of "$work/launcher.jar")" = "$expected" ] || fail "launcher checksum mismatch"
    mv -f "$work/launcher.jar" "$jar"
  fi
  [ "$(hash_of "$jar")" = "$expected" ] || fail "launcher checksum mismatch"
fi
java_bin=$(command -v java) || fail "java is not on PATH"
cleanup
trap - EXIT HUP INT TERM
exec "$java_bin" __STACK__ --enable-native-access=ALL-UNNAMED -jar "$jar" "$@"
"""

  private val Windows: String = """@echo off
setlocal EnableExtensions
rem Generated from the manual ship. Finds java.exe and runs the pinned launcher jar.
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
function Fail([string] $message) {
  [Console]::Error.WriteLine('histos ' + $version + ': ' + $message)
  exit 1
}
function Hash([string] $path) { (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant() }
$java = $null
if ($env:JAVA_HOME) {
  $candidate = Join-Path $env:JAVA_HOME 'bin\java.exe'
  if (Test-Path -LiteralPath $candidate -PathType Leaf) { $java = $candidate }
}
if (-not $java) {
  $found = Get-Command java.exe -ErrorAction SilentlyContinue
  if ($found) { $java = $found.Source }
}
if (-not $java) { Fail 'java is not on PATH' }
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
  $lockPath = Join-Path $root '.histos\launcher.lock'
  if (-not (Test-Path -LiteralPath $lockPath -PathType Leaf)) {
    Fail 'missing launcher.lock; restore the committed pin or use a source assembly'
  }
  $fields = @{}
  foreach ($line in [IO.File]::ReadAllLines($lockPath)) {
    $trim = $line.Trim()
    if ($trim -eq '' -or $trim.StartsWith('#')) { continue }
    $parts = $trim -split '\s+', 2
    if ($parts.Count -ne 2 -or $fields.ContainsKey($parts[0])) { Fail 'invalid launcher lock' }
    $fields[$parts[0]] = $parts[1]
  }
  foreach ($name in @('repository','version','revision','sha256')) {
    if (-not $fields.ContainsKey($name)) { Fail 'invalid launcher lock' }
  }
  if ($fields['version'] -ne $version) { Fail 'launcher lock version does not match script' }
  $expected = $fields['sha256'].ToLowerInvariant()
  if ($expected -notmatch '^[0-9a-f]{64}$') { Fail 'invalid launcher lock' }
  $repository = $fields['repository']
  if ($repository -notmatch '^https://' -and $repository -notmatch '^http://(127\.0\.0\.1|localhost|\[::1\])([:/]|$)') {
    Fail 'invalid launcher repository'
  }
  $cacheRoot = if ($env:XDG_CACHE_HOME) { $env:XDG_CACHE_HOME } else { Join-Path $env:USERPROFILE '.cache' }
  $dest = Join-Path (Join-Path (Join-Path $cacheRoot 'histos\launchers') $fields['revision']) $expected
  $cached = Join-Path $dest 'histos.jar'
  $staged = Join-Path (Join-Path (Join-Path $root '.histos\launcher-cache') $expected) 'histos.jar'
  if ((Test-Path -LiteralPath $staged -PathType Leaf) -and (Hash $staged) -eq $expected) { $jar = $staged }
  elseif ((Test-Path -LiteralPath $cached -PathType Leaf) -and (Hash $cached) -eq $expected) { $jar = $cached }
  else {
    New-Item -ItemType Directory -Force -Path $dest | Out-Null
    $part = Join-Path $dest 'histos.jar.partial'
    $address = $repository.TrimEnd('/') + '/rocks/earlyeffect/histos/' + $fields['version'] + '/histos-' + $fields['revision'] + '.jar'
    Invoke-WebRequest -Uri $address -OutFile $part
    if ((Hash $part) -ne $expected) {
      Remove-Item -LiteralPath $part -Force
      Fail 'launcher checksum mismatch'
    }
    Move-Item -LiteralPath $part -Destination $cached -Force
    $jar = $cached
  }
  if ((Hash $jar) -ne $expected) { Fail 'launcher checksum mismatch' }
}
& $java __STACK__ --enable-native-access=ALL-UNNAMED -jar $jar @launchArgs
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
