package histos.publish

import histos.version.Catalog
import histos.workspace.{IoFailure, IoFault}
import histos.ProjectGraph

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** `histos` and `histos.bat`, rendered from the manual ship the way a workflow
  * file is.
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
              if pin.version != version then
                ZIO.fail(ScriptError.Pin(LauncherError.Version(pin.version)))
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
              if pin.version != version then
                ZIO.fail(ScriptError.Pin(LauncherError.Version(pin.version)))
              else
                write(
                  repo.resolve("histos"),
                  posix(version),
                  executable = true
                ) *>
                  write(
                    repo.resolve("histos.bat"),
                    windows(version),
                    executable = false
                  )

  def posix(version: String): String =
    Posix.replace("__VERSION__", version)

  def windows(version: String): String =
    Windows.replace("__VERSION__", version)

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
      executable: Boolean
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
install=
cleanup() { rm -rf "$work"; if [ -n "$install" ]; then rm -rf "$install"; fi; }
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
host_key() {
  os=$(uname -s) || fail "could not identify runtime OS"
  cpu=$(uname -m | tr '[:upper:]' '[:lower:]' | awk '{print $1}') || fail "could not identify runtime architecture"
  case $os in Darwin) os=macos ;; Linux) os=linux ;; *) os=unsupported ;; esac
  case $cpu in x86_64|amd64) cpu=x86_64 ;; aarch64|arm64) cpu=aarch64 ;; *) cpu=unsupported ;; esac
  printf '%s-%s\n' "$os" "$cpu"
}
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
      if(kind=="archive" && raw~/^file:\//) {
        path=substr(raw,6); if(path~/^\/\// && path!~/^\/\/\//) exit 1
        if(path~/^\/\/\//) path=substr(path,3)
        if(path~/^\/\// || path~/\?/ || tolower(path)~/%(00|0[1-9a-f]|1[0-9a-f]|7f)/) exit 1
        if(path!~/^\// || path~/[\[\]]/) exit 1
        ok=1; exit
      }
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
validate_graph() {
  LC_ALL=C awk -F '\t' '
    function canonical(s, p,n,i,result) {n=split(s,p,"/"); result=""; for(i=1;i<=n;i++) if(p[i]!="" && p[i]!=".") result=result (result=="" ? "" : "/") p[i]; return result}
    function resolve(s,hops,hard,p,n,i,stack,depth,name,base,rest,j) {
      if(hops>count+1 || s~/^\// || s~/[\\:[:space:]]/) return "#unsafe"
      n=split(s,p,"/"); depth=0
      for(i=1;i<=n;i++) {
        if(p[i]=="" || p[i]==".") continue
        if(p[i]=="..") {if(!depth) return "#unsafe"; depth--; continue}
        base=""; for(j=1;j<=depth;j++) base=base (j==1 ? "" : "/") stack[j]
        name=base (base=="" ? "" : "/") p[i]
        if(type[name]=="l" || type[name]=="h") {
          if(hard && type[name]=="l") return "#unsafe"
          rest=""; for(j=i+1;j<=n;j++) rest=rest "/" p[j]
          return resolve((type[name]=="h" ? "" : base (base=="" ? "" : "/")) target[name] rest,hops+1,hard)
        }
        stack[++depth]=p[i]
      }
      name=""; for(j=1;j<=depth;j++) name=name (j==1 ? "" : "/") stack[j]
      return name
    }
    {
      name=canonical($2)
      if($2~/^\// || $2~/(^|\/)\.\.(\/|$)/ || $2~/[\\:[:space:]]/ || (name=="" && $1!="d") || seen[name]++) {failed=1; exit 1}
      type[name]=$1; target[name]=$3; count++
    }
    END {
      if(failed) exit 1
      for(name in type) {
        for(link in type) if((type[link]=="l" || type[link]=="h") && index(name,link "/")==1) exit 1
        if(type[name]=="l" || type[name]=="h") {
          base=name; sub(/[^\/]*$/,"",base)
          resolved=resolve((type[name]=="h" ? "" : base) target[name],0,type[name]=="h")
          if(resolved=="#unsafe" || (resolved!="" && !(resolved in type)) || (type[name]=="h" && type[resolved]!="-")) exit 1
        }
      }
    }' "$1" || fail "unsafe jdk archive link graph"
}
runtime_hash() (
  home=$1
  [ -d "$home" ] && [ ! -L "$home" ] || fail "invalid installed runtime"
  find "$home" -print | LC_ALL=C sort > "$work/tree.paths"
  : > "$work/tree.inventory"
  : > "$work/tree.graph"
  while IFS= read -r item; do
    relative=${item#"$home"}; relative=${relative#/}
    printf '%s\n' "$relative" | LC_ALL=C awk '/[[:space:]\\:]/ {exit 1}' || fail "unsafe installed runtime name"
    mode=$(file_mode "$item")
    uid=${mode%% *}; mode=${mode#* }
    [ "$uid" = "$(id -u)" ] || fail "runtime is not owned by current user"
    if [ -L "$item" ]; then
      target=$(readlink "$item")
      printf 'l\t%s\t%s\n' "$relative" "$target" >> "$work/tree.graph"
      printf 'l\t%s\t%s\t%s\n' "$relative" "$mode" "$target" >> "$work/tree.inventory"
    elif [ -d "$item" ]; then
      printf 'd\t%s\n' "$relative" >> "$work/tree.graph"
      printf 'd\t%s\t%s\n' "$relative" "$mode" >> "$work/tree.inventory"
    elif [ -f "$item" ]; then
      printf -- '-\t%s\n' "$relative" >> "$work/tree.graph"
      printf 'f\t%s\t%s\t%s\n' "$relative" "$mode" "$(hash_of "$item")" >> "$work/tree.inventory"
    else fail "unsupported runtime entry"; fi
  done < "$work/tree.paths"
  validate_graph "$work/tree.graph"
  hash_of "$work/tree.inventory"
)

private_dir "$cache"
work=$(mktemp -d "$cache/.bootstrap.XXXXXX")
fetch() {
  case $1 in
    file:///*) valid_url "$1" archive || fail "invalid file url"; curl -fsS --proto '=file' "$1" -o "$2" ;;
    *) valid_url "$1" "${3:-archive}" || fail "invalid download url"; curl -fsSL --proto '=https,http' --proto-redir '=https' "$1" -o "$2" ;;
  esac
}
# Standalone lock validation mirrors the Scala parser before any field is used.
parse_lock() {
  LC_ALL=C awk -v kind="$2" '
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
      if(kind=="jdk" && name=="archive") {
        if(NF!=4 || $2 !~ /^(macos|linux|windows)-(x86_64|aarch64)$/ || hosts[$2]++ || length($4)!=64 || $4 ~ /[^0-9a-fA-F]/) bad()
        rows[++count]=$2 "\t" $3 "\t" tolower($4)
      } else {
        if(seen[name]++) bad()
        if(kind=="launcher" && name!="repository" && name!="version" && name!="revision" && name!="sha256") bad()
        if(kind=="jdk" && name!="major" && name!="version") bad()
        fields[name]=value
      }
    }
    END {
      if(failed) exit 1
      if(kind=="launcher") {
        if(length(fields["sha256"])!=64 || fields["sha256"] ~ /[^0-9a-fA-F]/) exit 1
        if(fields["repository"]=="" || fields["version"] !~ /^[0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9]+([.-][A-Za-z0-9]+)*)?-SNAPSHOT$/) exit 1
        base=fields["version"]; sub(/-SNAPSHOT$/,"",base); prefix=base "-"
        if(substr(fields["revision"],1,length(prefix))!=prefix) exit 1
        tail=substr(fields["revision"],length(prefix)+1); build=substr(tail,17)
        if(substr(tail,16,1)!="-" || build !~ /^[1-9][0-9]*$/ || !integer(build) || !stamp(substr(tail,1,15))) exit 1
        print fields["repository"]; print fields["version"]; print fields["revision"]; print tolower(fields["sha256"])
      } else {
        major=fields["major"]; v=fields["version"]
        if(!integer(major) || v !~ /^[0-9][A-Za-z0-9.+_-]*$/ || !count) exit 1
        if(v ~ /^1\./) v=substr(v,3); sub(/[^0-9].*$/, "",v)
        if(!integer(v) || major+0!=v+0) exit 1
        for(i=1;i<=count;i++) print rows[i]
      }
    }' "$1" > "$work/fields" || fail "invalid $2 lock"
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
  parse_lock "$root/.histos/launcher.lock" launcher
  { IFS= read -r repository; IFS= read -r locked_version; IFS= read -r revision; IFS= read -r expected; } < "$work/fields"
  [ "$version" = "$locked_version" ] || fail "launcher lock version does not match script"
  valid_url "$repository" repository || fail "invalid launcher repository"
  local_jar=$root/.histos/launcher-cache/$expected/histos.jar
  dest=$cache/launchers/$revision/$expected
  private_dir "$dest"
  jar=$dest/histos.jar
  if [ -f "$local_jar" ] && [ "$(hash_of "$local_jar")" = "$expected" ]; then jar=$local_jar
  elif [ ! -f "$jar" ] || [ "$(hash_of "$jar")" != "$expected" ]; then
    fetch "${repository%/}/rocks/earlyeffect/histos/$locked_version/histos-$revision.jar" "$work/launcher.jar" repository || fail "launcher download failed"
    [ "$(hash_of "$work/launcher.jar")" = "$expected" ] || fail "launcher checksum mismatch"
    mv -f "$work/launcher.jar" "$jar"
  fi
  [ "$(hash_of "$jar")" = "$expected" ] || fail "launcher checksum mismatch"
fi
safe_archive() {
  tar -tf "$1" > "$work/names" || fail "invalid jdk archive"
  LC_ALL=C awk '/(^\/|[[:space:]\\:]|(^|\/)\.\.(\/|$))/ {exit 1}' "$work/names" || fail "unsafe jdk archive path"
  tar -tvf "$1" > "$work/entries" || fail "invalid jdk archive"
  awk '
    {
      type=substr($0,1,1); name=$NF; target=""
      if(type=="l") {if($(NF-1)!="->") exit 1; name=$(NF-2); target=$NF}
      if(type=="h") {if($(NF-2)!="link" || $(NF-1)!="to") exit 1; name=$(NF-3); target=$NF}
      if(type!="-" && type!="d" && type!="l" && type!="h") exit 1
      printf "%s\t%s\t%s\n",type,name,target
    }' "$work/entries" > "$work/archive.graph" || fail "invalid jdk archive entries"
  validate_graph "$work/archive.graph"
}
jdk_present=
if present "$root/.histos/jdk.lock"; then
  regular "$root/.histos/jdk.lock"
  parse_lock "$root/.histos/jdk.lock" jdk
  while IFS="$(printf '\t')" read -r row_host row_url row_sha; do
    valid_url "$row_url" archive || fail "invalid jdk url for $row_host"
  done < "$work/fields"
  jdk_present=yes
fi
if present "$root/.histos/toolchain.local"; then
  regular "$root/.histos/toolchain.local"
  home=$(awk '{sub(/\r$/,""); sub(/^[[:space:]]+/,""); sub(/[[:space:]]+$/,""); if(length) {n++; path=$0}} END {if(n!=1) exit 1; print path}' "$root/.histos/toolchain.local") || fail "invalid toolchain.local"
  case $home in /*) ;; *) fail "toolchain.local must be absolute" ;; esac
  java_bin=$home/bin/java
  [ -x "$java_bin" ] || fail "toolchain.local has no bin/java"
elif [ "$jdk_present" = yes ]; then
  host=$(host_key)
  url=; sha=
  while IFS="$(printf '\t')" read -r row_host row_url row_sha; do
    if [ "$row_host" = "$host" ]; then url=$row_url; sha=$row_sha; fi
  done < "$work/fields"
  [ -n "$url" ] || fail "no archive for runtime host $host"
  case $url in file:/*) case $url in file:///*) ;; *) url=file://${url#file:} ;; esac ;; esac
  dest=$cache/jdks/$sha
  private_dir "$dest"
  archive=$dest/archive
  if [ ! -f "$archive" ] || [ "$(hash_of "$archive")" != "$sha" ]; then
    fetch "$url" "$work/jdk.archive" || fail "jdk download failed"
    [ "$(hash_of "$work/jdk.archive")" = "$sha" ] || fail "jdk checksum mismatch"
    mv -f "$work/jdk.archive" "$archive"
  fi
  [ "$(hash_of "$archive")" = "$sha" ] || fail "jdk checksum mismatch"
  java_bin=
  for parent in "$dest"/install.*; do
    [ -f "$parent/runtime.sha256" ] && [ -f "$parent/java.path" ] || continue
    private_dir "$parent"
    regular "$parent/runtime.sha256"
    relative=$(cat "$parent/java.path")
    case $relative in *..*|/*) continue ;; esac
    expected_tree=$(cat "$parent/runtime.sha256")
    valid_sha "$expected_tree" || continue
    if actual_tree=$(runtime_hash "$parent/home") && [ "$actual_tree" = "$expected_tree" ] && [ -x "$parent/home/$relative" ]; then
      java_bin=$parent/home/$relative; break
    fi
  done
  if [ -z "$java_bin" ]; then
    safe_archive "$archive"
    mkdir "$work/unpack"
    tar --no-same-owner --no-same-permissions -xf "$archive" -C "$work/unpack" || fail "jdk extraction failed"
    found=$(find "$work/unpack" -type f -path '*/bin/java')
    [ "$(printf '%s\n' "$found" | awk 'NF {n++} END {print n+0}')" = 1 ] || fail "locked jdk must have one bin/java"
    relative=${found#"$work/unpack/"}
    find "$work/unpack" -type d -exec chmod 700 {} +
    find "$work/unpack" -type f -exec chmod u+rw,go-rwx,u-s,g-s,o-t {} +
    chmod u+x "$found"
    install=$(mktemp -d "$dest/install.XXXXXX")
    runtime_hash "$work/unpack" > "$install/runtime.sha256"
    printf '%s\n' "$relative" > "$install/java.path"
    mv "$work/unpack" "$install/home"
    java_bin=$install/home/$relative
    install=
  fi
else
  java_bin=$(command -v java) || fail "java is not on PATH and .histos/jdk.lock is missing"
fi
cleanup
trap - EXIT HUP INT TERM
exec "$java_bin" --enable-native-access=ALL-UNNAMED -jar "$jar" "$@"
"""

  private val Windows: String = """@echo off
setlocal EnableExtensions DisableDelayedExpansion
rem Generated from the manual ship. Requires Windows PowerShell 5.1 and tar for tar archives.
set "HISTOS_BOOT_ROOT=%~dp0"
set "HISTOS_BOOT_VERSION=__VERSION__"
for /f "delims=" %%T in ('powershell.exe -NoLogo -NoProfile -Command "[IO.Path]::Combine([IO.Path]::GetTempPath(), 'histos-' + [Guid]::NewGuid().ToString('N'))"') do set "HISTOS_BOOT_TEMP=%%T"
if not defined HISTOS_BOOT_TEMP exit /b 1
powershell.exe -NoLogo -NoProfile -Command "$ErrorActionPreference='Stop'; $text=[IO.File]::ReadAllText($env:HISTOS_BOOT_ROOT+'histos.bat').Replace([string][char]13,''); $marker=[char]10+'# HISTOS_POWERSHELL'+[char]10; $body=$text.Substring($text.LastIndexOf($marker)+$marker.Length); $owner=[Security.Principal.WindowsIdentity]::GetCurrent().User; $acl=New-Object Security.AccessControl.DirectorySecurity; $acl.SetOwner($owner); $acl.SetAccessRuleProtection($true,$false); $rule=New-Object Security.AccessControl.FileSystemAccessRule($owner,'FullControl','ContainerInherit,ObjectInherit','None','Allow'); $acl.AddAccessRule($rule); [IO.Directory]::CreateDirectory($env:HISTOS_BOOT_TEMP,$acl) | Out-Null; [IO.File]::WriteAllText($env:HISTOS_BOOT_TEMP+'\bootstrap.ps1',$body)"
if errorlevel 1 (
  if exist "%HISTOS_BOOT_TEMP%" rmdir /s /q "%HISTOS_BOOT_TEMP%"
  exit /b 1
)
powershell.exe -NoLogo -NoProfile -ExecutionPolicy Bypass -File "%HISTOS_BOOT_TEMP%\bootstrap.ps1" %*
set "HISTOS_EXIT=%ERRORLEVEL%"
if exist "%HISTOS_BOOT_TEMP%\work-path" (
  set /p HISTOS_BOOT_WORK=<"%HISTOS_BOOT_TEMP%\work-path"
  call :cleanup_work
)
if exist "%HISTOS_BOOT_TEMP%" rmdir /s /q "%HISTOS_BOOT_TEMP%"
exit /b %HISTOS_EXIT%
:cleanup_work
if defined HISTOS_BOOT_WORK if exist "%HISTOS_BOOT_WORK%" rmdir /s /q "%HISTOS_BOOT_WORK%"
exit /b 0
# HISTOS_POWERSHELL
$ErrorActionPreference = 'Stop'
$launchArgs = $args
$root = $env:HISTOS_BOOT_ROOT
$version = $env:HISTOS_BOOT_VERSION
$cacheRoot = if ($env:XDG_CACHE_HOME) { $env:XDG_CACHE_HOME } else { Join-Path $env:USERPROFILE '.cache' }
$cache = Join-Path $cacheRoot 'histos'
$work = Join-Path $cache ('.bootstrap.' + [Guid]::NewGuid().ToString('N'))
$install = $null
$exitCode = 1
function Fail([string] $message) { Write-Error ('histos ' + $version + ': ' + $message) }
function Hash([string] $path) { (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant() }
$owner = [Security.Principal.WindowsIdentity]::GetCurrent().User
function Entry([string] $path) {
  $parent = [IO.Path]::GetDirectoryName($path)
  try { $attributes=[IO.File]::GetAttributes($parent) }
  catch [IO.DirectoryNotFoundException] { $false; return }
  catch [IO.FileNotFoundException] { $false; return }
  if (!($attributes -band [IO.FileAttributes]::Directory)) { Fail 'invalid configuration directory' }
  @([IO.Directory]::GetFileSystemEntries($parent) | Where-Object { [string]::Equals($_,$path,[StringComparison]::OrdinalIgnoreCase) }).Count -gt 0
}
function Regular([string] $path) {
  if (!(Entry $path) -or ![IO.File]::Exists($path) -or ([IO.File]::GetAttributes($path) -band [IO.FileAttributes]::ReparsePoint)) { Fail ('invalid file path: ' + $path) }
  $stream = [IO.File]::OpenRead($path); $stream.Dispose()
}
function Private([string] $path) {
  if ([IO.File]::GetAttributes($path) -band [IO.FileAttributes]::ReparsePoint) { Fail 'unsafe cache link' }
  $acl = Get-Acl -LiteralPath $path
  if ($acl.GetOwner([Security.Principal.SecurityIdentifier]) -ne $owner) { Fail ('cache is not owned by current user: '+$path) }
  $write = [Security.AccessControl.FileSystemRights]::Write -bor [Security.AccessControl.FileSystemRights]::Delete -bor [Security.AccessControl.FileSystemRights]::ChangePermissions -bor [Security.AccessControl.FileSystemRights]::TakeOwnership
  foreach ($rule in $acl.Access) {
    $sid = $rule.IdentityReference.Translate([Security.Principal.SecurityIdentifier]).Value
    if ($rule.AccessControlType -eq 'Allow' -and $sid -notin @($owner.Value,'S-1-5-18','S-1-5-32-544') -and ($rule.FileSystemRights -band $write)) { Fail 'unsafe shared cache ACL' }
  }
  $acl.SetAccessRuleProtection($true,$false)
  foreach ($rule in @($acl.Access)) { $acl.RemoveAccessRuleSpecific($rule) }
  $flags = if ([IO.Directory]::Exists($path)) { [Security.AccessControl.InheritanceFlags]'ContainerInherit,ObjectInherit' } else { [Security.AccessControl.InheritanceFlags]::None }
  $rule = New-Object Security.AccessControl.FileSystemAccessRule($owner,'FullControl',$flags,'None','Allow')
  $acl.AddAccessRule($rule)
  Set-Acl -LiteralPath $path -AclObject $acl
}
function OwnedAcl {
  $acl=New-Object Security.AccessControl.DirectorySecurity
  $acl.SetOwner($owner); $acl.SetAccessRuleProtection($true,$false)
  $rule=New-Object Security.AccessControl.FileSystemAccessRule($owner,'FullControl','ContainerInherit,ObjectInherit','None','Allow')
  $acl.AddAccessRule($rule)
  $acl
}
function PrivateDirectory([string] $path) {
  if (!(Entry $path)) { [IO.Directory]::CreateDirectory($path,(OwnedAcl)) | Out-Null }
  Private $path
  $ancestor=[IO.Directory]::GetParent([IO.Path]::GetFullPath($path))
  while ($ancestor) {
    $acl=Get-Acl -LiteralPath $ancestor.FullName
    $trusted=@($owner.Value,'S-1-5-18','S-1-5-32-544','S-1-5-80-956008885-3418522649-1831038044-1853292631-2271478464')
    if ($acl.GetOwner([Security.Principal.SecurityIdentifier]).Value -notin $trusted) { Fail ('untrusted cache ancestor owner: '+$ancestor.FullName) }
    $replace=[Security.AccessControl.FileSystemRights]::Delete -bor [Security.AccessControl.FileSystemRights]::DeleteSubdirectoriesAndFiles -bor [Security.AccessControl.FileSystemRights]::ChangePermissions -bor [Security.AccessControl.FileSystemRights]::TakeOwnership
    foreach ($rule in $acl.Access) {
      $applies = !($rule.PropagationFlags -band [Security.AccessControl.PropagationFlags]::InheritOnly)
      $sid = $rule.IdentityReference.Translate([Security.Principal.SecurityIdentifier]).Value
      if ($applies -and $rule.AccessControlType -eq 'Allow' -and $sid -notin $trusted -and ($rule.FileSystemRights -band $replace)) { Fail ('unsafe shared cache ancestor ACL: '+$ancestor.FullName+' '+$sid) }
    }
    $ancestor=$ancestor.Parent
  }
}
function ProtectExtracted([string] $path) {
  $acl=Get-Acl -LiteralPath $path; $acl.SetOwner($owner)
  Set-Acl -LiteralPath $path -AclObject $acl
  Private $path
}
function ValidUrl([string] $raw, [string] $kind) {
  if (!$raw -or $raw -match '[^!-~]|["<>\\^`{|}#]' -or $raw -match '%(?![0-9a-fA-F]{2})') { Fail 'invalid URL characters or escapes' }
  $uri = $null
  if (![Uri]::TryCreate($raw,[UriKind]::Absolute,[ref]$uri)) { Fail 'invalid URL' }
  if ($kind -eq 'archive' -and $raw -cmatch '^file:/') {
    $path = $raw.Substring(5)
    if ($path -match '^//[^/]|^////|\?|[\[\]]' -or $raw -match '(?i)%(00|0[1-9a-f]|1[0-9a-f]|7f)' -or $uri.Host) { Fail 'invalid file URL' }
  } else {
    if ($raw -cnotmatch '^https?://([^/?]+)' -or $uri.UserInfo -or !$uri.Host -or $uri.Port -lt 1 -or $uri.Port -gt 65535) { Fail 'invalid remote URL' }
    $authority = $Matches[1]
    if ($authority.EndsWith(':') -or $authority -match '%|@' -or $raw.Substring($raw.IndexOf($authority)+$authority.Length) -match '[\[\]]') { Fail 'invalid URL authority' }
    if ($uri.Scheme -ne 'https' -and !($kind -eq 'repository' -and $uri.Scheme -eq 'http' -and $uri.Host -in @('127.0.0.1','localhost','[::1]','::1'))) { Fail 'invalid URL scheme' }
    if ($kind -eq 'repository' -and ($raw -match '[?%]' -or $raw -match '(^|/)\.\.(/|$)')) { Fail 'invalid repository URL' }
  }
}
function Canonical([string] $name) { (($name -split '/') | Where-Object { $_ -and $_ -ne '.' }) -join '/' }
function ResolveGraph([string] $path, $graph, [int] $hops, [bool] $hard) {
  if ($hops -gt $graph.Count+1 -or $path -match '^/|[\\:\s]') { Fail 'unsafe jdk archive link graph' }
  $parts = @($path -split '/')
  $stack = New-Object 'Collections.Generic.List[string]'
  for ($i=0; $i -lt $parts.Count; $i++) {
    $part=$parts[$i]
    if (!$part -or $part -eq '.') { continue }
    if ($part -eq '..') { if (!$stack.Count) { Fail 'unsafe jdk archive link graph' }; $stack.RemoveAt($stack.Count-1); continue }
    $base=$stack -join '/'; $name=($base+'/'+$part).TrimStart('/')
    if ($graph.ContainsKey($name) -and $graph[$name].Kind -in @('l','h')) {
      $entry=$graph[$name]
      if ($hard -and $entry.Kind -eq 'l') { Fail 'unsafe hardlink target' }
      $tail=@(); if ($i+1 -lt $parts.Count) { $tail=@($parts[($i+1)..($parts.Count-1)]) }
      $prefix=if ($entry.Kind -eq 'h' -or $entry.Rooted) { '' } else { $base+'/' }
      return (ResolveGraph ($prefix+$entry.Target+'/'+($tail -join '/')) $graph ($hops+1) $hard)
    }
    $stack.Add($part)
  }
  $stack -join '/'
}
function CheckGraph($graph) {
  foreach ($name in $graph.Keys) {
    foreach ($link in $graph.Keys) {
      if ($graph[$link].Kind -in @('l','h') -and $name.StartsWith($link+'/',[StringComparison]::OrdinalIgnoreCase)) { Fail 'jdk member traverses an archive link' }
    }
    $entry=$graph[$name]
    if ($entry.Kind -in @('l','h')) {
      $base=if ($entry.Kind -eq 'h' -or $entry.Rooted) { '' } else { $name -replace '[^/]*$','' }
      $resolved=ResolveGraph ($base+$entry.Target) $graph 0 ($entry.Kind -eq 'h')
      if (($resolved -and !$graph.ContainsKey($resolved)) -or ($entry.Kind -eq 'h' -and $graph[$resolved].Kind -ne '-')) { Fail 'invalid archive link target' }
    }
  }
}
function AddGraph($graph,[string] $name,[string] $kind,[string] $target,[bool] $rooted=$false) {
  if ($name -match '^/|[\\:\s]|(^|/)\.\.(/|$)') { Fail 'unsafe jdk archive name' }
  $name=Canonical $name
  if ((!$name -and $kind -ne 'd') -or $graph.ContainsKey($name)) { Fail 'duplicate or aliased runtime entry' }
  $graph.Add($name,[pscustomobject]@{ Kind=$kind; Target=$target; Rooted=$rooted })
}
function TreeHash([string] $home) {
  if (![IO.Directory]::Exists($home) -or ([IO.File]::GetAttributes($home) -band [IO.FileAttributes]::ReparsePoint)) { Fail 'invalid runtime root' }
  $graph=@{}; $paths=@{}; $targets=@{}
  $pending=New-Object 'Collections.Generic.Stack[string]'; $pending.Push($home)
  while ($pending.Count) {
    $path=$pending.Pop(); $name=$path.Substring($home.Length).TrimStart('\').Replace('\','/')
    $attr=[IO.File]::GetAttributes($path); $target=''; $rooted=$false
    if ($attr -band [IO.FileAttributes]::ReparsePoint) {
      $item=Get-Item -LiteralPath $path -Force
      if ($item.LinkType -ne 'SymbolicLink' -or @($item.Target).Count -ne 1) { Fail 'unsupported runtime reparse point' }
      $values=@($item.Target); $target=[string]$values[0]; $targets[$name]=$target
      if ([IO.Path]::IsPathRooted($target)) {
        if (!$target.StartsWith($home+'\',[StringComparison]::OrdinalIgnoreCase)) { Fail 'escaping runtime link' }
        $target=$target.Substring($home.Length+1); $rooted=$true
      }
      AddGraph $graph $name l $target.Replace('\','/') $rooted
    } elseif ($attr -band [IO.FileAttributes]::Directory) {
      AddGraph $graph $name d ''
      foreach ($child in [IO.Directory]::GetFileSystemEntries($path)) { $pending.Push($child) }
    } else { AddGraph $graph $name '-' '' }
    $paths[$name]=$path
  }
  CheckGraph $graph
  $rows=New-Object 'Collections.Generic.List[string]'
  foreach ($name in $graph.Keys) {
    $path=$paths[$name]; $acl=Get-Acl -LiteralPath $path
    if ($acl.GetOwner([Security.Principal.SecurityIdentifier]) -ne $owner) { Fail 'runtime ownership changed' }
    $security=$acl.GetSecurityDescriptorSddlForm([Security.AccessControl.AccessControlSections]'Access,Owner')
    $content=if ($graph[$name].Kind -eq '-') { Hash $path } elseif ($graph[$name].Kind -eq 'l') { $targets[$name] } else { '' }
    $rows.Add(($name.ToLowerInvariant()+'|'+$graph[$name].Kind+'|'+[int][IO.File]::GetAttributes($path)+'|'+$security+'|'+$content))
  }
  $rows.Sort([StringComparer]::Ordinal)
  $digest=[Security.Cryptography.SHA256]::Create()
  try { ([BitConverter]::ToString($digest.ComputeHash([Text.Encoding]::UTF8.GetBytes(($rows -join "`n"))))).Replace('-','').ToLowerInvariant() } finally { $digest.Dispose() }
}

function Fetch([string] $raw, [string] $path, [string] $kind = 'archive') {
  if ($raw -cmatch '^file:/') { [IO.File]::Copy(([Uri]$raw).LocalPath, $path) }
  else {
    ValidUrl $raw $kind
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
    $uri = [Uri]$raw
    for ($redirect = 0; $redirect -le 10; $redirect++) {
      $request = [Net.HttpWebRequest]::Create($uri); $request.AllowAutoRedirect = $false
      $response = $request.GetResponse()
      try {
        $code = [int]$response.StatusCode
        if ($code -ge 300 -and $code -lt 400) {
          if ($redirect -eq 10 -or !$response.Headers['Location']) { Fail 'invalid download redirect' }
          $uri = New-Object Uri($uri, $response.Headers['Location'])
          if ($uri.Scheme -ne 'https') { Fail 'insecure download redirect' }
          ValidUrl $uri.AbsoluteUri archive
          continue
        }
        if ($code -ne 200) { Fail 'download status was not 200' }
        $input = $response.GetResponseStream(); $output = [IO.File]::Create($path)
        try { $input.CopyTo($output) } finally { $output.Dispose(); $input.Dispose() }
        break
      } finally { $response.Dispose() }
    }
  }
}
function HostKey {
  Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
namespace Histos.Bootstrap {
  public static class NativeHost {
    [StructLayout(LayoutKind.Sequential)] struct Info {
      public ushort Architecture, Reserved; public uint PageSize; public IntPtr Minimum, Maximum;
      public UIntPtr Active; public uint Count, Type, Granularity; public ushort Level, Revision;
    }
    [DllImport("kernel32.dll")] static extern void GetNativeSystemInfo(out Info info);
    public static ushort Architecture() { Info info; GetNativeSystemInfo(out info); return info.Architecture; }
  }
}
'@
  switch ([Histos.Bootstrap.NativeHost]::Architecture()) {
    9 { 'windows-x86_64' }
    12 { 'windows-aarch64' }
    default { 'windows-unsupported' }
  }
}
function Lock([string] $path, [string] $kind) {
  $fields=@{}; $archives=@{}
  $allowed=if ($kind -eq 'launcher') { @('repository','version','revision','sha256') } else { @('major','version') }
  foreach ($line in [IO.File]::ReadAllLines($path)) {
    $line=$line.Trim([char[]]@(' ',"`t","`r"))
    if (!$line -or $line.StartsWith('#')) { continue }
    if ($line -cnotmatch '^([a-z0-9]+)[ \t]+([^ \t].*)$') { Fail ('invalid '+$kind+' lock line') }
    $key=$Matches[1]; $value=$Matches[2]
    if ($kind -eq 'jdk' -and $key -ceq 'archive') {
      $tokens=@($line -split '[ \t]+')
      if ($tokens.Count -ne 4 -or $tokens[1] -cnotmatch '^(macos|linux|windows)-(x86_64|aarch64)$' -or $archives.ContainsKey($tokens[1]) -or $tokens[3] -cnotmatch '^[0-9a-fA-F]{64}$') { Fail 'invalid or duplicate runtime archive row' }
      ValidUrl $tokens[2] archive
      $archives.Add($tokens[1],[pscustomobject]@{ Url=$tokens[2]; Sha=$tokens[3].ToLowerInvariant() })
    } else {
      if ($key -cnotin $allowed -or $fields.ContainsKey($key)) { Fail ('invalid '+$kind+' lock field') }
      $fields.Add($key,$value)
    }
  }
  foreach ($key in $allowed) { if (!$fields.ContainsKey($key)) { Fail ('missing '+$kind+' lock field') } }
  if ($kind -eq 'launcher') {
    if ($fields['sha256'] -cnotmatch '^[0-9a-fA-F]{64}$') { Fail 'invalid lock checksum' }
    $fields['sha256']=$fields['sha256'].ToLowerInvariant()
    if ($fields['version'] -cnotmatch '^[0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9]+([.-][A-Za-z0-9]+)*)?-SNAPSHOT$') { Fail 'invalid launcher version' }
    $pattern='^'+[Regex]::Escape(($fields['version'] -creplace '-SNAPSHOT$',''))+'-([0-9]{8}\.[0-9]{6})-([1-9][0-9]*)$'
    if ($fields['revision'] -cnotmatch $pattern) { Fail 'invalid launcher revision' }
    $build=0; $stamp=[DateTime]::MinValue
    if (![int]::TryParse($Matches[2],[ref]$build) -or ![DateTime]::TryParseExact($Matches[1],'yyyyMMdd.HHmmss',[Globalization.CultureInfo]::InvariantCulture,[Globalization.DateTimeStyles]::None,[ref]$stamp)) { Fail 'invalid launcher timestamp or build' }
    ValidUrl $fields['repository'] repository
  } else {
    $major=0
    if ($fields['major'] -cnotmatch '^[+]?[0-9]+$' -or ![int]::TryParse($fields['major'],[ref]$major) -or $major -le 0 -or $fields['version'] -cnotmatch '^[0-9][A-Za-z0-9.+_-]*$' -or !$archives.Count) { Fail 'invalid jdk major, version or empty archive table' }
    $v=$fields['version'] -creplace '^1\.',''; $v=$v -creplace '[^0-9].*$',''
    $found=0
    if (![int]::TryParse($v,[ref]$found) -or $found -ne $major) { Fail 'jdk major disagrees' }
    $fields['archives']=$archives
  }
  $fields
}
function SafeName([string] $name) {
  if (!$name -or $name -match '(^[/\\]|[\\:\s]|(^|/)\.\.(/|$))') { Fail 'unsafe jdk archive path' }
}
function Unpack([string] $archive, [string] $destination) {
  [IO.Directory]::CreateDirectory($destination) | Out-Null
  $stream = [IO.File]::OpenRead($archive)
  try { $signature = New-Object byte[] 4; $read = $stream.Read($signature,0,4) } finally { $stream.Dispose() }
  if ($read -eq 4 -and $signature[0] -eq 80 -and $signature[1] -eq 75) {
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $zip = [IO.Compression.ZipFile]::OpenRead($archive)
    try {
      $graph=@{}
      foreach ($entry in $zip.Entries) {
        AddGraph $graph $entry.FullName $(if ($entry.FullName.EndsWith('/')) { 'd' } else { '-' }) ''
        SafeName $entry.FullName
        if ((($entry.ExternalAttributes -shr 16) -band 0xF000) -notin @(0,0x8000,0x4000)) { Fail 'unsafe jdk zip symlink' }
        $target = [IO.Path]::GetFullPath((Join-Path $destination $entry.FullName))
        if (!$target.StartsWith($destination + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) { Fail 'unsafe jdk archive destination' }
      }
      CheckGraph $graph
      [IO.Compression.ZipFileExtensions]::ExtractToDirectory($zip, $destination)
    } finally { $zip.Dispose() }
  } else {
    $names = & tar -tf $archive
    if ($LASTEXITCODE -ne 0) { Fail 'invalid jdk archive' }
    foreach ($name in $names) { SafeName $name }
    $entries = & tar -tvf $archive
    if ($LASTEXITCODE -ne 0) { Fail 'invalid jdk archive' }
    $graph=@{}
    foreach ($entry in $entries) {
      if ($entry -notmatch '^[-dlh]') { Fail 'unsafe jdk archive entry' }
      $kind=$entry.Substring(0,1); $name=($entry -split ' ')[-1]; $target=''
      if ($kind -in @('l','h')) {
        if ($entry -notmatch ' ([^ ]+) (->|link to) ([^ ]+)$') { Fail 'invalid jdk archive link' }
        $name=$Matches[1]; $target=$Matches[3]
      }
      AddGraph $graph $name $kind $target
    }
    CheckGraph $graph
    & tar -xf $archive -C $destination
    if ($LASTEXITCODE -ne 0) { Fail 'jdk extraction failed' }
  }
}
function QuoteArgument([string] $value) {
  '"' + [Regex]::Replace([Regex]::Replace($value, '(\\*)"', '$1$1\"'), '(\\+)$', '$1$1') + '"'
}
try {
  PrivateDirectory $cache
  PrivateDirectory $work
  PrivateDirectory $env:HISTOS_BOOT_TEMP
  [IO.File]::WriteAllText((Join-Path $env:HISTOS_BOOT_TEMP 'work-path'),$work)
  $jar = if ($env:HISTOS_ASSEMBLY) { $env:HISTOS_ASSEMBLY } elseif (Test-Path -LiteralPath (Join-Path $root 'dist\histos.jar')) { Join-Path $root 'dist\histos.jar' } else { $null }
  if ($jar) {
    $expected = [IO.File]::ReadAllText((Join-Path $root '.histos\assembly.sha256')).Trim()
    if ($expected -notmatch '^[0-9a-fA-F]{64}$') { Fail 'invalid assembly checksum' }
    if ((Hash $jar) -ne $expected.ToLowerInvariant()) { Fail 'checksum mismatch' }
  } else {
    Regular (Join-Path $root '.histos\launcher.lock')
    $pin = Lock (Join-Path $root '.histos\launcher.lock') launcher
    if ($pin['version'] -cne $version) { Fail 'launcher lock version does not match script' }
    $expected = $pin['sha256']
    $localJar = Join-Path $root ('.histos\launcher-cache\' + $expected + '\histos.jar')
    $dest = Join-Path $cache ('launchers\' + $pin['revision'] + '\' + $expected)
    PrivateDirectory $dest
    $jar = Join-Path $dest 'histos.jar'
    if ((Test-Path -LiteralPath $localJar) -and (Hash $localJar) -eq $expected) { $jar = $localJar }
    elseif (!(Test-Path -LiteralPath $jar) -or (Hash $jar) -ne $expected) {
      $part = Join-Path $work 'launcher.jar'
      Fetch ($pin['repository'].TrimEnd('/') + '/rocks/earlyeffect/histos/' + $pin['version'] + '/histos-' + $pin['revision'] + '.jar') $part repository
      if ((Hash $part) -ne $expected) { Fail 'launcher checksum mismatch' }
      if (Test-Path -LiteralPath $jar) { [IO.File]::Replace($part,$jar,$null) } else {
        try { [IO.File]::Move($part,$jar) } catch [IO.IOException] {
          if (!(Test-Path -LiteralPath $jar) -or (Hash $jar) -ne $expected) { Write-Error $_ }
        }
      }
    }
    if ((Hash $jar) -ne $expected) { Fail 'launcher checksum mismatch' }
  }
  $local = Join-Path $root '.histos\toolchain.local'
  $jdkLock = Join-Path $root '.histos\jdk.lock'
  $jdkPresent = Entry $jdkLock
  if ($jdkPresent) {
    Regular $jdkLock
    $lock = Lock $jdkLock jdk
  }
  if (Entry $local) {
    Regular $local
    $homes = @([IO.File]::ReadAllLines($local) | ForEach-Object { $_.Trim() } | Where-Object { $_ })
    if ($homes.Count -ne 1 -or ![IO.Path]::IsPathRooted($homes[0])) { Fail 'invalid toolchain.local' }
    $java = Join-Path $homes[0] 'bin\java.exe'
    if (!(Test-Path -LiteralPath $java -PathType Leaf)) { Fail 'toolchain.local has no bin/java.exe' }
  } elseif ($jdkPresent) {
    $hostKey=HostKey
    if (!$lock['archives'].ContainsKey($hostKey)) { Fail ('no archive for runtime host '+$hostKey) }
    $lock['url']=$lock['archives'][$hostKey].Url; $lock['sha256']=$lock['archives'][$hostKey].Sha
    $sha = $lock['sha256']; $dest = Join-Path $cache ('jdks\' + $sha)
    PrivateDirectory $dest
    $archive = Join-Path $dest 'archive'
    if (!(Test-Path -LiteralPath $archive) -or (Hash $archive) -ne $sha) {
      $part = Join-Path $work 'jdk.archive'; Fetch $lock['url'] $part
      if ((Hash $part) -ne $sha) { Fail 'jdk checksum mismatch' }
      if (Test-Path -LiteralPath $archive) { [IO.File]::Replace($part,$archive,$null) } else {
        try { [IO.File]::Move($part,$archive) } catch [IO.IOException] {
          if (!(Test-Path -LiteralPath $archive) -or (Hash $archive) -ne $sha) { Write-Error $_ }
        }
      }
    }
    if ((Hash $archive) -ne $sha) { Fail 'jdk checksum mismatch' }
    $java = $null
    foreach ($item in Get-ChildItem -LiteralPath $dest -Directory -Filter 'install.*') {
      Private $item.FullName
      $home=Join-Path $item.FullName 'home'
      $pathFile=Join-Path $item.FullName 'java.path'; $expectedFile=Join-Path $item.FullName 'runtime.sha256'
      if (!(Entry $pathFile) -or !(Entry $expectedFile)) { continue }
      Regular $pathFile; Regular $expectedFile
      $relative=[IO.File]::ReadAllText($pathFile).Trim()
      if ($relative -match '\.\.|^[/\\]|:') { continue }
      try {
        $actual=TreeHash $home
        if ($actual -ceq [IO.File]::ReadAllText($expectedFile).Trim()) { $java=Join-Path $home $relative; break }
      } catch { [Console]::Error.WriteLine('cached runtime failed integrity verification') }
    }
    if (!$java) {
      $unpack = Join-Path $work 'unpack'; Unpack $archive $unpack
      $found = @(Get-ChildItem -LiteralPath $unpack -Recurse -File -Filter 'java.exe' | Where-Object { $_.Directory.Name -eq 'bin' })
      if ($found.Count -ne 1) { Fail 'locked jdk must have one bin/java.exe' }
      $relative = $found[0].FullName.Substring($unpack.Length+1)
      foreach ($item in Get-ChildItem -LiteralPath $unpack -Recurse -Force) {
        if (!($item.Attributes -band [IO.FileAttributes]::ReparsePoint)) {
          if ($item.Attributes -band [IO.FileAttributes]::ReadOnly) { $item.Attributes=$item.Attributes -band (-bnot [IO.FileAttributes]::ReadOnly) }
          ProtectExtracted $item.FullName
        }
      }
      ProtectExtracted $unpack
      $expected=TreeHash $unpack
      $install=Join-Path $dest ('install.'+[Guid]::NewGuid().ToString('N'))
      PrivateDirectory $install
      [IO.File]::WriteAllText((Join-Path $install 'java.path'),$relative)
      [IO.File]::WriteAllText((Join-Path $install 'runtime.sha256'),$expected)
      [IO.Directory]::Move($unpack,(Join-Path $install 'home'))
      $java = Join-Path $install ('home\'+$relative); $install = $null
    }
  } else {
    $java = (Get-Command java.exe -ErrorAction SilentlyContinue).Source
    if (!$java) { Fail 'java is not on PATH and .histos/jdk.lock is missing' }
  }
  $all = @('--enable-native-access=ALL-UNNAMED','-jar',$jar) + $launchArgs
Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
using System.Text;
namespace Histos.Bootstrap {
  public sealed class JavaJob : IDisposable {
    [StructLayout(LayoutKind.Sequential)] struct BasicLimits {
      public long ProcessTime, JobTime; public uint Flags; public UIntPtr Minimum, Maximum;
      public uint Processes; public UIntPtr Affinity; public uint Priority, Scheduling;
    }
    [StructLayout(LayoutKind.Sequential)] struct Counters { public ulong ReadOps, WriteOps, OtherOps, ReadBytes, WriteBytes, OtherBytes; }
    [StructLayout(LayoutKind.Sequential)] struct Limits { public BasicLimits Basic; public Counters IO; public UIntPtr ProcessMemory, JobMemory, PeakProcess, PeakJob; }
    [StructLayout(LayoutKind.Sequential, CharSet=CharSet.Unicode)] struct Startup {
      public uint Size; public string Reserved, Desktop, Title; public uint X,Y,XSize,YSize,XCount,YCount,Fill,Flags;
      public ushort Show, ReservedSize; public IntPtr ReservedBytes, Input, Output, Error;
    }
    [StructLayout(LayoutKind.Sequential)] struct ProcessInfo { public IntPtr Process, Thread; public uint Id, ThreadId; }
    [DllImport("kernel32.dll", CharSet=CharSet.Unicode, SetLastError=true)] static extern IntPtr CreateJobObject(IntPtr attributes, string name);
    [DllImport("kernel32.dll", SetLastError=true)] static extern bool SetInformationJobObject(IntPtr job, int kind, ref Limits data, uint size);
    [DllImport("kernel32.dll", SetLastError=true)] static extern bool AssignProcessToJobObject(IntPtr job, IntPtr process);
    [DllImport("kernel32.dll", CharSet=CharSet.Unicode, SetLastError=true)] static extern bool CreateProcess(string app, StringBuilder command, IntPtr processAttributes, IntPtr threadAttributes, bool inherit, uint flags, IntPtr environment, string directory, ref Startup startup, out ProcessInfo process);
    [DllImport("kernel32.dll", SetLastError=true)] static extern uint ResumeThread(IntPtr thread);
    [DllImport("kernel32.dll")] static extern bool TerminateProcess(IntPtr process, uint exit);
    [DllImport("kernel32.dll")] static extern bool CloseHandle(IntPtr handle);
    private IntPtr job, thread, process;
    public int ProcessId { get; private set; }
    public int Error { get; private set; }
    public bool Start(string app, string arguments) {
      job=CreateJobObject(IntPtr.Zero, null);
      var limits=new Limits(); limits.Basic.Flags=0x2000;
      bool ready=job!=IntPtr.Zero && SetInformationJobObject(job,9,ref limits,(uint)Marshal.SizeOf(typeof(Limits)));
      var startup=new Startup(); startup.Size=(uint)Marshal.SizeOf(typeof(Startup));
      ProcessInfo child;
      if(ready && CreateProcess(app,new StringBuilder("\""+app+"\" "+arguments),IntPtr.Zero,IntPtr.Zero,true,4,IntPtr.Zero,null,ref startup,out child)) {
        process=child.Process; thread=child.Thread; ProcessId=(int)child.Id;
        ready=AssignProcessToJobObject(job,process);
      } else ready=false;
      if(!ready) { Error=Marshal.GetLastWin32Error(); if(process!=IntPtr.Zero) TerminateProcess(process,1); }
      return ready;
    }
    public bool Resume() {
      bool ready=ResumeThread(thread)!=UInt32.MaxValue;
      if(!ready) Error=Marshal.GetLastWin32Error();
      return ready;
    }
    public void Dispose() {
      if(job!=IntPtr.Zero) { CloseHandle(job); job=IntPtr.Zero; }
      if(thread!=IntPtr.Zero) { CloseHandle(thread); thread=IntPtr.Zero; }
      if(process!=IntPtr.Zero) { CloseHandle(process); process=IntPtr.Zero; }
    }
  }
}
'@
$job = New-Object Histos.Bootstrap.JavaJob
try {
  if (!$job.Start($java, (($all | ForEach-Object { QuoteArgument $_ }) -join ' '))) { Fail ('Java job creation failed: ' + $job.Error) }
  $child = [Diagnostics.Process]::GetProcessById($job.ProcessId)
  try {
    $handle = $child.Handle
    if (!$job.Resume()) { Fail ('Java resume failed: ' + $job.Error) }
    while (!$child.WaitForExit(100)) {}
    $exitCode = $child.ExitCode
  } finally { $child.Dispose() }
} finally { $job.Dispose() }

} catch { [Console]::Error.WriteLine($_.Exception.Message) } finally {
  if (Test-Path -LiteralPath $work) { Remove-Item -LiteralPath $work -Recurse -Force }
  if ($install -and (Test-Path -LiteralPath $install)) { Remove-Item -LiteralPath $install -Recurse -Force }
  if (Test-Path -LiteralPath $env:HISTOS_BOOT_TEMP) { Remove-Item -LiteralPath $env:HISTOS_BOOT_TEMP -Recurse -Force }
}
exit $exitCode
"""

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
