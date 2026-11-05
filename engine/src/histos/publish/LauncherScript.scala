package histos.publish

import histos.version.Catalog
import histos.workspace.{IoFailure, IoFault}
import histos.{Module, ProjectGraph, Publication}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** `histos`, rendered from the ship number the way a workflow file is. A line states the next release. A local
  * assembly is checked against `.histos/assembly.sha256`. Otherwise the script execs the user shim. JVM arguments come
  * from a repository `.jvmopts`, one per line. Histos does not install a JDK.
  */
object LauncherScript:
  def check(repo: Path, graph: ProjectGraph): IO[ScriptError, Unit] =
    ZIO
      .fromEither(ship(graph))
      .flatMap:
        case None          => ZIO.unit
        case Some(version) =>
          LauncherPin.read(repo).mapError(ScriptError.Pin(_)) *>
            agree(repo.resolve("histos"), posix(version))

  def update(repo: Path, graph: ProjectGraph): IO[ScriptError, Unit] =
    ZIO
      .fromEither(ship(graph))
      .flatMap:
        case None          => ZIO.unit
        case Some(version) =>
          LauncherPin.read(repo).mapError(ScriptError.Pin(_)) *>
            write(
              repo.resolve("histos"),
              posix(version),
              executable = true,
            )

  def posix(version: String): String =
    Posix.replace("__VERSION__", version)

  private def ship(graph: ProjectGraph): Either[ScriptError, Option[String]] =
    if !launcher(graph) then Right(None)
    else
      versions(graph.catalog) match
        case List(version) => Right(Some(version))
        case _             => Left(ScriptError.Revision)

  /** The next release number. One distinct value. Session coordinates stay out of the script. */
  private def versions(catalog: Catalog): List[String] =
    catalog match
      case Catalog.Empty       => Nil
      case Catalog.Lines(rows) => rows.toList.map(_.number.render).distinct

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
# Generated from the ship number. `histos workflow --update` rewrites this file.
# A local assembly is checked against .histos/assembly.sha256. Otherwise the user shim runs.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
version=__VERSION__
fail() { echo "histos $version: $*" >&2; exit 1; }
hash_of() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | awk '{print $1}'
  else shasum -a 256 "$1" | awk '{print $1}'; fi
}
valid_sha() { printf '%s\n' "$1" | awk 'length($0)==64 && $0 !~ /[^0-9a-fA-F]/ {ok=1} END {exit !ok}'; }
os=$(uname -s 2>/dev/null || true)
case $os in
  MINGW*|MSYS*|CYGWIN*|Windows*)
    fail "Histos runs on macOS and Linux. On Windows, use WSL. A native Windows build may come later."
    ;;
esac
java_guide='A JDK 21 or newer is required. Histos does not install it.
macOS: brew install --cask temurin@21
Linux: https://adoptium.net/temurin/releases?version=21'
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
  home=${HISTOS_HOME:-$HOME/.histos}
  shim=$home/bin/histos
  [ -x "$shim" ] || fail "histos is not installed. Run the install script, then export PATH=\"\$HOME/.histos/bin:\$PATH\""
  exec "$shim" "$@"
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
client=$root/dist/histos-client
jar_client=1
if [ "$#" -ge 1 ]; then
  case $1 in
    server|bsp|mcp) jar_client=1 ;;
    *)
      if [ -f "$client" ] && [ -x "$client" ]; then jar_client=0; fi
      ;;
  esac
fi
if [ "$jar_client" -eq 0 ]; then
  pin_file=$root/build.properties
  [ -f "$pin_file" ] || fail "missing build.properties"
  pin_line=$(awk -F= '$1 == "histos.version" { print $2; exit }' "$pin_file")
  [ -n "$pin_line" ] || fail "missing histos.version"
  HISTOS_JAVA=$java_bin
  HISTOS_JAR=$jar
  HISTOS_ROOT=$root
  HISTOS_VERSION="histos $pin_line"
  export HISTOS_JAVA HISTOS_JAR HISTOS_ROOT HISTOS_VERSION
  exec "$client" "$@"
fi
eval "set -- ${jvm_extra} --enable-native-access=ALL-UNNAMED -jar $(shell_quote "$jar")${quoted_args}"
exec "$java_bin" "$@"
"""
end LauncherScript
