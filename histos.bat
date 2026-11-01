@echo off
setlocal EnableExtensions
rem Generated from the manual ship. Finds java.exe and, without a local assembly, asks cs fetch.
set "HISTOS_BOOT_ROOT=%~dp0"
set "HISTOS_BOOT_VERSION=0.0.2-SNAPSHOT"
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
