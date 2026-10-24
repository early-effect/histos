@echo off
setlocal EnableExtensions
rem Generated from the manual ship. Finds java.exe and runs the pinned launcher jar.
set "HISTOS_BOOT_ROOT=%~dp0"
set "HISTOS_BOOT_VERSION=0.0.1-SNAPSHOT"
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
& $java --enable-native-access=ALL-UNNAMED -jar $jar @launchArgs
exit $LASTEXITCODE
