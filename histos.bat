@echo off
setlocal EnableExtensions
rem Frozen Windows entry point. Not executed by the POSIX test.
set "ROOT=%~dp0"
set /p VERSION=<"%ROOT%.histos-version"
if defined HISTOS_ASSEMBLY (
  set "JAR=%HISTOS_ASSEMBLY%"
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
where java >nul 2>&1
if errorlevel 1 (
  echo histos %VERSION%: java is not on PATH 1>&2
  exit /b 1
)
java -jar "%JAR%" %*
exit /b %ERRORLEVEL%
