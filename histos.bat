@echo off
setlocal EnableExtensions
rem Generated from the manual ship. histos workflow --update rewrites this file.
rem Not executed by the POSIX test.
set "ROOT=%~dp0"
set "VERSION=0.0.1-SNAPSHOT"
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
