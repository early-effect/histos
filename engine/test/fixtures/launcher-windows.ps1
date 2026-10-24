# Hand-written Windows bootstrap fixture, independent of the generated build workflow.
#requires -Version 5.1
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Check([bool] $condition, [string] $message) {
  if (!$condition) { Write-Error $message }
}
function Hash([string] $path) { (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant() }
function Quote([string] $value) {
  '"' + [Regex]::Replace([Regex]::Replace($value, '(\\*)"', '$1$1\"'), '(\\+)$', '$1$1') + '"'
}
function Stop-Tree([Diagnostics.Process] $process) {
  if (!$process.HasExited) {
    foreach ($child in Get-CimInstance Win32_Process -Filter ("ParentProcessId=" + $process.Id)) {
      $found = Get-Process -Id $child.ProcessId -ErrorAction SilentlyContinue
      if ($found) { try { Stop-Tree $found } finally { $found.Dispose() } }
    }
    try { $process.Kill() } catch [InvalidOperationException] { if (!$process.HasExited) { Write-Error $_ } }
    $process.WaitForExit()
  }
}

Check ($PSVersionTable.PSEdition -eq 'Desktop' -and $env:OS -eq 'Windows_NT') 'Run this fixture with stock Windows PowerShell, not pwsh.'
$source = [IO.File]::ReadAllText((Join-Path $PSScriptRoot '..\..\..\histos.bat'))
$ship = [Regex]::Match($source, '(?m)^set "HISTOS_BOOT_VERSION=([^"]+)"\r?$')
Check $ship.Success 'The committed launcher has no ship version.'
$version = $ship.Groups[1].Value
$revision = ($version -creplace '-SNAPSHOT$', '') + '-20261023.224241-14'
$scope = Join-Path ([IO.Path]::GetTempPath()) ('histos win ' + [Guid]::NewGuid().ToString('N'))
$children = New-Object 'System.Collections.Generic.List[System.Diagnostics.Process]'

try {
  [IO.Directory]::CreateDirectory($scope) | Out-Null
  $program = Join-Path $scope 'fixture.exe'
  # The same stock .NET executable records Java argv or serves only the loopback fixture.
  Add-Type -OutputAssembly $program -OutputType ConsoleApplication -TypeDefinition @'
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
namespace Histos.WindowsFixture {
  public static class Program {
    public static void Main(string[] args) {
      if (args.Length == 1 && (args[0] == "descendant" || args[0] == "grandchild")) Lifetime(args[0]);
      else if (args.Length == 5 && args[0] == "serve") {
        var listener = new TcpListener(IPAddress.Loopback, 0);
        listener.Start();
        Console.WriteLine(((IPEndPoint)listener.LocalEndpoint).Port);
        Console.Out.Flush();
        try {
          while (true) {
            using (var client = listener.AcceptTcpClient())
            using (var stream = client.GetStream()) {
              stream.ReadTimeout = 10000;
              using (var reader = new StreamReader(stream, Encoding.ASCII, false, 1024, true)) {
                string line = reader.ReadLine();
                string[] request = String.IsNullOrEmpty(line) ? new string[0] : line.Split(' ');
                string target = request.Length > 1 ? request[1] : "";
                while (!String.IsNullOrEmpty(reader.ReadLine())) {}
                File.AppendAllText(args[2], target + "\n");
                byte[] body = new byte[0];
                string status = "404 Not Found";
                if (File.Exists(args[4])) status = "503 Service Unavailable";
                else if (target == args[3]) { status = "200 OK"; body = File.ReadAllBytes(args[1]); }
                else if (target == args[3] + ".sha256") {
                  using (var digest = SHA256.Create()) {
                    body = Encoding.ASCII.GetBytes(BitConverter.ToString(digest.ComputeHash(File.ReadAllBytes(args[1]))).Replace("-", "").ToLowerInvariant());
                  }
                  status = "200 OK";
                }
                byte[] head = Encoding.ASCII.GetBytes("HTTP/1.1 " + status + "\r\nContent-Length: " + body.Length + "\r\nConnection: close\r\n\r\n");
                stream.Write(head, 0, head.Length);
                stream.Write(body, 0, body.Length);
              }
            }
          }
        } finally { listener.Stop(); }
      } else {
        var lines = new List<string>();
        lines.Add(Assembly.GetExecutingAssembly().Location);
        foreach (string arg in args) lines.Add(Convert.ToBase64String(Encoding.UTF8.GetBytes(arg)));
        File.WriteAllLines(Environment.GetEnvironmentVariable("HISTOS_FIXTURE_LOG"), lines.ToArray(), Encoding.UTF8);
        if (Environment.GetEnvironmentVariable("HISTOS_FIXTURE_LIFETIME") == "yes") Lifetime("java");
        int exit;
        if (Int32.TryParse(Environment.GetEnvironmentVariable("HISTOS_FIXTURE_EXIT"), out exit)) Environment.ExitCode = exit;
      }
    }
    private static void Lifetime(string role) {
      File.WriteAllText(Path.Combine(Environment.GetEnvironmentVariable("HISTOS_FIXTURE_PIDS"),role+".pid"),Process.GetCurrentProcess().Id.ToString());
      using (var stop=EventWaitHandle.OpenExisting(Environment.GetEnvironmentVariable("HISTOS_FIXTURE_STOP"))) {
        if (role == "grandchild") {
          using (var ready=EventWaitHandle.OpenExisting(Environment.GetEnvironmentVariable("HISTOS_FIXTURE_READY"))) ready.Set();
          stop.WaitOne();
        } else {
          var start=new ProcessStartInfo(Assembly.GetExecutingAssembly().Location,role=="java" ? "descendant" : "grandchild");
          start.UseShellExecute=false;
          using (var child=Process.Start(start)) stop.WaitOne();
        }
      }
    }
  }
}
'@
  Add-Type -AssemblyName System.IO.Compression.FileSystem
  $jar = Join-Path $scope 'remote.jar'
  [IO.File]::WriteAllText($jar, 'immutable remote fixture')
  $jarSha = Hash $jar
  $requestPath = '/repo/rocks/earlyeffect/histos/' + $version + '/histos-' + $revision + '.jar'
  $requests = Join-Path $scope 'requests.log'
  $offline = Join-Path $scope 'offline'
  [IO.File]::WriteAllText($requests, '')
  $serverStart = New-Object Diagnostics.ProcessStartInfo
  $serverStart.FileName = $program
  $serverStart.Arguments = (@('serve', $jar, $requests, $requestPath, $offline) | ForEach-Object { Quote $_ }) -join ' '
  $serverStart.UseShellExecute = $false
  $serverStart.RedirectStandardOutput = $true
  $server = [Diagnostics.Process]::Start($serverStart)
  $children.Add($server)
  $ready = $server.StandardOutput.ReadLineAsync()
  Check ($ready.Wait(15000)) 'The loopback fixture did not become ready.'
  $port = 0
  Check ([int]::TryParse($ready.Result, [ref]$port) -and $port -gt 0) 'The loopback fixture did not report a port.'
  $repository = 'http://127.0.0.1:' + $port + '/repo'
  $pin = "repository $repository`nversion $version`nrevision $revision`nsha256 $jarSha`n"
  $jdkHome = Join-Path $scope 'jdk'
  [IO.Directory]::CreateDirectory((Join-Path $jdkHome 'bin')) | Out-Null
  [IO.File]::Copy($program, (Join-Path $jdkHome 'bin\java.exe'))
  [IO.Directory]::CreateDirectory((Join-Path $jdkHome 'lib')) | Out-Null
  [IO.File]::WriteAllText((Join-Path $jdkHome 'lib\jvm.dll'),'locked library')
  [IO.File]::WriteAllText((Join-Path $jdkHome 'lib\modules'),'locked image')
  [IO.File]::WriteAllText((Join-Path $jdkHome 'release'),'locked release')
  $zip = Join-Path $scope 'jdk.zip'
  [IO.Compression.ZipFile]::CreateFromDirectory($jdkHome, $zip)
  $zipSha = Hash $zip
  $hostStart=$source.IndexOf('function HostKey {'); $hostEnd=$source.IndexOf('function Lock(',$hostStart)
  Check ($hostStart -ge 0 -and $hostEnd -gt $hostStart) 'Cannot locate the committed native host detector.'
  . ([scriptblock]::Create($source.Substring($hostStart,$hostEnd-$hostStart)))
  $hostKey=HostKey
  Check ($hostKey -cin @('windows-x86_64','windows-aarch64')) 'Unsupported native Windows fixture host.'

  function New-Fixture([string] $name) {
    $root = Join-Path $scope $name
    foreach ($folder in @($root, "$root\.histos", "$root\installed\bin", "$root\tmp")) {
      [IO.Directory]::CreateDirectory($folder) | Out-Null
    }
    [IO.File]::WriteAllText("$root\histos.bat", $source)
    [IO.File]::WriteAllText("$root\.histos\launcher.lock", $pin)
    [IO.File]::WriteAllText("$root\source.jar", 'independent source fixture')
    [IO.File]::WriteAllText("$root\.histos\assembly.sha256", (Hash "$root\source.jar"))
    [IO.File]::Copy($program, "$root\installed\bin\java.exe")
    [IO.File]::Copy($zip, "$root\jdk.zip")
    [pscustomobject]@{ Root = $root; Cache = "$root\cache\histos"; Temp = "$root\tmp"; Remote = "$root\cache\histos\launchers\$revision\$jarSha\histos.jar" }
  }
  function Lock-Jdk($fixture) {
    $url = ([Uri](Join-Path $fixture.Root 'jdk.zip')).AbsoluteUri
    $mac = ([Uri](Join-Path $fixture.Root 'unrelated-mac.zip')).AbsoluteUri
    $linux = ([Uri](Join-Path $fixture.Root 'unrelated-linux.zip')).AbsoluteUri
    [IO.File]::WriteAllText(($fixture.Root + '\.histos\jdk.lock'), "major 21`nversion 21.0.2`narchive macos-aarch64 $mac $('0' * 64)`narchive linux-x86_64 $linux $('1' * 64)`narchive $hostKey $url $zipSha`n")
  }
  function Start-Launcher($fixture, [AllowEmptyCollection()][AllowEmptyString()][string[]] $arguments = @(), [hashtable] $extra = @{}) {
    $log = Join-Path $fixture.Root ('java-' + [Guid]::NewGuid().ToString('N') + '.log')
    $start = New-Object Diagnostics.ProcessStartInfo
    $start.FileName = $env:ComSpec
    $command = (Quote ($fixture.Root + '\histos.bat')) + ' ' + (($arguments | ForEach-Object { Quote $_ }) -join ' ')
    $start.Arguments = '/d /v:off /s /c "' + $command + '"'
    $start.WorkingDirectory = $fixture.Root
    $start.UseShellExecute = $false
    $start.RedirectStandardOutput = $true
    $start.RedirectStandardError = $true
    $start.EnvironmentVariables.Remove('HISTOS_ASSEMBLY')
    $environment = @{
      HISTOS_FIXTURE_LOG = $log; XDG_CACHE_HOME = ($fixture.Root + '\cache')
      TEMP = $fixture.Temp; TMP = $fixture.Temp
      PATH = ($fixture.Root + '\installed\bin;' + $PSHOME + ';' + $env:SystemRoot + '\System32')
    }
    foreach ($key in $extra.Keys) { $environment[$key] = $extra[$key] }
    foreach ($key in $environment.Keys) { $start.EnvironmentVariables[$key] = $environment[$key] }
    $process = [Diagnostics.Process]::Start($start)
    $children.Add($process)
    [pscustomobject]@{ Process = $process; Out = $process.StandardOutput.ReadToEndAsync(); Err = $process.StandardError.ReadToEndAsync(); Log = $log; Fixture = $fixture }
  }
  function Finish-Launcher($run, [int] $expectedExit, [AllowEmptyCollection()][AllowEmptyString()][string[]] $expectedArguments = @()) {
    Check ($run.Process.WaitForExit(60000)) ('Launcher timed out: ' + $run.Fixture.Root)
    $output = $run.Out.Result + $run.Err.Result
    Check ($run.Process.ExitCode -eq $expectedExit) ("Expected exit $expectedExit, found " + $run.Process.ExitCode + ': ' + $output)
    $lines = @(if (Test-Path -LiteralPath $run.Log) { [IO.File]::ReadAllLines($run.Log) })
    $actual = @($lines | Select-Object -Skip 1 | ForEach-Object { [Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($_)) })
    Check ($actual.Count -eq $expectedArguments.Count) ('Argument count differs: ' + ($actual | ConvertTo-Json -Compress))
    for ($index = 0; $index -lt $actual.Count; $index++) {
      Check ($actual[$index] -ceq $expectedArguments[$index]) ("Argument $index differs: " + ($actual | ConvertTo-Json -Compress))
    }
    if ($expectedArguments.Count -eq 0) { Check ($lines.Count -eq 0) 'Rejected bytes reached Java.' }
    Check (@(Get-ChildItem -LiteralPath $run.Fixture.Temp -Force).Count -eq 0) 'Batch PowerShell staging was not cleaned.'
    Check (@(Get-ChildItem -LiteralPath $run.Fixture.Cache -Directory -Filter '.bootstrap.*' -ErrorAction SilentlyContinue).Count -eq 0) 'Bootstrap staging was not cleaned.'
    [pscustomobject]@{ Java = $(if ($lines.Count -gt 0) { $lines[0] } else { '' }); Output = $output }
  }
  function Source-Env($fixture) { @{ HISTOS_ASSEMBLY = ($fixture.Root + '\source.jar') } }
  function Java-Args([string] $path) { @('--enable-native-access=ALL-UNNAMED', '-jar', $path) }

  $fixture = New-Fixture 'source and arguments'
  $tail = @('graph', 'a b', '', 'literal;$value&|<>^!%', 'embedded"quote', 'trailing\')
  $before = [IO.File]::ReadAllLines($requests).Count
  [IO.File]::WriteAllText($offline, 'offline')
  $actual = Finish-Launcher (Start-Launcher $fixture $tail (Source-Env $fixture)) 0 ((Java-Args ($fixture.Root + '\source.jar')) + $tail)
  Check ($actual.Java -eq ($fixture.Root + '\installed\bin\java.exe')) 'No-lock bootstrap did not use the fixture PATH Java.'
  $exitEnv = Source-Env $fixture
  $exitEnv.HISTOS_FIXTURE_EXIT = '7'
  $actual = Finish-Launcher (Start-Launcher $fixture @() $exitEnv) 7 (Java-Args ($fixture.Root + '\source.jar'))
  Check ([IO.File]::ReadAllLines($requests).Count -eq $before) 'Source override contacted the repository.'
  [IO.File]::WriteAllText(($fixture.Root + '\source.jar'), 'changed source')
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
  Check ($actual.Output.Contains('checksum mismatch')) 'Source mismatch was not diagnosed.'
  [IO.File]::Delete($offline)
  Write-Host 'PASS source override, independent digest and exact arguments'

  $fixture = New-Fixture 'remote and cache'
  $actual = Finish-Launcher (Start-Launcher $fixture) 0 (Java-Args $fixture.Remote)
  Check ((Hash $fixture.Remote) -eq $jarSha) 'Downloaded launcher is not the committed pin.'
  $before = [IO.File]::ReadAllLines($requests).Count
  [IO.File]::WriteAllText($offline, 'offline')
  $actual = Finish-Launcher (Start-Launcher $fixture) 0 (Java-Args $fixture.Remote)
  Check ([IO.File]::ReadAllLines($requests).Count -eq $before) 'Warm startup fetched metadata, a sidecar or bytes.'
  [IO.File]::WriteAllText($fixture.Remote, 'changed cached jar')
  $actual = Finish-Launcher (Start-Launcher $fixture) 1
  [IO.File]::Delete($offline)
  $actual = Finish-Launcher (Start-Launcher $fixture) 0 (Java-Args $fixture.Remote)
  Check ((Hash $fixture.Remote) -eq $jarSha) 'Corrupt cached launcher was not repaired.'
  [IO.File]::WriteAllText($jar, 'wrong remote jar with its own matching sidecar')
  $refused = New-Fixture 'wrong remote pin'
  $actual = Finish-Launcher (Start-Launcher $refused) 1
  Check ($actual.Output.Contains('checksum mismatch') -and !(Test-Path -LiteralPath $refused.Remote)) 'Wrong remote bytes were published or executed.'
  [IO.File]::WriteAllText($jar, 'immutable remote fixture')
  Write-Host 'PASS exact download pin, warm offline launch and corrupt launcher recovery'

  $fixture = New-Fixture 'locked JDK'
  Lock-Jdk $fixture
  $sourceArguments = Java-Args ($fixture.Root + '\source.jar')
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
  $jdkCache = Join-Path $fixture.Cache ('jdks\' + $zipSha)
  Check ($actual.Java.StartsWith($jdkCache + '\install.', [StringComparison]::OrdinalIgnoreCase)) 'PATH Java overrode the locked JDK.'
  $installed = $actual.Java
  foreach ($name in @('lib\jvm.dll','lib\modules','release')) {
    $oldJava=$actual.Java
    $home=Split-Path (Split-Path $oldJava)
    [IO.File]::WriteAllText((Join-Path $home $name),'changed runtime content')
    $actual=Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
    Check ($actual.Java -ne $oldJava) ('Changed '+$name+' reached Java before repair.')
  }
  $oldJava=$actual.Java; $home=Split-Path (Split-Path $oldJava)
  $library=Join-Path $home 'lib\modules'; $acl=Get-Acl -LiteralPath $library
  $everyone=New-Object Security.Principal.SecurityIdentifier('S-1-1-0')
  $rule=New-Object Security.AccessControl.FileSystemAccessRule($everyone,'ReadAndExecute','Allow')
  $acl.AddAccessRule($rule); Set-Acl -LiteralPath $library -AclObject $acl
  $actual=Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
  Check ($actual.Java -ne $oldJava) 'Changed runtime permissions reached Java.'
  $oldJava=$actual.Java; $home=Split-Path (Split-Path $oldJava)
  New-Item -ItemType SymbolicLink -Path (Join-Path $home 'lib\current') -Target (Join-Path $home 'lib\jvm.dll') | Out-Null
  $actual=Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
  Check ($actual.Java -ne $oldJava) 'Changed runtime link inventory reached Java.'
  $installed=$actual.Java
  [IO.File]::Move(($fixture.Root + '\jdk.zip'), ($fixture.Root + '\jdk.offline'))
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
  Check ($actual.Java -eq $installed) 'Warm archive was unpacked again.'
  [IO.File]::WriteAllText($installed, 'changed installed executable')
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
  Check ($actual.Java -ne $installed -and (Hash $actual.Java) -eq (Hash $program)) 'Changed installed Java was not verified and repaired.'
  [IO.File]::WriteAllText(($jdkCache + '\archive'), 'changed cached archive')
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
  [IO.File]::Move(($fixture.Root + '\jdk.offline'), ($fixture.Root + '\jdk.zip'))
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
  Check ((Hash ($jdkCache + '\archive')) -eq $zipSha) 'Changed archive was not repaired from verified bytes.'
  [IO.File]::WriteAllText(($fixture.Root + '\jdk.zip'), 'changed source archive')
  [IO.File]::WriteAllText(($jdkCache + '\archive'), 'changed cached archive')
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
  Check ($actual.Output.Contains('jdk checksum mismatch')) 'Archive mismatch was not diagnosed.'
  [IO.File]::WriteAllText(($fixture.Root + '\.histos\toolchain.local'), ($fixture.Root + '\installed'))
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
  Check ($actual.Java -eq ($fixture.Root + '\installed\bin\java.exe')) 'Explicit local trust did not override the lock.'
  [IO.File]::WriteAllText(($fixture.Root+'\.histos\jdk.lock'),"major 21`nversion 21.0.2`narchive macos-aarch64 $(([Uri]($fixture.Root+'\jdk.zip')).AbsoluteUri) $zipSha`n")
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 0 $sourceArguments
  Check ($actual.Java -eq ($fixture.Root+'\installed\bin\java.exe')) 'Explicit trust required a matching archive row.'
  Write-Host 'PASS locked JDK precedence, ZIP warm reuse, archive/executable recovery and local trust'

  $fixture=New-Fixture 'missing host'
  Lock-Jdk $fixture
  $lockPath=$fixture.Root+'\.histos\jdk.lock'
  $records=@([IO.File]::ReadAllLines($lockPath) | Where-Object { !$_.StartsWith('archive '+$hostKey+' ') })
  [IO.File]::WriteAllLines($lockPath,$records)
  $actual=Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
  Check ($actual.Output.Contains('no archive for runtime host '+$hostKey)) 'A missing host used an unrelated archive.'
  Check (!(Test-Path -LiteralPath ($fixture.Cache+'\jdks'))) 'A missing host downloaded or installed another host archive.'
  Write-Host 'PASS portable host tables, exact native host selection and missing-host rejection'

  $fixture = New-Fixture 'schema rejection'
  foreach ($text in @(
    ($pin + "revision latest`n"), ($pin + "unknown value`n"), $pin.Replace($revision, 'latest'),
    $pin.Replace('20261023', '20260230'), $pin.Replace($jarSha, 'bad'),
    $pin.Replace('127.0.0.1', '127.0.0.2'), $pin.Replace("version $version`n", '')
  )) {
    [IO.File]::WriteAllText(($fixture.Root + '\.histos\launcher.lock'), $text)
    $actual = Finish-Launcher (Start-Launcher $fixture) 1
    Check ($actual.Output -match 'invalid|missing') 'Malformed pin was not refused by schema validation.'
  }
  [IO.File]::WriteAllText(($fixture.Root + '\.histos\launcher.lock'), $pin)
  Lock-Jdk $fixture
  $lock = [IO.File]::ReadAllText(($fixture.Root + '\.histos\jdk.lock'))
  foreach ($text in @(
    ($lock + "major 21`n"), ($lock + "unknown value`n"), $lock.Replace('major 21', 'major 0'),
    $lock.Replace('21.0.2', '22.0.2'), $lock.Replace($zipSha, 'bad'),
    $lock.Replace('file:', 'http:'), $lock.Replace("version 21.0.2`n", ''),
    ($lock + "archive $hostKey file:///C:/unrelated.zip $zipSha`n"),
    $lock.Replace('macos-aarch64','macos-arm64'), ($lock + "url file:///C:/legacy.zip`n"),
    ($lock + "sha256 $zipSha`n"), "major 21`nversion 21.0.2`n", $lock.Replace('archive linux-x86_64','archive Linux-x86_64')
  )) {
    [IO.File]::WriteAllText(($fixture.Root + '\.histos\jdk.lock'), $text)
    $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
    Check ($actual.Output -match 'invalid|missing|disagrees') 'Malformed JDK lock was not refused by schema validation.'
  }
  Write-Host 'PASS malformed, duplicate, unknown and missing launcher/JDK fields'

  $fixture = New-Fixture 'concurrent atomic installs'
  Lock-Jdk $fixture
  $runs = @(1..3 | ForEach-Object { Start-Launcher $fixture @("parallel-$_") })
  foreach ($run in $runs) {
    # Other launches may still own staging until all concurrent children finish.
    Check ($run.Process.WaitForExit(60000)) 'Concurrent launcher timed out.'
  }
  for ($index = 0; $index -lt $runs.Count; $index++) {
    $actual = Finish-Launcher $runs[$index] 0 ((Java-Args $fixture.Remote) + @('parallel-' + ($index + 1)))
    Check ((Hash $actual.Java) -eq (Hash $program)) 'Concurrent install published different Java bytes.'
  }
  Check ((Hash $fixture.Remote) -eq $jarSha -and (Hash ($fixture.Cache + '\jdks\' + $zipSha + '\archive')) -eq $zipSha) 'Concurrent cache publication was not complete.'
  foreach ($install in Get-ChildItem -LiteralPath ($fixture.Cache + '\jdks\' + $zipSha) -Directory -Filter 'install.*') {
    Check ((Hash ($install.FullName + '\home\bin\java.exe')) -eq (Hash $program)) 'An incomplete JDK install was visible.'
  }
  Check (@(Get-ChildItem -LiteralPath ($fixture.Cache + '\jdks\' + $zipSha) -Recurse -Directory -Filter 'unpack').Count -eq 0) 'A racing install nested an unpack directory.'
  Write-Host 'PASS concurrent launcher/archive publication and atomic JDK installs'

  foreach ($name in @('jdk.lock','toolchain.local')) {
    $fixture=New-Fixture ('directory '+$name)
    [IO.Directory]::CreateDirectory(($fixture.Root+'\.histos\'+$name)) | Out-Null
    $actual=Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
    Check ($actual.Output.Contains('invalid file path')) 'A present directory activated PATH trust.'
  }

  foreach ($kind in @('directory','dangling','malformed','url')) {
    $fixture=New-Fixture ('local plus invalid lock '+$kind)
    [IO.Directory]::CreateDirectory(($fixture.Root+'\local\bin')) | Out-Null
    [IO.File]::Copy($program,($fixture.Root+'\local\bin\java.exe'))
    [IO.File]::WriteAllText(($fixture.Root+'\.histos\toolchain.local'),($fixture.Root+'\local'))
    $path=$fixture.Root+'\.histos\jdk.lock'
    if ($kind -eq 'directory') { [IO.Directory]::CreateDirectory($path) | Out-Null }
    elseif ($kind -eq 'dangling') {
      $missing=$fixture.Root+'\missing'
      [IO.Directory]::CreateDirectory($missing) | Out-Null
      New-Item -ItemType Junction -Path $path -Target $missing | Out-Null
      [IO.Directory]::Delete($missing)
    } elseif ($kind -eq 'malformed') { [IO.File]::WriteAllText($path,"major 21`nversion bad`n") }
    else { [IO.File]::WriteAllText($path,"major 21`nversion 21.0.2`narchive macos-aarch64 file:///tmp/%00.zip $zipSha`n") }
    $actual=Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
    Check (!$actual.Java) 'A valid local override bypassed a present invalid lock.'
  }
  Write-Host 'PASS valid local override plus invalid present lock'

  # These validators come from the committed template, not a second test implementation.
  function Fail([string] $message) { Write-Error $message }
  $begin=$source.IndexOf('function ValidUrl('); $end=$source.IndexOf('function Fetch(',$begin)
  Check ($begin -ge 0 -and $end -gt $begin) 'Cannot locate committed URL/link validators.'
  . ([scriptblock]::Create($source.Substring($begin,$end-$begin)))
  $lockBegin=$source.IndexOf('function Lock('); $lockEnd=$source.IndexOf('function SafeName(',$lockBegin)
  Check ($lockBegin -ge 0 -and $lockEnd -gt $lockBegin) 'Cannot locate committed lock validator.'
  . ([scriptblock]::Create($source.Substring($lockBegin,$lockEnd-$lockBegin)))
  function CheckArchiveSchema([string] $raw, [bool] $expected) {
    $path=Join-Path $scope 'corpus.lock'
    [IO.File]::WriteAllText($path,"major 21`nversion 21.0.2`narchive windows-x86_64 file:///C:/foreign/selected.zip $zipSha`narchive macos-aarch64 $raw $zipSha`n")
    $accepted=$true
    try { Lock $path jdk | Out-Null } catch { $accepted=$false }
    Check ($accepted -eq $expected) ('Unselected archive schema disagreement: '+$raw)
  }
  foreach ($line in [IO.File]::ReadAllLines((Join-Path $PSScriptRoot 'launcher-urls.tsv'))) {
    if ($line.StartsWith('#')) { continue }
    $row=$line.Split("`t"); Check ($row.Length -eq 3) 'Malformed URL fixture row.'
    $raw=[Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($row[2])); $accepted=$true
    try { ValidUrl $raw $row[0] } catch { $accepted=$false }
    Check ($accepted -eq ($row[1] -eq '1')) ('URL corpus disagreement: '+$raw)
    if ($row[0] -eq 'archive') { CheckArchiveSchema $raw ($row[1] -eq '1') }
  }
  foreach ($line in [IO.File]::ReadAllLines((Join-Path $PSScriptRoot 'runtime-lock\urls.tsv'))) {
    $row=$line.Split("`t"); Check ($row.Length -eq 2) 'Malformed runtime URL fixture row.'
    $raw=$row[1].Replace('{TAB}',"`t").Replace('{CR}',"`r").Replace('{LF}',"`n").Replace('{NUL}',[string][char]0)
    $accepted=$true; try { ValidUrl $raw archive } catch { $accepted=$false }
    Check ($accepted -eq ($row[0] -eq 'accept')) ('Runtime URL corpus disagreement: '+$raw)
    CheckArchiveSchema $raw ($row[0] -eq 'accept')
  }
  $graph=@{}; AddGraph $graph 'd' d ''; AddGraph $graph 'd/a' d ''; AddGraph $graph 'd/a/up' l '..'; AddGraph $graph 'escape' l 'd/a/up/../..'
  $rejected=$false; try { CheckGraph $graph } catch { $rejected=$true }
  Check $rejected 'Composed escaping links passed the archived link graph.'
  $graph=@{}; AddGraph $graph 'file' '-' ''; AddGraph $graph 'alias' h 'file'; CheckGraph $graph
  $rejected=$false; try { AddGraph $graph 'FILE' '-' '' } catch { $rejected=$true }
  Check $rejected 'Windows case aliases were accepted.'
  Write-Host 'PASS shared URL corpus, hardlinks, composed links and case aliases'

  $fixture=New-Fixture 'shared cache ACL'
  [IO.Directory]::CreateDirectory($fixture.Cache) | Out-Null
  $acl=Get-Acl -LiteralPath $fixture.Cache
  $acl.SetOwner([Security.Principal.WindowsIdentity]::GetCurrent().User)
  $everyone=New-Object Security.Principal.SecurityIdentifier('S-1-1-0')
  $rule=New-Object Security.AccessControl.FileSystemAccessRule($everyone,'Modify','ContainerInherit,ObjectInherit','None','Allow')
  $acl.AddAccessRule($rule); Set-Acl -LiteralPath $fixture.Cache -AclObject $acl
  $actual=Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
  Check ($actual.Output.Contains('unsafe shared cache ACL')) 'Shared writable cache ACL was accepted.'
  Write-Host 'PASS cache ACL boundary and full runtime permissions/link integrity'

  $fixture=New-Fixture 'job lifetime'
  $pids=Join-Path $fixture.Root 'pids'; [IO.Directory]::CreateDirectory($pids) | Out-Null
  $nonce=[Guid]::NewGuid().ToString('N')
  $ready=New-Object Threading.EventWaitHandle($false,'ManualReset',('Local\histos-ready-'+$nonce))
  $stop=New-Object Threading.EventWaitHandle($false,'ManualReset',('Local\histos-stop-'+$nonce))
  $owned=New-Object 'Collections.Generic.List[Diagnostics.Process]'
  try {
    $environment=Source-Env $fixture
    $environment.HISTOS_FIXTURE_LIFETIME='yes'; $environment.HISTOS_FIXTURE_PIDS=$pids
    $environment.HISTOS_FIXTURE_READY='Local\histos-ready-'+$nonce; $environment.HISTOS_FIXTURE_STOP='Local\histos-stop-'+$nonce
    $run=Start-Launcher $fixture @() $environment
    Check ($ready.WaitOne(30000)) 'Java descendants did not become ready.'
    foreach ($role in @('java','descendant','grandchild')) {
      $id=[int][IO.File]::ReadAllText((Join-Path $pids ($role+'.pid')))
      $process=[Diagnostics.Process]::GetProcessById($id); $handle=$process.Handle; $owned.Add($process)
    }
    $javaId=$owned[0].Id
    $parent=(Get-CimInstance Win32_Process -Filter ('ProcessId='+$javaId)).ParentProcessId
    $hostProcess=[Diagnostics.Process]::GetProcessById($parent)
    try { $hostProcess.Kill(); $hostProcess.WaitForExit() } finally { $hostProcess.Dispose() }
    foreach ($process in $owned) { Check ($process.WaitForExit(15000)) 'A Java descendant survived closing the bootstrap job.' }
    Check ($run.Process.WaitForExit(15000) -and $run.Process.ExitCode -ne 0) 'Interrupted bootstrap did not fail.'
    Check (@(Get-ChildItem -LiteralPath $fixture.Temp -Force).Count -eq 0) 'Interrupted batch staging leaked.'
    Check (@(Get-ChildItem -LiteralPath $fixture.Cache -Directory -Filter '.bootstrap.*').Count -eq 0) 'Interrupted runtime staging leaked.'
  } finally {
    $stop.Set() | Out-Null
    foreach ($process in $owned) { $process.Dispose() }
    $ready.Dispose(); $stop.Dispose()
  }
  Write-Host 'PASS interrupted bootstrap job owns child and grandchild lifetime'

  $fixture = New-Fixture 'unsafe ZIP destination'
  $unsafe = Join-Path $fixture.Root 'jdk.zip'
  [IO.File]::Delete($unsafe)
  $archive = [IO.Compression.ZipFile]::Open($unsafe, [IO.Compression.ZipArchiveMode]::Create)
  try {
    $entry = $archive.CreateEntry('../escaped.txt')
    $stream = $entry.Open()
    try { $stream.WriteByte(1) } finally { $stream.Dispose() }
  } finally { $archive.Dispose() }
  $unsafeSha = Hash $unsafe
  $url = ([Uri]$unsafe).AbsoluteUri
  [IO.File]::WriteAllText(($fixture.Root + '\.histos\jdk.lock'), "major 21`nversion 21.0.2`narchive $hostKey $url $unsafeSha`n")
  $actual = Finish-Launcher (Start-Launcher $fixture @() (Source-Env $fixture)) 1
  Check ($actual.Output.Contains('unsafe jdk archive') -and @(Get-ChildItem -LiteralPath $fixture.Root -Recurse -File -Filter 'escaped.txt').Count -eq 0) 'ZIP extraction escaped its destination.'
  Check (@([IO.File]::ReadAllLines($requests) | Where-Object { $_ -cne $requestPath }).Count -eq 0) 'Normal startup requested metadata or a sidecar.'
  Write-Host 'PASS unsafe ZIP destination and no startup metadata/sidecar reads'
  Write-Host ('Windows bootstrap fixtures passed under ' + $PSVersionTable.PSVersion)
} finally {
  $cleanupFailed = $false
  try {
    foreach ($process in $children) {
      try { Stop-Tree $process } catch {
        $cleanupFailed = $true
        Write-Warning ('Could not stop fixture process: ' + $_.Exception.Message)
      } finally { $process.Dispose() }
    }
  } finally {
    if (Test-Path -LiteralPath $scope) { Remove-Item -LiteralPath $scope -Recurse -Force }
  }
  Check (!$cleanupFailed) 'Fixture subprocess cleanup failed.'
}
