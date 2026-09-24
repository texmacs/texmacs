param(
    [int]$Port = 8085
)

Write-Output 'LanguageTool server startup'
Write-Output 'Checking for Java 17 or newer...'

$javaPath = [string](Get-ChildItem 'C:\Program Files\Microsoft\jdk-17*\bin\java.exe' -ErrorAction SilentlyContinue |
    Sort-Object FullName -Descending | Select-Object -ExpandProperty FullName -First 1)

if ([string]::IsNullOrWhiteSpace($javaPath)) {
    $cmdJava = (Get-Command java -ErrorAction SilentlyContinue)
    if ($null -ne $cmdJava) {
        $javaPath = $cmdJava.Source
    }
}

if ([string]::IsNullOrWhiteSpace($javaPath)) {
    $localAppData = [Environment]::GetFolderPath([Environment+SpecialFolder]::LocalApplicationData)
    $wingetPath = Join-Path $localAppData 'Microsoft\WindowsApps\winget.exe'
    if (-not (Test-Path -LiteralPath $wingetPath)) {
        throw 'winget.exe was not found.'
    }
    Write-Output 'Java 17 was not found. Starting Microsoft OpenJDK 17 installation...'
    Write-Output 'Accept the Windows administrator prompt if it appears.'
    $wingetPath = (Resolve-Path -LiteralPath $wingetPath).Path
    $wingetProcess = Start-Process -FilePath $wingetPath -NoNewWindow -Wait -PassThru -ArgumentList @(
        'install', '--id', 'Microsoft.OpenJDK.17', '--exact',
        '--accept-source-agreements', '--accept-package-agreements')
    if ($wingetProcess.ExitCode -ne 0) {
        throw ('OpenJDK 17 installation failed with exit code ' + $wingetProcess.ExitCode + '.')
    }
    $javaPath = [string](Get-ChildItem 'C:\Program Files\Microsoft\jdk-17*\bin\java.exe' -ErrorAction SilentlyContinue |
        Sort-Object FullName -Descending | Select-Object -ExpandProperty FullName -First 1)
}

if ([string]::IsNullOrWhiteSpace($javaPath)) {
    throw 'Java 17 or newer was not found after installation.'
}

$javaPath = (Resolve-Path -LiteralPath $javaPath).Path
Write-Output ('Using Java: ' + $javaPath)

$root = Join-Path $env:TEMP 'TeXmacs-LanguageTool'
$zip = Join-Path $root 'LanguageTool-latest-snapshot.zip'
Write-Output ('Preparing temporary directory: ' + $root)
New-Item -ItemType Directory -Force -Path $root | Out-Null

if (Test-Path $zip) {
    Write-Output 'Using the previously downloaded LanguageTool archive.'
} else {
    Write-Output 'Downloading the latest LanguageTool snapshot...'
    $client = [System.Net.WebClient]::new()
    try {
        $client.DownloadFile('https://internal1.languagetool.org/snapshots/LanguageTool-latest-snapshot.zip', $zip)
    } finally {
        $client.Dispose()
    }
    Write-Output 'LanguageTool download completed.'
}

$jar = Get-ChildItem -Path $root -Filter 'languagetool-server.jar' -Recurse -ErrorAction SilentlyContinue | Select-Object -First 1
if ($null -eq $jar) {
    Write-Output 'Extracting LanguageTool...'
    Expand-Archive -Path $zip -DestinationPath $root -Force
    $jar = Get-ChildItem -Path $root -Filter 'languagetool-server.jar' -Recurse | Select-Object -First 1
    if ($null -eq $jar) {
        throw 'languagetool-server.jar was not found after extraction.'
    }
}

Set-Location $jar.DirectoryName
Write-Output ("Starting LanguageTool on http://localhost:" + $Port)
Start-Process -FilePath $javaPath -NoNewWindow -Wait -ArgumentList @(
    '-cp', $jar.FullName, 'org.languagetool.server.HTTPServer',
    '--port', [string]$Port, '--allow-origin')
