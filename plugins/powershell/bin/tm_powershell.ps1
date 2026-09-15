$ErrorActionPreference = 'Continue'
$InformationPreference = 'Continue'

$rawInputEncoding = [Console]::InputEncoding
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
$OutputEncoding = [System.Text.UTF8Encoding]::new($false)

$dataBegin = [char]2
$dataEnd = [char]5
$dataEscape = [char]27

function Convert-FromHostEncoding([string]$str) {
    if ($rawInputEncoding.CodePage -eq 65001) {
        return $str
    }
    try {
        $bytes = $rawInputEncoding.GetBytes($str)
        return [System.Text.Encoding]::UTF8.GetString($bytes)
    }
    catch {
        return $str
    }
}

function Write-TexmacsOutput([string]$text) {
    Write-TexmacsBegin
    Write-TexmacsChunk $text
    Write-TexmacsEnd
}

function Write-TexmacsBegin {
    [Console]::Out.Write("$dataBegin`utf8:")
    [Console]::Out.Flush()
}

function Write-TexmacsChunk([string]$text) {
    $text = $text.Replace("`r`n", "`n").Replace("`r", "`n")
    $escaped = $text.Replace([string]$dataEscape, ([string]$dataEscape + [string]$dataEscape))
    $escaped = $escaped.Replace([string]$dataBegin, ([string]$dataEscape + [string]$dataBegin))
    $escaped = $escaped.Replace([string]$dataEnd, ([string]$dataEscape + [string]$dataEnd))
    [Console]::Out.Write($escaped)
    [Console]::Out.Flush()
}

function Write-TexmacsEnd {
    [Console]::Out.Write($dataEnd)
    [Console]::Out.Flush()
}

function Write-TexmacsPrompt {
    [Console]::Out.Write("$dataBegin`prompt#PowerShell] $dataEnd")
    [Console]::Out.Flush()
}

$inputLines = [System.Collections.Generic.List[string]]::new()
Write-TexmacsPrompt
foreach ($line in $input) {
    if ($line -ne '<EOF>') {
        $inputLines.Add($line)
        continue
    }

    $rawCommand = [string]::Join([Environment]::NewLine, $inputLines)
    $command = Convert-FromHostEncoding $rawCommand
    $inputLines.Clear()
    Write-TexmacsBegin
    try {
        & { Invoke-Expression $command } *>&1 | ForEach-Object {
            $text = ($_ | Out-String).TrimEnd()
            if ($text) { Write-TexmacsChunk "$text`n" }
        }
    }
    catch {
        Write-TexmacsChunk ("PowerShell error: " + $_.Exception.Message + "`n" +
            $_.InvocationInfo.PositionMessage + "`n")
    }
    Write-TexmacsEnd
    Write-TexmacsPrompt
}
