@echo off
where powershell.exe >nul 2>&1
if not errorlevel 1 (
	powershell.exe -NoLogo -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "%~dp0tm_powershell.ps1"
) else (
	pwsh.exe -NoLogo -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "%~dp0tm_powershell.ps1"
)