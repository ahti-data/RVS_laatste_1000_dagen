# Eenmalige installatie voor projectmanagers: voegt de veiligheidsregels uit pm-settings.json toe aan
# je persoonlijke Claude Code-instellingen (~\.claude\settings.json). Data-analisten draaien dit niet.
#
#   powershell -ExecutionPolicy Bypass -File .claude\pm\setup-pm.ps1
#
# Bestaande instellingen blijven behouden; er wordt eerst een back-up gemaakt. Opnieuw draaien is veilig.
param(
    [string]$SettingsPath = (Join-Path $env:USERPROFILE '.claude\settings.json')
)
$ErrorActionPreference = 'Stop'

$source = Join-Path $PSScriptRoot 'pm-settings.json'
$rules = @((Get-Content $source -Raw | ConvertFrom-Json).permissions.deny)

$dir = Split-Path $SettingsPath -Parent
if (-not (Test-Path $dir)) { New-Item -ItemType Directory -Path $dir | Out-Null }

$cfg = $null
if (Test-Path $SettingsPath) {
    Copy-Item $SettingsPath ($SettingsPath + '.bak-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
    $raw = Get-Content $SettingsPath -Raw
    if ($raw -and $raw.Trim()) { $cfg = $raw | ConvertFrom-Json }
}
if (-not $cfg) { $cfg = [pscustomobject]@{} }
if (-not $cfg.PSObject.Properties['permissions']) {
    $cfg | Add-Member -NotePropertyName permissions -NotePropertyValue ([pscustomobject]@{})
}
if (-not $cfg.permissions.PSObject.Properties['deny']) {
    $cfg.permissions | Add-Member -NotePropertyName deny -NotePropertyValue @()
}

$merged = @(@($cfg.permissions.deny) + $rules | Where-Object { $_ } | Select-Object -Unique)
$cfg.permissions.deny = $merged

$json = $cfg | ConvertTo-Json -Depth 20
[System.IO.File]::WriteAllText($SettingsPath, $json, (New-Object System.Text.UTF8Encoding($false)))
Write-Host "Klaar: $($rules.Count) regels toegevoegd aan $SettingsPath"
