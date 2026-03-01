# =========================================
# BlackVideo Real-Time AI Commit Script (DeepSeek)
# =========================================
# Requirements:
# - Git installed and in PATH
# - DeepSeek AI API key set in environment variable $env:DEEPSEEK_API_KEY
# =========================================

# --- Configuration ---
$RepoPath = "C:\Users\Vilma E. Agripo\Documents\JednazLonestamp\Projects\Computer.Programs\BlackBlazent\Projects\BlackVideo\BlackVideo-Utilities\blackvideo-mini-player"
$IncludePatterns = @(
    "src\*.adb","src\*.ads",
    "bindings\*.adb","bindings\*.ads",
    "config\*.ads","config\*.gpr","config\*.h",
    "scripts\*.bat","scripts\*.sh",
    "tools\*.bat","tools\*.sh",
    "*.md","*.txt"
)
$ExcludeFolders = @("build","dist","obj","lib\include","utils","samples","__docs__","release","Output","error","backup",".VSCodeCounter")
$BranchName = "main"
$DebounceSeconds = 5  # Wait time to aggregate rapid changes

# --- Load .env if exists (after RepoPath is set) ---
$envFile = Join-Path $RepoPath ".env"
if (Test-Path $envFile) {
    Get-Content $envFile | ForEach-Object {
        if ($_ -match "^\s*([^#][\w]+)\s*=\s*(.+)\s*$") {
            $name = $matches[1]
            $value = $matches[2]
            [System.Environment]::SetEnvironmentVariable($name, $value, "Process")
        }
    }
    Write-Host ".env loaded"
}

# --- Helper: Generate AI commit message with DeepSeek ---
function Get-AICommitMessage {
    param($FileDiffs)

    $apiKey = $env:DEEPSEEK_API_KEY
    if (-not $apiKey) { return "[auto] Updated files" }

    $prompt = "Write a concise git commit message for the following file changes, categorize with [src], [script], [doc]:`n$FileDiffs"

    $body = @{
        input = $prompt
        model = "commit-gen"
        max_tokens = 60
    } | ConvertTo-Json -Depth 3

    try {
        $response = Invoke-RestMethod -Uri "https://api.deepseek.ai/v1/commit" `
                    -Headers @{ Authorization = "Bearer $apiKey" } `
                    -Method Post -ContentType "application/json" -Body $body
        return $response.commit_message
    } catch {
        Write-Warning "DeepSeek API failed: $_"
        return "[auto] Updated files"
    }
}

# --- File System Watcher ---
$fsw = New-Object System.IO.FileSystemWatcher
$fsw.Path = $RepoPath
$fsw.IncludeSubdirectories = $true
$fsw.EnableRaisingEvents = $true
$fsw.NotifyFilter = [System.IO.NotifyFilters]'FileName, LastWrite, Size'

# --- Event Handler ---
$timer = $null

$action = {
    param($source, $e)

    cd $using:RepoPath

    # Stage all changes: modified + untracked, excluding ignored folders
    git add -A
    $stagedFiles = git diff --cached --name-only | Where-Object {
        $relativePath = $_
        foreach ($folder in $using:ExcludeFolders) {
            if ($relativePath -like "$folder*") { return $false }
        }
        $match = $false
        foreach ($pattern in $using:IncludePatterns) {
            if ($relativePath -like $pattern) { $match = $true; break }
        }
        $match
    }

    if (-not $stagedFiles) { return }

    # Debounce multiple rapid events
    if ($global:timer -ne $null) { $global:timer.Stop() }
    $global:timer = New-Object Timers.Timer $using:DebounceSeconds*1000
    $global:timer.AutoReset = $false
    $global:timer.Add_Elapsed({
        $diffText = git diff --cached
        $commitMessage = Get-AICommitMessage -FileDiffs $diffText

        git commit -m "$commitMessage"
        git push origin $using:BranchName

        Write-Host "Committed & pushed: $commitMessage"

        $global:timer = $null
    })
    $global:timer.Start()
}

# --- Register Events ---
Register-ObjectEvent $fsw Changed -Action $action
Register-ObjectEvent $fsw Created -Action $action
Register-ObjectEvent $fsw Renamed -Action $action
Register-ObjectEvent $fsw Deleted -Action $action

Write-Host "Watching $RepoPath for changes..."
while ($true) { Start-Sleep -Seconds 2 }