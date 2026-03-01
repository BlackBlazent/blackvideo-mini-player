:: ============================================================
:: BlackVideo Mini Player - Context Menu Installer
:: ============================================================

@echo off
setlocal enabledelayedexpansion

echo ============================================================
echo  BlackVideo Mini Player - Context Menu Installer
echo ============================================================
echo.
echo This will add "Open with BlackVideo Player" to the
echo right-click menu for: MP4, MKV, AVI, MOV, WMV, WebM,
echo                       FLV, M4V, MPG, MPEG, TS
echo.

:: ── Check admin rights ────────────────────────────────────────────────────
net session >nul 2>&1
if errorlevel 1 (
    echo ERROR: This script requires Administrator privileges.
    echo.
    echo Right-click this .bat file and choose "Run as administrator".
    pause
    exit /b 1
)

:: ── Locate the blackvideo-player.exe ─────────────────────────────────────
set "EXE_PATH="

:: Check in the same folder as this script (tools\context_menu\ → ..\..\build\)
set "SCRIPT_DIR=%~dp0"
set "CANDIDATE=%SCRIPT_DIR%..\..\build\blackvideo-player.exe"
if exist "%CANDIDATE%" (
    for %%f in ("%CANDIDATE%") do set "EXE_PATH=%%~ff"
    goto :found_exe
)

:: Check C:\BlackVideo\
if exist "C:\BlackVideo\blackvideo-player.exe" (
    set "EXE_PATH=C:\BlackVideo\blackvideo-player.exe"
    goto :found_exe
)

:: Ask user to locate it manually
echo Could not auto-detect blackvideo-player.exe.
set /p EXE_PATH="Enter full path to blackvideo-player.exe: "
if not exist "%EXE_PATH%" (
    echo ERROR: File not found: %EXE_PATH%
    pause
    exit /b 1
)

:found_exe
echo [OK] Using executable: %EXE_PATH%
echo.

:: ── Generate a temporary .reg file with the correct path ──────────────────
set "REG_TMP=%TEMP%\blackvideo_context_menu.reg"

:: Escape backslashes for registry (\ → \\)
set "EXE_ESC=%EXE_PATH:\=\\%"

(
echo Windows Registry Editor Version 5.00
echo.
echo [HKEY_CLASSES_ROOT\Applications\blackvideo-player.exe]
echo @="BlackVideo Mini Player"
echo.
echo [HKEY_CLASSES_ROOT\Applications\blackvideo-player.exe\shell]
echo.
echo [HKEY_CLASSES_ROOT\Applications\blackvideo-player.exe\shell\open]
echo @="Open with BlackVideo Player"
echo "Icon"="%EXE_ESC%,0"
echo.
echo [HKEY_CLASSES_ROOT\Applications\blackvideo-player.exe\shell\open\command]
echo @="\"%EXE_ESC%\" \"%%1\""
) > "%REG_TMP%"

:: Append per-extension entries using a helper macro
for %%E in (mp4 mkv avi mov wmv webm flv m4v mpg mpeg ts) do (
    echo.>> "%REG_TMP%"
    echo [HKEY_CLASSES_ROOT\.%%E\shell\BlackVideoPlayer]>> "%REG_TMP%"
    echo @="Open with BlackVideo Player">> "%REG_TMP%"
    echo "Icon"="%EXE_ESC%,0">> "%REG_TMP%"
    echo.>> "%REG_TMP%"
    echo [HKEY_CLASSES_ROOT\.%%E\shell\BlackVideoPlayer\command]>> "%REG_TMP%"
    echo @="\"%EXE_ESC%\" \"%%1\"">> "%REG_TMP%"
    echo.>> "%REG_TMP%"
    echo [HKEY_CLASSES_ROOT\.%%E\OpenWithList\blackvideo-player.exe]>> "%REG_TMP%"
    echo @="">> "%REG_TMP%"
)

:: ── Import into registry ──────────────────────────────────────────────────
regedit /s "%REG_TMP%"
if errorlevel 1 (
    echo ERROR: Registry import failed.
    del "%REG_TMP%" 2>nul
    pause
    exit /b 1
)

del "%REG_TMP%" 2>nul

echo.
echo ============================================================
echo  Done! Right-click any video file to see:
echo  "Open with BlackVideo Player"
echo.
echo  To remove: run uninstall_context_menu.bat (as admin)
echo ============================================================
echo.
pause
