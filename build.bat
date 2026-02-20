@echo off
setlocal enabledelayedexpansion

echo ==========================================
echo  BlackVideo Mini Player - Windows Build
echo ==========================================
echo.

:: ── Navigate to project root (where blackvideo_player.gpr lives) ──────────
:: Strategy: check current dir first, then try one level up (if run from scripts\)
if exist "blackvideo_player.gpr" (
    :: Already in project root — good, stay here
    echo [INFO] Working directory: %CD%
) else if exist "..\blackvideo_player.gpr" (
    :: Running from scripts\ subfolder — go up one level
    cd /d "%~dp0.."
    echo [INFO] Working directory: %CD%
) else (
    echo ERROR: Cannot find blackvideo_player.gpr
    echo        Run this script from the project root or from scripts\
    echo        Expected location: blackvideo-mini-player\blackvideo_player.gpr
    pause
    exit /b 1
)
echo.

:: ── WARN about leftover old files that break the build ────────────────────
if exist "bindings\sdl2\sdl-video.adb" (
    echo WARNING: Found old file bindings\sdl2\sdl-video.adb
    echo          This file must be DELETED - it conflicts with the new bindings.
    echo          Deleting it now...
    del /q "bindings\sdl2\sdl-video.adb"
    echo          Deleted.
    echo.
)

:: ── Find gprbuild ─────────────────────────────────────────────────────────
set "GPRBUILD="

:: 1) Already on PATH?
for %%x in (gprbuild.exe) do set "GPRBUILD=%%~$PATH:x"
if defined GPRBUILD goto :found_gprbuild

:: 2) GNAT 2021 (detected in your system)
if exist "C:\gnat\2021\bin\gprbuild.exe" (
    set "GPRBUILD=C:\gnat\2021\bin\gprbuild.exe"
    goto :found_gprbuild
)
:: 3) Other common installs
for %%D in ("C:\gnat\2022\bin" "C:\gnat\2023\bin" "C:\GNAT\bin" "C:\Program Files\Alire\bin" "C:\Alire\bin") do (
    if exist "%%~D\gprbuild.exe" (
        set "GPRBUILD=%%~D\gprbuild.exe"
        goto :found_gprbuild
    )
)
:: 4) Deep search in Alire toolchains
if exist "%LOCALAPPDATA%\alire" (
    for /r "%LOCALAPPDATA%\alire" %%f in (gprbuild.exe) do (
        if exist "%%f" ( set "GPRBUILD=%%f" & goto :found_gprbuild )
    )
)

:found_gprbuild
if not defined GPRBUILD (
    echo ERROR: gprbuild.exe not found.
    echo.
    echo You have GNAT 2021 at C:\gnat\2021 - add it to PATH:
    echo   System Properties -^> Environment Variables -^> System Path -^> New
    echo   Add: C:\gnat\2021\bin
    echo Then close and reopen this command prompt.
    pause
    exit /b 1
)
echo [OK] gprbuild: %GPRBUILD%
echo.

:: ── Check lib\ import libraries ───────────────────────────────────────────
echo Checking lib\ for required import libs...
set "WARN=0"
if not exist "lib\libSDL2.a" (
    echo   MISSING: lib\libSDL2.a
    echo     ^> Copy from SDL2-devel-2.0.5-mingw\x86_64-w64-mingw32\lib\libSDL2.a
    set "WARN=1"
)
for %%L in (libavcodec.dll.a libavformat.dll.a libavutil.dll.a libswscale.dll.a libswresample.dll.a) do (
    if not exist "lib\%%L" (
        echo   MISSING: lib\%%L
        echo     ^> Copy from ffmpeg-8.0.1-full_build-shared\lib\%%L
        set "WARN=1"
    )
)
if "%WARN%"=="1" (
    echo.
    echo   ^^^ These are needed for LINKING. Compilation will run but linking will fail.
    echo.
) else (
    echo   [OK] All import libs present.
    echo.
)

:: ── Create build dirs ─────────────────────────────────────────────────────
if not exist "build\obj" mkdir "build\obj"

:: ── Build ─────────────────────────────────────────────────────────────────
echo Building...
echo.
"%GPRBUILD%" -P blackvideo_player.gpr -XOS_TARGET=windows -j0

if errorlevel 1 (
    echo.
    echo ==========================================
    echo  BUILD FAILED
    echo ==========================================
    echo.
    echo See errors above. Common fixes:
    echo   1. lib\libSDL2.a missing        ^(copy from SDL2-devel MinGW package^)
    echo   2. lib\libavcodec.dll.a missing  ^(copy from FFmpeg shared build^)
    echo   3. Old sdl-video.adb still present ^(the script auto-deletes it^)
    echo.
    pause
    exit /b 1
)

echo.
echo ==========================================
echo  BUILD SUCCESS
echo  Output: build\blackvideo-player.exe
echo ==========================================
echo.
echo Copy these DLLs into build\ before running:
echo   SDL2.dll              from SDL2-devel-2.0.5-mingw\x86_64-w64-mingw32\bin\
echo   avcodec-62.dll        from ffmpeg-8.0.1-full_build-shared\bin\
echo   avformat-62.dll
echo   avutil-60.dll
echo   swresample-6.dll
echo   swscale-9.dll
echo   avdevice-62.dll
echo   avfilter-11.dll
echo.
echo Run:  build\blackvideo-player.exe "C:\path\to\video.mp4"
echo.
pause
