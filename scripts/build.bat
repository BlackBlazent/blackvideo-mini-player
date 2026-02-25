@echo off
setlocal enabledelayedexpansion

echo ==========================================
echo  BlackVideo Mini Player - Windows Build
echo ==========================================
echo.

:: ── Navigate to project root ──────────────────────────────────────────────
if exist "blackvideo_player.gpr" (
    echo [INFO] Working directory: %CD%
) else if exist "..\blackvideo_player.gpr" (
    cd /d "%~dp0.."
    echo [INFO] Working directory: %CD%
) else (
    echo ERROR: Cannot find blackvideo_player.gpr
    pause & exit /b 1
)
echo.

:: ── Delete old conflicting file ───────────────────────────────────────────
if exist "bindings\sdl2\sdl-video.adb" (
    del /q "bindings\sdl2\sdl-video.adb"
    echo [FIX] Deleted old bindings\sdl2\sdl-video.adb
    echo.
)

:: ── Find gprbuild ─────────────────────────────────────────────────────────
set "GPRBUILD="
for %%x in (gprbuild.exe) do set "GPRBUILD=%%~$PATH:x"
if defined GPRBUILD goto :found_gprbuild
if exist "C:\gnat\2021\bin\gprbuild.exe" set "GPRBUILD=C:\gnat\2021\bin\gprbuild.exe" & goto :found_gprbuild
for %%D in ("C:\gnat\2022\bin" "C:\gnat\2023\bin" "C:\GNAT\bin") do (
    if exist "%%~D\gprbuild.exe" set "GPRBUILD=%%~D\gprbuild.exe" & goto :found_gprbuild
)
:found_gprbuild
if not defined GPRBUILD (
    echo ERROR: gprbuild.exe not found. Add C:\gnat\2021\bin to PATH.
    pause & exit /b 1
)
echo [OK] gprbuild: %GPRBUILD%
echo.

:: ── Check FFmpeg headers (needed to compile ffmpeg_helpers.c) ─────────────
echo Checking FFmpeg headers in lib\include...
if not exist "lib\include\libavformat\avformat.h" (
    echo   MISSING: lib\include\libavformat\avformat.h
    echo.
    echo   You need FFmpeg dev headers. Copy from your FFmpeg package:
    echo     ffmpeg-8.0.1-full_build-shared\include\  ->  lib\include\
    echo.
    echo   Or download headers-only from:
    echo     https://github.com/BtbN/FFmpeg-Builds/releases
    echo     (get the -dev or -lgpl-shared build which includes include\)
    echo.
    pause & exit /b 1
) else (
    echo   [OK] FFmpeg headers present.
)
echo.

:: ── Check import libraries ────────────────────────────────────────────────
echo Checking lib\ import libraries...
set "MISSING=0"

if not exist "lib\libSDL2.a"     ( echo   MISSING: lib\libSDL2.a     & set "MISSING=1" )
if not exist "lib\libSDL2main.a" ( echo   MISSING: lib\libSDL2main.a & set "MISSING=1" )

:: Copy .dll.a to .a if plain .a not present (MinGW needs plain .a name)
for %%N in (avcodec avformat avutil swscale swresample) do (
    if not exist "lib\lib%%N.a" (
        if exist "lib\lib%%N.dll.a" (
            copy /y "lib\lib%%N.dll.a" "lib\lib%%N.a" >nul
            echo   [FIX] lib\lib%%N.dll.a -> lib\lib%%N.a
        ) else (
            echo   MISSING: lib\lib%%N.a
            set "MISSING=1"
        )
    )
)

if "%MISSING%"=="1" (
    echo.
    echo ERROR: Copy missing files into lib\ then retry.
    pause & exit /b 1
)
echo   [OK] All import libs ready.
echo.

:: ── Create build dirs ─────────────────────────────────────────────────────
if not exist "build\obj" mkdir "build\obj"

:: ── Build ─────────────────────────────────────────────────────────────────
echo Building...
echo.
"%GPRBUILD%" -P blackvideo_player.gpr -XOS_TARGET=windows -j0

if errorlevel 1 (
    echo.
    echo ==========================================
    echo  BUILD FAILED — see errors above
    echo ==========================================
    pause & exit /b 1
)

echo.
echo ==========================================
echo  BUILD SUCCESS: build\blackvideo-player.exe
echo ==========================================
echo.

:: ── Copy runtime DLLs to build\ ──────────────────────────────────────────
echo Copying runtime DLLs to build\ ...
for %%F in (SDL2.dll avcodec-62.dll avdevice-62.dll avfilter-11.dll avformat-62.dll avutil-60.dll swresample-6.dll swscale-9.dll) do (
    if exist "lib\%%F" ( copy /y "lib\%%F" "build\%%F" >nul & echo   Copied %%F )
)
echo.
echo Run:  build\blackvideo-player.exe "C:\path\to\video.mp4"
echo.
pause
