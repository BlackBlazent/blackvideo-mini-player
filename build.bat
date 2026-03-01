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

:: ── Delete ALL stale .adb files that conflict with current architecture ───
set "STALE=0"
if exist "bindings\ffmpeg\ffmpeg-avcodec.adb"  ( del /q "bindings\ffmpeg\ffmpeg-avcodec.adb"  & echo [FIX] Deleted stale bindings\ffmpeg\ffmpeg-avcodec.adb  & set "STALE=1" )
if exist "bindings\ffmpeg\ffmpeg-avformat.adb" ( del /q "bindings\ffmpeg\ffmpeg-avformat.adb" & echo [FIX] Deleted stale bindings\ffmpeg\ffmpeg-avformat.adb & set "STALE=1" )
if exist "bindings\sdl2\sdl-video.adb"         ( del /q "bindings\sdl2\sdl-video.adb"         & echo [FIX] Deleted stale bindings\sdl2\sdl-video.adb          & set "STALE=1" )
if "%STALE%"=="1" echo.

:: ── Find GNAT tools ───────────────────────────────────────────────────────
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

:: GCC is in the same bin dir as gprbuild
for %%F in ("%GPRBUILD%") do set "GNAT_BIN=%%~dpF"
set "GCC=%GNAT_BIN%gcc.exe"
if not exist "%GCC%" (
    echo ERROR: gcc.exe not found at %GCC%
    pause & exit /b 1
)
echo [OK] gprbuild: %GPRBUILD%
echo [OK] gcc:      %GCC%
echo.

:: ── Check FFmpeg headers (required by src\ffmpeg_helpers.c) ──────────────
echo Checking FFmpeg headers in lib\include\ ...
set "HDR_OK=1"
for %%H in (libavformat\avformat.h libavcodec\avcodec.h libavutil\avutil.h) do (
    if not exist "lib\include\%%H" (
        echo   MISSING: lib\include\%%H
        set "HDR_OK=0"
    )
)
if "%HDR_OK%"=="0" (
    echo.
    echo   Copy the FFmpeg headers into lib\include\:
    echo     ffmpeg-8.0.1-full_build-shared\include\  -->  lib\include\
    echo.
    pause & exit /b 1
)
echo   [OK] FFmpeg headers present.
echo.

:: ── Check import libraries ────────────────────────────────────────────────
echo Checking lib\ import libraries...
set "MISSING=0"
if not exist "lib\libSDL2.a"     ( echo   MISSING: lib\libSDL2.a     & set "MISSING=1" )
if not exist "lib\libSDL2main.a" ( echo   MISSING: lib\libSDL2main.a & set "MISSING=1" )
for %%N in (avcodec avformat avutil swscale swresample) do (
    if not exist "lib\lib%%N.a" (
        if exist "lib\lib%%N.dll.a" (
            copy /y "lib\lib%%N.dll.a" "lib\lib%%N.a" >nul
            echo   [FIX] lib\lib%%N.dll.a copied to lib\lib%%N.a
        ) else (
            echo   MISSING: lib\lib%%N.a
            set "MISSING=1"
        )
    )
)
if "%MISSING%"=="1" (
    echo.
    echo ERROR: Copy missing .a files into lib\ then retry.
    pause & exit /b 1
)
echo   [OK] All import libs ready.
echo.

:: ── Create build dirs ─────────────────────────────────────────────────────
if not exist "build\obj" mkdir "build\obj"

:: ── STEP 1: Compile ffmpeg_helpers.c with gcc directly ───────────────────
:: This bypasses GPRbuild's mixed-language archive entirely.
:: The .o file is passed directly to the linker via blackvideo_player.gpr.
echo Compiling C helper (ffmpeg_helpers.c)...
"%GCC%" -O2 -c "src\ffmpeg_helpers.c" ^
    -I"lib\include" ^
    -o "build\obj\ffmpeg_helpers.o"

if errorlevel 1 (
    echo.
    echo ==========================================
    echo  C COMPILE FAILED - see errors above
    echo ==========================================
    pause & exit /b 1
)
echo   [OK] build\obj\ffmpeg_helpers.o
echo.

:: ── STEP 2: Build Ada sources and link everything ─────────────────────────
:: Pure Ada project now — no mixed-language archive, no ranlib needed.
:: ffmpeg_helpers.o is linked directly via GPR Linker switches.
echo Building Ada sources and linking...
echo.
"%GPRBUILD%" -P blackvideo_player.gpr -XOS_TARGET=windows -j0

if errorlevel 1 (
    echo.
    echo ==========================================
    echo  BUILD FAILED - see errors above
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
