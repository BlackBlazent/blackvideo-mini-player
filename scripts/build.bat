@echo off
setlocal enabledelayedexpansion

echo ==========================================
echo  BlackVideo Mini Player v2.3 - Windows Build
echo ==========================================
echo.

:: Navigate to project root
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

:: ROOT = full absolute path (handles spaces in path like "Vilma E. Agripo")
set "ROOT=%CD%"

:: ── Production vs Dev mode ──────────────────────────────────────────────────
:: In production: no .env file needed. Whisper is resolved from build\ at runtime.
:: In dev:        create build\.env with BLACKVIDEO_WHISPER_PATH and
::                BLACKVIDEO_WHISPER_MODEL to override search paths.
if exist "%ROOT%\.env" (
    echo [INFO] .env file found - development mode
    echo [INFO] Env vars will be loaded at runtime from .env
) else (
    echo [INFO] No .env file - production mode
    echo [INFO] Whisper resolved from build\ at runtime
)
echo.

:: ── Delete stale .adb files ─────────────────────────────────────────────────
set "STALE=0"
if exist "bindings\ffmpeg\ffmpeg-avcodec.adb"  ( del /q "bindings\ffmpeg\ffmpeg-avcodec.adb"  & echo [FIX] Deleted stale ffmpeg-avcodec.adb  & set "STALE=1" )
if exist "bindings\ffmpeg\ffmpeg-avformat.adb" ( del /q "bindings\ffmpeg\ffmpeg-avformat.adb" & echo [FIX] Deleted stale ffmpeg-avformat.adb & set "STALE=1" )
if exist "bindings\sdl2\sdl-video.adb"         ( del /q "bindings\sdl2\sdl-video.adb"         & echo [FIX] Deleted stale sdl-video.adb         & set "STALE=1" )
if "%STALE%"=="1" echo.

:: ── Find GNAT tools ─────────────────────────────────────────────────────────
set "GPRBUILD="
for %%x in (gprbuild.exe) do set "GPRBUILD=%%~$PATH:x"
if defined GPRBUILD goto :found_gprbuild
if exist "C:\gnat\2021\bin\gprbuild.exe" set "GPRBUILD=C:\gnat\2021\bin\gprbuild.exe" & goto :found_gprbuild
for %%D in ("C:\gnat\2022\bin" "C:\gnat\2023\bin" "C:\GNAT\bin") do (
    if exist "%%~D\gprbuild.exe" set "GPRBUILD=%%~D\gprbuild.exe" & goto :found_gprbuild
)
:: Search Alire toolchains
for /d %%D in ("%LOCALAPPDATA%\alire\toolchains\gnat_native_*") do (
    if exist "%%~D\bin\gprbuild.exe" set "GPRBUILD=%%~D\bin\gprbuild.exe" & goto :found_gprbuild
)
:found_gprbuild
if not defined GPRBUILD (
    echo ERROR: gprbuild.exe not found. Install Alire and run: alr toolchain --select
    pause & exit /b 1
)
for %%F in ("%GPRBUILD%") do set "GNAT_BIN=%%~dpF"
set "GCC=%GNAT_BIN%gcc.exe"
if not exist "%GCC%" ( echo ERROR: gcc.exe not found at %GCC% & pause & exit /b 1 )
echo [OK] gprbuild: %GPRBUILD%
echo [OK] gcc:      %GCC%
echo.

:: ── Check FFmpeg headers ────────────────────────────────────────────────────
echo Checking FFmpeg headers ...
set "HDR_OK=1"
for %%H in (libavformat\avformat.h libavcodec\avcodec.h libavutil\avutil.h) do (
    if not exist "%ROOT%\lib\include\%%H" ( echo   MISSING: lib\include\%%H & set "HDR_OK=0" )
)
if "%HDR_OK%"=="0" ( echo. & echo Copy FFmpeg headers to lib\include\ & echo. & pause & exit /b 1 )
echo   [OK] FFmpeg headers present.
echo.

:: ── Check SDL2 headers ──────────────────────────────────────────────────────
echo Checking SDL2 headers ...
set "SDL_I1="
set "SDL_I2="
if exist "%ROOT%\lib\include\SDL2\SDL.h" (
    echo   [OK] SDL.h  --  lib\include\SDL2\SDL.h
    set "SDL_I1=-I"%ROOT%\lib\include""
    set "SDL_I2=-I"%ROOT%\lib\include\SDL2""
) else if exist "%ROOT%\lib\include\SDL.h" (
    echo   [OK] SDL.h  --  lib\include\SDL.h
    set "SDL_I1=-I"%ROOT%\lib\include""
) else (
    echo   ERROR: SDL.h not found in lib\include\ or lib\include\SDL2\
    pause & exit /b 1
)
if exist "%ROOT%\lib\include\SDL2\SDL_ttf.h" (
    echo   [OK] SDL_ttf.h  --  lib\include\SDL2\SDL_ttf.h
) else if exist "%ROOT%\lib\include\SDL_ttf.h" (
    echo   [OK] SDL_ttf.h  --  lib\include\SDL_ttf.h
) else (
    echo   WARNING: SDL_ttf.h not found - UI overlay may not compile
)
echo.

:: ── Check import libraries ──────────────────────────────────────────────────
echo Checking lib\ import libraries ...
set "MISSING=0"
if not exist "%ROOT%\lib\libSDL2.a"     ( echo   MISSING: lib\libSDL2.a     & set "MISSING=1" )
if not exist "%ROOT%\lib\libSDL2main.a" ( echo   MISSING: lib\libSDL2main.a & set "MISSING=1" )
if not exist "%ROOT%\lib\libSDL2_ttf.dll.a" (
    if exist "%ROOT%\lib\libSDL2_ttf.a" (
        copy /y "%ROOT%\lib\libSDL2_ttf.a" "%ROOT%\lib\libSDL2_ttf.dll.a" >nul
        echo   [FIX] libSDL2_ttf.a copied to libSDL2_ttf.dll.a
    ) else (
        echo   MISSING: lib\libSDL2_ttf.dll.a
        set "MISSING=1"
    )
)
for %%N in (avcodec avformat avutil swscale swresample) do (
    if not exist "%ROOT%\lib\lib%%N.a" (
        if exist "%ROOT%\lib\lib%%N.dll.a" (
            copy /y "%ROOT%\lib\lib%%N.dll.a" "%ROOT%\lib\lib%%N.a" >nul
            echo   [FIX] lib%%N.dll.a -> lib%%N.a
        ) else ( echo   MISSING: lib\lib%%N.a & set "MISSING=1" )
    )
)
if "%MISSING%"=="1" ( echo. & echo ERROR: Copy missing .a files into lib\ & pause & exit /b 1 )
echo   [OK] All import libs ready.
echo.

:: ── Optional: check Whisper assets (non-fatal) ──────────────────────────────
echo Checking Whisper assets (optional) ...
set "WHISPER_OK=0"
if exist "%ROOT%\build\whisper-cli.exe" (
    echo   [OK] whisper-cli.exe found in build\
    set "WHISPER_OK=1"
) else (
    echo   [INFO] whisper-cli.exe not in build\ - set BLACKVIDEO_WHISPER_PATH in .env or place beside exe
)
if exist "%ROOT%\build\models\ggml-base.bin" (
    echo   [OK] ggml-base.bin found in build\models\
) else if exist "%ROOT%\build\ggml-base.bin" (
    echo   [OK] ggml-base.bin found in build\
) else (
    echo   [INFO] ggml-base.bin not found - set BLACKVIDEO_WHISPER_MODEL in .env or place in build\models\
)
echo.

:: ── Create build dirs ────────────────────────────────────────────────────────
if not exist "%ROOT%\build\obj" mkdir "%ROOT%\build\obj"

:: Remove stale ui_overlay.o
if exist "%ROOT%\build\obj\ui_overlay.o" (
    del /q "%ROOT%\build\obj\ui_overlay.o"
    echo [FIX] Deleted stale build\obj\ui_overlay.o
)

:: ── STEP 1: Compile ffmpeg_helpers.c ────────────────────────────────────────
echo Compiling src\ffmpeg_helpers.c ...
"%GCC%" -O2 -c "%ROOT%\src\ffmpeg_helpers.c" ^
    -I"%ROOT%\lib\include" ^
    -o "%ROOT%\build\obj\ffmpeg_helpers.o"
if errorlevel 1 ( echo. & echo FAILED: ffmpeg_helpers.c & pause & exit /b 1 )
echo   [OK] build\obj\ffmpeg_helpers.o
echo.

:: ── STEP 2: Compile csrc\ui_overlay.c ───────────────────────────────────────
echo Compiling csrc\ui_overlay.c ...
"%GCC%" -O2 -c "%ROOT%\csrc\ui_overlay.c" ^
    %SDL_I1% %SDL_I2% ^
    -o "%ROOT%\build\obj\ui_overlay_c.o"
if errorlevel 1 ( echo. & echo FAILED: ui_overlay.c & pause & exit /b 1 )
if not exist "%ROOT%\build\obj\ui_overlay_c.o" ( echo. & echo FAILED: ui_overlay_c.o not created & pause & exit /b 1 )
echo   [OK] build\obj\ui_overlay_c.o
echo.

:: ── STEP 3: Build Ada + link ──────────────────────────────────────────────────
echo Building Ada sources and linking ...
echo.
"%GPRBUILD%" -P blackvideo_player.gpr -XOS_TARGET=windows -j0
if errorlevel 1 (
    echo.
    echo ==========================================
    echo  BUILD FAILED
    echo ==========================================
    pause & exit /b 1
)

echo.
echo ==========================================
echo  BUILD SUCCESS: build\blackvideo-player.exe
echo ==========================================
echo.

:: ── Copy runtime DLLs ────────────────────────────────────────────────────────
echo Copying runtime DLLs to build\ ...
for %%F in (SDL2.dll SDL2_ttf.dll libfreetype-6.dll zlib1.dll avcodec-62.dll avdevice-62.dll avfilter-11.dll avformat-62.dll avutil-60.dll swresample-6.dll swscale-9.dll) do (
    if exist "%ROOT%\lib\%%F" ( copy /y "%ROOT%\lib\%%F" "%ROOT%\build\%%F" >nul & echo   Copied %%F )
)

:: ── Copy Whisper DLLs to build\ if present ──────────────────────────────────
echo.
echo Checking for Whisper DLLs to copy ...
for %%F in (whisper.dll ggml.dll ggml-base.dll ggml-cpu.dll) do (
    if exist "%ROOT%\lib\%%F" ( copy /y "%ROOT%\lib\%%F" "%ROOT%\build\%%F" >nul & echo   Copied %%F )
)

:: ── Create .env template if none exists ─────────────────────────────────────
if not exist "%ROOT%\.env" (
    echo.
    echo Creating .env.example (rename to .env for dev overrides) ...
    (
        echo # BlackVideo Mini Player - Development overrides
        echo # This file is loaded at runtime in dev mode only.
        echo # In a release build, delete this file - paths resolve automatically.
        echo #
        echo # Uncomment and set these if whisper-cli.exe is not beside the player exe:
        echo # BLACKVIDEO_WHISPER_PATH=C:\path\to\whisper-bin-x64\whisper-cli.exe
        echo # BLACKVIDEO_WHISPER_MODEL=C:\path\to\whisper-bin-x64\models\ggml-base.bin
    ) > "%ROOT%\.env.example"
    echo   Created .env.example
)

echo.
echo ── Whisper setup reminder ───────────────────────────────────────────────
echo   For offline captions, place these in build\ :
echo     whisper-cli.exe  ggml.dll  ggml-base.dll  ggml-cpu.dll  whisper.dll
echo     models\ggml-base.bin  (or set BLACKVIDEO_WHISPER_MODEL in .env)
echo ─────────────────────────────────────────────────────────────────────────
echo.
echo Run:  build\blackvideo-player.exe "C:\path\to\video.mp4"
echo.
pause
