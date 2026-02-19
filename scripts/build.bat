@echo off
:: build.bat - Build BlackVideo Mini Player for Windows
:: Requirements: GNAT (MSYS2 / GNAT Studio / Alire), SDL2, FFmpeg

echo ==========================================
echo  BlackVideo Mini Player - Windows Build
echo ==========================================

:: Check for gprbuild
where gprbuild >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: gprbuild not found.
    echo Install GNAT via Alire: https://alire.ada.dev/
    exit /b 1
)

:: Create build dir
if not exist "build\obj" mkdir "build\obj"

echo Building...

gprbuild -P blackvideo_player.gpr ^
    -XOS_TARGET=windows ^
    -j0 ^
    --RTS=native

if %errorlevel% neq 0 (
    echo BUILD FAILED.
    exit /b 1
)

echo.
echo Build complete: build\blackvideo-player.exe
echo.
echo Run with:
echo   build\blackvideo-player.exe your-video.mp4
echo.
echo Make sure these DLLs are in the same folder as the exe:
echo   SDL2.dll, avcodec-61.dll, avformat-61.dll
echo   avutil-59.dll, swscale-8.dll, swresample-5.dll
