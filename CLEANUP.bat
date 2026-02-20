@echo off
:: CLEANUP.bat — Run this ONCE before building if you have old project files.
:: Deletes files from previous versions that break the current build.

echo Cleaning up old conflicting files...

if exist "bindings\sdl2\sdl-video.adb" (
    del /q "bindings\sdl2\sdl-video.adb"
    echo   Deleted: bindings\sdl2\sdl-video.adb
) else (
    echo   OK: bindings\sdl2\sdl-video.adb not present
)

if exist "build\obj" (
    rmdir /s /q "build\obj"
    mkdir "build\obj"
    echo   Cleared: build\obj\
)

echo.
echo Done. Now run:  scripts\build.bat
echo.
pause
