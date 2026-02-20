@echo off
echo ============================================================
echo  BlackVideo Mini Player - Remove Context Menu
echo ============================================================
echo.

net session >nul 2>&1
if errorlevel 1 (
    echo ERROR: Requires Administrator. Right-click → Run as administrator.
    pause
    exit /b 1
)

set "REG_FILE=%~dp0uninstall_context_menu.reg"
if not exist "%REG_FILE%" (
    echo ERROR: uninstall_context_menu.reg not found beside this script.
    pause
    exit /b 1
)

regedit /s "%REG_FILE%"
if errorlevel 1 (
    echo ERROR: Registry removal failed.
    pause
    exit /b 1
)

echo Done. Context menu entries removed.
echo.
pause
