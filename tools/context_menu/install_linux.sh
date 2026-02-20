#!/usr/bin/env bash
# install_linux.sh
# Installs blackvideo-player system-wide + registers it for right-click in file managers
# (Nautilus, Dolphin, Thunar, Nemo, Caja...)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_EXE="$SCRIPT_DIR/../../build/blackvideo-player"
INSTALL_DIR="/usr/local/bin"
DESKTOP_DIR="/usr/share/applications"
DESKTOP_FILE="$SCRIPT_DIR/blackvideo-player.desktop"

echo "============================================================"
echo " BlackVideo Player — Linux System Install + Right-click"
echo "============================================================"

# ── Check for the built executable ──────────────────────────────────────────
if [[ ! -f "$BUILD_EXE" ]]; then
    echo "ERROR: blackvideo-player not found at $BUILD_EXE"
    echo "Build it first:  ./scripts/build.sh"
    exit 1
fi

# ── Require root (for /usr/local/bin) ───────────────────────────────────────
if [[ $EUID -ne 0 ]]; then
    echo "This script needs sudo:"
    exec sudo bash "$0" "$@"
fi

echo "Installing binary to $INSTALL_DIR ..."
cp "$BUILD_EXE" "$INSTALL_DIR/blackvideo-player"
chmod 755 "$INSTALL_DIR/blackvideo-player"

echo "Installing .desktop file to $DESKTOP_DIR ..."
cp "$DESKTOP_FILE" "$DESKTOP_DIR/blackvideo-player.desktop"

echo "Updating MIME database ..."
update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
xdg-mime default blackvideo-player.desktop video/mp4    2>/dev/null || true
xdg-mime default blackvideo-player.desktop video/x-matroska 2>/dev/null || true
xdg-mime default blackvideo-player.desktop video/x-msvideo  2>/dev/null || true

echo ""
echo "Done! Right-click any video file and choose:"
echo "  'Open With' → 'BlackVideo Player'"
echo ""
echo "To uninstall:"
echo "  sudo rm $INSTALL_DIR/blackvideo-player"
echo "  sudo rm $DESKTOP_DIR/blackvideo-player.desktop"
echo "  sudo update-desktop-database"
