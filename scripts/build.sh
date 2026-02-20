#!/usr/bin/env bash
# build.sh — BlackVideo Mini Player: Linux / macOS
set -e

echo "========================================"
echo " BlackVideo Mini Player — Build Script  "
echo "========================================"

OS=$(uname -s)
echo "OS: $OS"

# ── Dependency checks ──────────────────────────────────────────────────────
check() {
    if ! command -v "$1" &>/dev/null; then
        echo "ERROR: '$1' not found."
        echo "  $2"
        exit 1
    fi
}

check gprbuild "Install via: alr toolchain --select  (https://alire.ada.dev/)"
check pkg-config "Install: apt install pkg-config  OR  brew install pkg-config"

pkg-config --exists sdl2 2>/dev/null || {
    echo "ERROR: SDL2 dev libraries not found."
    [[ "$OS" == "Linux" ]] && echo "  sudo apt install libsdl2-dev"
    [[ "$OS" == "Darwin" ]] && echo "  brew install sdl2"
    exit 1
}

pkg-config --exists libavcodec libavformat libavutil libswscale libswresample 2>/dev/null || {
    echo "ERROR: FFmpeg dev libraries not found."
    [[ "$OS" == "Linux" ]] && echo "  sudo apt install libavcodec-dev libavformat-dev libavutil-dev libswscale-dev libswresample-dev"
    [[ "$OS" == "Darwin" ]] && echo "  brew install ffmpeg"
    exit 1
}

echo "SDL2:   $(pkg-config --modversion sdl2)"
echo "FFmpeg: $(pkg-config --modversion libavcodec)"
echo ""

# ── Build directories ──────────────────────────────────────────────────────
mkdir -p build/obj

# ── Detect OS target ──────────────────────────────────────────────────────
if [[ "$OS" == "Darwin" ]]; then OS_TARGET="macos"
else OS_TARGET="linux"; fi

# ── Build ─────────────────────────────────────────────────────────────────
echo "Building ..."
gprbuild -P blackvideo_player.gpr \
    -XOS_TARGET="$OS_TARGET" \
    -j0

echo ""
echo "✓ Build complete: build/blackvideo-player"
echo ""
echo "Usage:"
echo "  ./build/blackvideo-player /path/to/video.mp4"
