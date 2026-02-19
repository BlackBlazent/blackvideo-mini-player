#!/usr/bin/env bash
# build.sh - Build BlackVideo Mini Player for Linux and macOS
# Requirements: GNAT, SDL2, FFmpeg development libraries

set -e

echo "╔══════════════════════════════════════════╗"
echo "║  BlackVideo Mini Player - Build Script   ║"
echo "╚══════════════════════════════════════════╝"

# ── Detect OS ───────────────────────────────────
OS=$(uname -s)
echo "Detected OS: $OS"

# ── Check dependencies ───────────────────────────
check_dep() {
    if ! command -v "$1" &> /dev/null; then
        echo "ERROR: $1 not found. Please install it."
        exit 1
    fi
}

check_dep gprbuild
check_dep pkg-config

# ── Check SDL2 ───────────────────────────────────
if ! pkg-config --exists sdl2; then
    echo "ERROR: SDL2 development libraries not found."
    if [[ "$OS" == "Linux" ]]; then
        echo "Install with: sudo apt install libsdl2-dev"
    elif [[ "$OS" == "Darwin" ]]; then
        echo "Install with: brew install sdl2"
    fi
    exit 1
fi

# ── Check FFmpeg ─────────────────────────────────
if ! pkg-config --exists libavcodec libavformat libavutil libswscale libswresample; then
    echo "ERROR: FFmpeg development libraries not found."
    if [[ "$OS" == "Linux" ]]; then
        echo "Install with: sudo apt install libavcodec-dev libavformat-dev libavutil-dev libswscale-dev libswresample-dev"
    elif [[ "$OS" == "Darwin" ]]; then
        echo "Install with: brew install ffmpeg"
    fi
    exit 1
fi

echo "✓ SDL2 found:   $(pkg-config --modversion sdl2)"
echo "✓ FFmpeg found: $(pkg-config --modversion libavcodec)"

# ── Create build directories ─────────────────────
mkdir -p build/obj

# ── Build ────────────────────────────────────────
echo ""
echo "Building ..."

if [[ "$OS" == "Darwin" ]]; then
    OS_TARGET="macos"
else
    OS_TARGET="linux"
fi

gprbuild -P blackvideo_player.gpr \
    -XOS_TARGET="$OS_TARGET" \
    -j0 \
    --RTS=native

echo ""
echo "✓ Build complete: build/blackvideo-player"
echo ""
echo "Run with:"
echo "  ./build/blackvideo-player <your-video.mp4>"
