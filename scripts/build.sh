#!/usr/bin/env bash
# build.sh — BlackVideo Mini Player v2.3: Linux / macOS
# Mirrors the structure and checks of scripts/build.bat exactly.
set -e

echo "=========================================="
echo " BlackVideo Mini Player v2.3 - Build"
echo "=========================================="
echo

# ── Navigate to project root ────────────────────────────────────────────────
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ -f "$SCRIPT_DIR/../blackvideo_player.gpr" ]]; then
    cd "$SCRIPT_DIR/.."
elif [[ -f "blackvideo_player.gpr" ]]; then
    : # already in root
else
    echo "ERROR: Cannot find blackvideo_player.gpr"
    exit 1
fi

ROOT="$(pwd)"
echo "[INFO] Working directory: $ROOT"
echo

# ── Production vs Dev mode ──────────────────────────────────────────────────
# In production: no .env file needed. Whisper resolves from build/ at runtime.
# In dev:        create .env with BLACKVIDEO_WHISPER_PATH and
#                BLACKVIDEO_WHISPER_MODEL to override search paths.
if [[ -f "$ROOT/.env" ]]; then
    echo "[INFO] .env file found - development mode"
    echo "[INFO] Env vars will be loaded at runtime from .env"
    # Source .env so whisper path checks below see the vars
    set -a
    # shellcheck disable=SC1090
    source "$ROOT/.env" 2>/dev/null || true
    set +a
else
    echo "[INFO] No .env file - production mode"
    echo "[INFO] Whisper resolved from build/ at runtime"
fi
echo

# ── Delete stale .adb files ─────────────────────────────────────────────────
STALE=0
for F in \
    "bindings/ffmpeg/ffmpeg-avcodec.adb" \
    "bindings/ffmpeg/ffmpeg-avformat.adb" \
    "bindings/sdl2/sdl-video.adb"
do
    if [[ -f "$ROOT/$F" ]]; then
        rm -f "$ROOT/$F"
        echo "[FIX] Deleted stale $F"
        STALE=1
    fi
done
[[ "$STALE" == "1" ]] && echo

# ── Find GNAT tools ─────────────────────────────────────────────────────────
GPRBUILD=""

# 1. PATH
if command -v gprbuild &>/dev/null; then
    GPRBUILD="$(command -v gprbuild)"
fi

# 2. Common fixed install paths
if [[ -z "$GPRBUILD" ]]; then
    for D in \
        /usr/bin \
        /usr/local/bin \
        "$HOME/.local/bin" \
        /opt/gnat/bin \
        /opt/alire/bin
    do
        if [[ -x "$D/gprbuild" ]]; then
            GPRBUILD="$D/gprbuild"
            break
        fi
    done
fi

# 3. Alire toolchains (Linux/macOS)
if [[ -z "$GPRBUILD" ]]; then
    ALIRE_TC="${XDG_DATA_HOME:-$HOME/.local/share}/alire/toolchains"
    for D in "$ALIRE_TC"/gnat_native_*/bin; do
        if [[ -x "$D/gprbuild" ]]; then
            GPRBUILD="$D/gprbuild"
            break
        fi
    done
fi

if [[ -z "$GPRBUILD" ]]; then
    echo "ERROR: gprbuild not found."
    echo "  Install via Alire: alr toolchain --select"
    echo "  Or: sudo apt install gprbuild  /  brew install gprbuild"
    exit 1
fi

GCC="$(dirname "$GPRBUILD")/gcc"
if [[ ! -x "$GCC" ]]; then
    # Fall back to system gcc
    if command -v gcc &>/dev/null; then
        GCC="$(command -v gcc)"
    else
        echo "ERROR: gcc not found alongside gprbuild at $(dirname "$GPRBUILD")"
        exit 1
    fi
fi

echo "[OK] gprbuild: $GPRBUILD"
echo "[OK] gcc:      $GCC"
echo

# ── Check FFmpeg headers ─────────────────────────────────────────────────────
echo "Checking FFmpeg headers ..."
HDR_OK=1
for H in \
    "libavformat/avformat.h" \
    "libavcodec/avcodec.h" \
    "libavutil/avutil.h"
do
    if [[ ! -f "$ROOT/lib/include/$H" ]]; then
        echo "  MISSING: lib/include/$H"
        HDR_OK=0
    fi
done

# If not in lib/include, check system pkg-config paths (Linux/macOS standard)
if [[ "$HDR_OK" == "0" ]]; then
    if pkg-config --exists libavcodec libavformat libavutil libswscale libswresample 2>/dev/null; then
        echo "  [OK] FFmpeg headers found via pkg-config (system install)"
        HDR_OK=1
        USE_SYSTEM_FFMPEG=1
    else
        echo
        echo "ERROR: FFmpeg headers missing."
        echo "  Linux:  sudo apt install libavcodec-dev libavformat-dev libavutil-dev libswscale-dev libswresample-dev"
        echo "  macOS:  brew install ffmpeg"
        echo "  Or copy headers to lib/include/"
        exit 1
    fi
else
    echo "  [OK] FFmpeg headers present in lib/include/"
fi
echo

# ── Check SDL2 / SDL2_ttf headers ───────────────────────────────────────────
echo "Checking SDL2 headers ..."
SDL_I1=""
SDL_I2=""

if [[ -f "$ROOT/lib/include/SDL2/SDL.h" ]]; then
    echo "  [OK] SDL.h  --  lib/include/SDL2/SDL.h"
    SDL_I1="-I\"$ROOT/lib/include\""
    SDL_I2="-I\"$ROOT/lib/include/SDL2\""
elif [[ -f "$ROOT/lib/include/SDL.h" ]]; then
    echo "  [OK] SDL.h  --  lib/include/SDL.h"
    SDL_I1="-I\"$ROOT/lib/include\""
elif pkg-config --exists sdl2 2>/dev/null; then
    echo "  [OK] SDL.h  --  system (pkg-config)"
    SDL_I1="$(pkg-config --cflags sdl2)"
    SDL_I2=""
    USE_SYSTEM_SDL=1
else
    echo "  ERROR: SDL.h not found."
    echo "  Linux:  sudo apt install libsdl2-dev"
    echo "  macOS:  brew install sdl2"
    exit 1
fi

if [[ -f "$ROOT/lib/include/SDL2/SDL_ttf.h" ]]; then
    echo "  [OK] SDL_ttf.h  --  lib/include/SDL2/SDL_ttf.h"
elif [[ -f "$ROOT/lib/include/SDL_ttf.h" ]]; then
    echo "  [OK] SDL_ttf.h  --  lib/include/SDL_ttf.h"
elif pkg-config --exists SDL2_ttf 2>/dev/null; then
    echo "  [OK] SDL_ttf.h  --  system (pkg-config)"
    SDL_I1="$SDL_I1 $(pkg-config --cflags SDL2_ttf)"
else
    echo "  WARNING: SDL_ttf.h not found - UI overlay may not compile"
fi
echo

# ── Check import libraries ───────────────────────────────────────────────────
echo "Checking lib/ import libraries ..."
MISSING=0

# On Linux/macOS with system install, .a files live in system prefix —
# gprbuild picks them up via pkg-config linker flags in the GPR file.
# We only check lib/ if the user is using a local lib/ layout.
if [[ "${USE_SYSTEM_FFMPEG:-0}" == "0" ]]; then
    for N in avcodec avformat avutil swscale swresample; do
        if [[ ! -f "$ROOT/lib/lib${N}.a" ]]; then
            if [[ -f "$ROOT/lib/lib${N}.dll.a" ]]; then
                cp "$ROOT/lib/lib${N}.dll.a" "$ROOT/lib/lib${N}.a"
                echo "  [FIX] lib${N}.dll.a -> lib${N}.a"
            else
                echo "  MISSING: lib/lib${N}.a"
                MISSING=1
            fi
        fi
    done
fi

if [[ "${USE_SYSTEM_SDL:-0}" == "0" ]]; then
    [[ ! -f "$ROOT/lib/libSDL2.a"     ]] && echo "  MISSING: lib/libSDL2.a"     && MISSING=1
    [[ ! -f "$ROOT/lib/libSDL2main.a" ]] && echo "  MISSING: lib/libSDL2main.a" && MISSING=1
fi

if [[ "$MISSING" == "1" ]]; then
    echo
    echo "ERROR: Copy missing .a files into lib/"
    echo "  Or install system dev packages and re-run."
    exit 1
fi
echo "  [OK] Import libraries ready."
echo

# ── Optional: check Whisper assets (non-fatal) ──────────────────────────────
echo "Checking Whisper assets (optional) ..."

WHISPER_EXE=""
# Check env var first (sourced from .env above)
if [[ -n "${BLACKVIDEO_WHISPER_PATH:-}" && -x "$BLACKVIDEO_WHISPER_PATH" ]]; then
    WHISPER_EXE="$BLACKVIDEO_WHISPER_PATH"
    echo "  [OK] whisper-cli found via BLACKVIDEO_WHISPER_PATH"
elif [[ -x "$ROOT/build/whisper-cli" ]]; then
    WHISPER_EXE="$ROOT/build/whisper-cli"
    echo "  [OK] whisper-cli found in build/"
elif command -v whisper-cli &>/dev/null; then
    WHISPER_EXE="$(command -v whisper-cli)"
    echo "  [OK] whisper-cli found on PATH"
else
    echo "  [INFO] whisper-cli not found - set BLACKVIDEO_WHISPER_PATH in .env or place beside exe"
fi

MODEL_PATH=""
if [[ -n "${BLACKVIDEO_WHISPER_MODEL:-}" && -f "$BLACKVIDEO_WHISPER_MODEL" ]]; then
    MODEL_PATH="$BLACKVIDEO_WHISPER_MODEL"
    echo "  [OK] model found via BLACKVIDEO_WHISPER_MODEL"
elif [[ -f "$ROOT/build/models/ggml-base.bin" ]]; then
    MODEL_PATH="$ROOT/build/models/ggml-base.bin"
    echo "  [OK] ggml-base.bin found in build/models/"
elif [[ -f "$ROOT/build/ggml-base.bin" ]]; then
    MODEL_PATH="$ROOT/build/ggml-base.bin"
    echo "  [OK] ggml-base.bin found in build/"
else
    echo "  [INFO] ggml-base.bin not found - set BLACKVIDEO_WHISPER_MODEL in .env or place in build/models/"
fi
echo

# ── Create build dirs ────────────────────────────────────────────────────────
mkdir -p "$ROOT/build/obj"

# Remove stale ui_overlay.o
if [[ -f "$ROOT/build/obj/ui_overlay.o" ]]; then
    rm -f "$ROOT/build/obj/ui_overlay.o"
    echo "[FIX] Deleted stale build/obj/ui_overlay.o"
fi

# ── STEP 1: Compile src/ffmpeg_helpers.c ────────────────────────────────────
echo "Compiling src/ffmpeg_helpers.c ..."

if [[ "${USE_SYSTEM_FFMPEG:-0}" == "1" ]]; then
    FFMPEG_CFLAGS="$(pkg-config --cflags libavformat libavcodec libavutil)"
else
    FFMPEG_CFLAGS="-I\"$ROOT/lib/include\""
fi

eval "$GCC" -O2 -c "\"$ROOT/src/ffmpeg_helpers.c\"" \
    $FFMPEG_CFLAGS \
    -o "\"$ROOT/build/obj/ffmpeg_helpers.o\""

echo "  [OK] build/obj/ffmpeg_helpers.o"
echo

# ── STEP 2: Compile csrc/ui_overlay.c ───────────────────────────────────────
echo "Compiling csrc/ui_overlay.c ..."

eval "$GCC" -O2 -c "\"$ROOT/csrc/ui_overlay.c\"" \
    $SDL_I1 $SDL_I2 \
    -o "\"$ROOT/build/obj/ui_overlay_c.o\""

if [[ ! -f "$ROOT/build/obj/ui_overlay_c.o" ]]; then
    echo
    echo "FAILED: ui_overlay_c.o not created"
    exit 1
fi
echo "  [OK] build/obj/ui_overlay_c.o"
echo

# ── STEP 3: Build Ada + link ─────────────────────────────────────────────────
OS_UNAME="$(uname -s)"
if [[ "$OS_UNAME" == "Darwin" ]]; then
    OS_TARGET="macos"
else
    OS_TARGET="linux"
fi

echo "Building Ada sources and linking ..."
echo

"$GPRBUILD" -P blackvideo_player.gpr -XOS_TARGET="$OS_TARGET" -j0

echo
echo "=========================================="
echo " BUILD SUCCESS: build/blackvideo-player"
echo "=========================================="
echo

# ── Copy/verify runtime shared libraries ────────────────────────────────────
echo "Checking runtime shared libraries ..."

if [[ "$OS_TARGET" == "linux" ]]; then
    # On Linux, .so files are system-wide — just verify they're accessible
    for LIB in libSDL2-2.0.so.0 libSDL2_ttf-2.0.so.0 libavcodec libavformat; do
        if ldconfig -p 2>/dev/null | grep -q "$LIB"; then
            echo "  [OK] $LIB (system)"
        fi
    done
elif [[ "$OS_TARGET" == "macos" ]]; then
    echo "  [OK] Runtime dylibs linked via Homebrew (system)"
fi

# If lib/ has local .so/.dylib files, copy to build/
for EXT in so dylib; do
    for F in "$ROOT/lib/"*."$EXT"*; do
        [[ -f "$F" ]] && cp -n "$F" "$ROOT/build/" && echo "  Copied $(basename "$F")"
    done
done
echo

# ── Create .env template if none exists ─────────────────────────────────────
if [[ ! -f "$ROOT/.env" && ! -f "$ROOT/.env.example" ]]; then
    echo "Creating .env.example (rename to .env for dev overrides) ..."
    cat > "$ROOT/.env.example" << 'ENV'
# BlackVideo Mini Player - Development overrides
# This file is loaded at runtime in dev mode only.
# In a release build, delete this file - paths resolve automatically.
#
# Uncomment and set these if whisper-cli is not beside the player exe:
# BLACKVIDEO_WHISPER_PATH=/usr/local/bin/whisper-cli
# BLACKVIDEO_WHISPER_MODEL=/path/to/models/ggml-base.bin
ENV
    echo "  Created .env.example"
    echo
fi

# ── Whisper setup reminder ────────────────────────────────────────────────────
echo "── Whisper setup reminder ───────────────────────────────────────────────"
echo "  For offline captions, ensure these are available:"
echo "    whisper-cli  (on PATH or in build/)"
echo "    ggml-base.bin  (in build/models/ or set BLACKVIDEO_WHISPER_MODEL)"
echo "    ffmpeg  (on PATH or in build/)"
echo "─────────────────────────────────────────────────────────────────────────"
echo
echo "Run:  ./build/blackvideo-player \"/path/to/video.mp4\""
echo