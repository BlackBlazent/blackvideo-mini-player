#!/usr/bin/env bash
# build.sh — BlackVideo Mini Player v2.4: Linux / macOS
set -e

echo "=========================================="
echo " BlackVideo Mini Player v2.4 - Build"
echo "=========================================="
echo

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ -f "$SCRIPT_DIR/../blackvideo_player.gpr" ]]; then
    cd "$SCRIPT_DIR/.."
elif [[ -f "blackvideo_player.gpr" ]]; then
    :
else
    echo "ERROR: Cannot find blackvideo_player.gpr"
    exit 1
fi

ROOT="$(pwd)"
echo "[INFO] Working directory: $ROOT"
echo

if [[ -f "$ROOT/.env" ]]; then
    echo "[INFO] .env found - development mode"
    set -a; source "$ROOT/.env" 2>/dev/null || true; set +a
else
    echo "[INFO] No .env - production mode"
fi
echo

# ── Thumbnail preview uses WIC (Windows) / SDL_image (Linux/macOS) ──────────
echo "Thumbnail preview: WIC on Windows / SDL_image on Linux/macOS — no stb_image.h needed"
echo

# ── Delete stale .adb files ─────────────────────────────────────────────────
STALE=0
for F in "bindings/ffmpeg/ffmpeg-avcodec.adb" "bindings/ffmpeg/ffmpeg-avformat.adb" "bindings/sdl2/sdl-video.adb"; do
    if [[ -f "$ROOT/$F" ]]; then rm -f "$ROOT/$F"; echo "[FIX] Deleted stale $F"; STALE=1; fi
done
[[ "$STALE" == "1" ]] && echo

# ── Find GNAT tools ─────────────────────────────────────────────────────────
GPRBUILD=""
if command -v gprbuild &>/dev/null; then GPRBUILD="$(command -v gprbuild)"; fi
if [[ -z "$GPRBUILD" ]]; then
    for D in /usr/bin /usr/local/bin "$HOME/.local/bin" /opt/gnat/bin; do
        if [[ -x "$D/gprbuild" ]]; then GPRBUILD="$D/gprbuild"; break; fi
    done
fi
if [[ -z "$GPRBUILD" ]]; then
    ALIRE_TC="${XDG_DATA_HOME:-$HOME/.local/share}/alire/toolchains"
    for D in "$ALIRE_TC"/gnat_native_*/bin; do
        if [[ -x "$D/gprbuild" ]]; then GPRBUILD="$D/gprbuild"; break; fi
    done
fi
if [[ -z "$GPRBUILD" ]]; then
    echo "ERROR: gprbuild not found. Install via: alr toolchain --select"
    exit 1
fi
GCC="$(dirname "$GPRBUILD")/gcc"
if [[ ! -x "$GCC" ]]; then
    command -v gcc &>/dev/null && GCC="$(command -v gcc)" || { echo "ERROR: gcc not found"; exit 1; }
fi
echo "[OK] gprbuild: $GPRBUILD"
echo "[OK] gcc:      $GCC"
echo

# ── Check FFmpeg headers ─────────────────────────────────────────────────────
echo "Checking FFmpeg headers ..."
HDR_OK=1
for H in "libavformat/avformat.h" "libavcodec/avcodec.h" "libavutil/avutil.h"; do
    [[ ! -f "$ROOT/lib/include/$H" ]] && echo "  MISSING: lib/include/$H" && HDR_OK=0
done
if [[ "$HDR_OK" == "0" ]]; then
    pkg-config --exists libavcodec libavformat libavutil libswscale libswresample 2>/dev/null && {
        echo "  [OK] FFmpeg via pkg-config"
        USE_SYSTEM_FFMPEG=1
        HDR_OK=1
    } || {
        echo "ERROR: FFmpeg headers missing."
        echo "  Linux: sudo apt install libavcodec-dev libavformat-dev libavutil-dev libswscale-dev libswresample-dev"
        echo "  macOS: brew install ffmpeg"
        exit 1
    }
else
    echo "  [OK] FFmpeg headers in lib/include/"
fi
echo

# ── Check SDL2 / SDL2_ttf headers ───────────────────────────────────────────
echo "Checking SDL2 headers ..."
SDL_I1=""; SDL_I2=""
if [[ -f "$ROOT/lib/include/SDL2/SDL.h" ]]; then
    echo "  [OK] SDL.h -- lib/include/SDL2/SDL.h"
    SDL_I1="-I\"$ROOT/lib/include\""; SDL_I2="-I\"$ROOT/lib/include/SDL2\""
elif [[ -f "$ROOT/lib/include/SDL.h" ]]; then
    echo "  [OK] SDL.h -- lib/include/SDL.h"
    SDL_I1="-I\"$ROOT/lib/include\""
elif pkg-config --exists sdl2 2>/dev/null; then
    echo "  [OK] SDL.h -- system (pkg-config)"
    SDL_I1="$(pkg-config --cflags sdl2 SDL2_ttf 2>/dev/null || pkg-config --cflags sdl2)"
    USE_SYSTEM_SDL=1
else
    echo "  ERROR: SDL.h not found. sudo apt install libsdl2-dev / brew install sdl2"
    exit 1
fi
echo

# ── Check import libraries ───────────────────────────────────────────────────
echo "Checking import libraries ..."
MISSING=0
if [[ "${USE_SYSTEM_FFMPEG:-0}" == "0" ]]; then
    for N in avcodec avformat avutil swscale swresample; do
        if [[ ! -f "$ROOT/lib/lib${N}.a" ]]; then
            if [[ -f "$ROOT/lib/lib${N}.dll.a" ]]; then
                cp "$ROOT/lib/lib${N}.dll.a" "$ROOT/lib/lib${N}.a"
                echo "  [FIX] lib${N}.dll.a -> lib${N}.a"
            else
                echo "  MISSING: lib/lib${N}.a"; MISSING=1
            fi
        fi
    done
fi
if [[ "${USE_SYSTEM_SDL:-0}" == "0" ]]; then
    [[ ! -f "$ROOT/lib/libSDL2.a"     ]] && echo "  MISSING: lib/libSDL2.a"     && MISSING=1
    [[ ! -f "$ROOT/lib/libSDL2main.a" ]] && echo "  MISSING: lib/libSDL2main.a" && MISSING=1
fi
if [[ "$MISSING" == "1" ]]; then
    echo "ERROR: Copy missing .a files into lib/ or install system dev packages."
    exit 1
fi
echo "  [OK] Import libraries ready."
echo

# ── Whisper asset check ──────────────────────────────────────────────────────
echo "Checking Whisper assets (optional) ..."
if [[ -n "${BLACKVIDEO_WHISPER_PATH:-}" && -x "$BLACKVIDEO_WHISPER_PATH" ]]; then
    echo "  [OK] whisper-cli via BLACKVIDEO_WHISPER_PATH"
elif [[ -x "$ROOT/build/whisper-cli" ]]; then
    echo "  [OK] whisper-cli in build/"
elif command -v whisper-cli &>/dev/null; then
    echo "  [OK] whisper-cli on PATH"
else
    echo "  [INFO] whisper-cli not found"
fi
if [[ -f "$ROOT/build/models/ggml-base.bin" ]]; then
    echo "  [OK] ggml-base.bin in build/models/"
elif [[ -f "$ROOT/build/ggml-base.bin" ]]; then
    echo "  [OK] ggml-base.bin in build/"
else
    echo "  [INFO] ggml-base.bin not found"
fi
echo

# ── Create build dirs ────────────────────────────────────────────────────────
mkdir -p "$ROOT/build/obj" "$ROOT/build/cache"

# Remove stale objects
for F in ui_overlay_c.o process_helpers.o thumb_preview_c.o llm_client_c.o; do
    if [[ -f "$ROOT/build/obj/$F" ]]; then
        rm -f "$ROOT/build/obj/$F"
        echo "[FIX] Deleted stale build/obj/$F"
    fi
done

# ── STEP 1: Compile src/ffmpeg_helpers.c ─────────────────────────────────────
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

# ── STEP 2: Compile csrc/ui_overlay.c ────────────────────────────────────────
echo "Compiling csrc/ui_overlay.c ..."
eval "$GCC" -O2 -c "\"$ROOT/csrc/ui_overlay.c\"" \
    $SDL_I1 $SDL_I2 \
    -I"\"$ROOT/include\"" \
    -o "\"$ROOT/build/obj/ui_overlay_c.o\""
echo "  [OK] build/obj/ui_overlay_c.o"
echo

# ── STEP 3: Compile csrc/process_helpers.c ───────────────────────────────────
echo "Compiling csrc/process_helpers.c ..."
eval "$GCC" -O2 -c "\"$ROOT/csrc/process_helpers.c\"" \
    -o "\"$ROOT/build/obj/process_helpers.o\""
echo "  [OK] build/obj/process_helpers.o"
echo

# ── STEP 4: Compile csrc/thumb_preview.c ─────────────────────────────────────
echo "Compiling csrc/thumb_preview.c ..."
eval "$GCC" -O2 -c "\"$ROOT/csrc/thumb_preview.c\"" \
    $SDL_I1 $SDL_I2 \
    -o "\"$ROOT/build/obj/thumb_preview_c.o\""
echo "  [OK] build/obj/thumb_preview_c.o"
echo

# ── STEP 5: Compile csrc/llm_client.c ────────────────────────────────────────
echo "Compiling csrc/llm_client.c ..."
eval "$GCC" -O2 -c "\"$ROOT/csrc/llm_client.c\"" \
    -o "\"$ROOT/build/obj/llm_client_c.o\""
echo "  [OK] build/obj/llm_client_c.o"
echo

# ── STEP 6: Build Ada + link ──────────────────────────────────────────────────
OS_UNAME="$(uname -s)"
if [[ "$OS_UNAME" == "Darwin" ]]; then OS_TARGET="macos"; else OS_TARGET="linux"; fi

echo "Building Ada sources and linking (OS_TARGET=$OS_TARGET) ..."
echo
"$GPRBUILD" -P blackvideo_player.gpr -XOS_TARGET="$OS_TARGET" -j0

echo
echo "=========================================="
echo " BUILD SUCCESS: build/blackvideo-player"
echo "=========================================="
echo

# ── Create .env.example if not exists ────────────────────────────────────────
if [[ ! -f "$ROOT/.env" && ! -f "$ROOT/.env.example" ]]; then
    cat > "$ROOT/.env.example" << 'ENV'
# BlackVideo Mini Player v2.4 - Development overrides
# Rename to .env for dev mode. Delete for production.
#
# BLACKVIDEO_WHISPER_PATH=/usr/local/bin/whisper-cli
# BLACKVIDEO_WHISPER_MODEL=/path/to/models/ggml-base.bin
ENV
    echo "Created .env.example"
    echo
fi

echo "── Setup reminders ─────────────────────────────────────────────────────"
echo "  Whisper: whisper-cli on PATH or in build/; ggml-base.bin in build/models/"
echo "  LLM keys: stored at runtime in ~/.config/BlackVideo/keys.cfg"
echo "  Thumbnail preview: requires ffmpeg on PATH; cached in build/cache/"
echo "  Auto-updater: create latest.json in repo root (see docs)"
echo "────────────────────────────────────────────────────────────────────────"
echo
echo "Run:  ./build/blackvideo-player"
echo " or:  ./build/blackvideo-player \"/path/to/video.mp4\""
echo
