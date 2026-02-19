# BlackVideo Mini Player

A lightweight, cross-platform support video player built with **Ada + SDL2 + FFmpeg**.  
Designed as a companion executable for the BlackVideo Tauri v2 app.

---

## Architecture

```
blackvideo-mini-player/
│
├── src/
│   ├── main.adb              ← Entry point, CLI argument parsing
│   ├── player.ads / .adb     ← Core orchestrator (event loop, state machine)
│   ├── video_decoder.ads/.adb← FFmpeg decode: open, read, decode, YUV→RGB
│   ├── renderer.ads / .adb   ← SDL2 texture upload, aspect-ratio draw
│   ├── audio.ads / .adb      ← SDL2 audio callback, volume, mute
│   └── utils.ads / .adb      ← Helpers: base_name, format_time
│
├── bindings/
│   ├── ffmpeg/
│   │   ├── ffmpeg-avformat.ads   ← libavformat (open, read, seek, close)
│   │   ├── ffmpeg-avcodec.ads    ← libavcodec (decode, packet, context)
│   │   ├── ffmpeg-avutil.ads     ← libavutil (AVFrame, pixel formats)
│   │   ├── ffmpeg-swscale.ads    ← libswscale (YUV → RGB conversion)
│   │   └── ffmpeg-swresample.ads ← libswresample (audio resampling)
│   └── sdl2/
│       ├── sdl.ads               ← SDL_Init, SDL_Quit, SDL_Delay
│       ├── sdl-video.ads/.adb    ← Windows, Renderers, Textures
│       ├── sdl-events.ads/.adb   ← SDL_PollEvent, keyboard, window events
│       └── sdl-audio.ads         ← SDL_OpenAudioDevice, audio callback
│
├── scripts/
│   ├── build.sh              ← Linux / macOS build script
│   └── build.bat             ← Windows build script
│
├── lib/                      ← Put Windows DLLs here
├── build/                    ← Output: blackvideo-player[.exe]
├── alire.toml
└── blackvideo_player.gpr     ← GPR project (cross-platform)
```

---

## Keyboard Controls

| Key | Action |
|-----|--------|
| `SPACE` | Play / Pause |
| `LEFT` | Seek −5 seconds |
| `RIGHT` | Seek +5 seconds |
| `UP` | Volume +10% |
| `DOWN` | Volume −10% |
| `M` | Mute / Unmute |
| `F` | Toggle Fullscreen |
| `ESC` / `Q` | Quit |

---

## Building

### Prerequisites

#### Linux (Ubuntu / Debian)
```bash
sudo apt install gnat gprbuild \
    libsdl2-dev \
    libavcodec-dev libavformat-dev libavutil-dev \
    libswscale-dev libswresample-dev
```

#### macOS (Homebrew)
```bash
brew install gnat gprbuild sdl2 ffmpeg
```

#### Windows (MSYS2 + GNAT)
1. Install [Alire](https://alire.ada.dev/) for GNAT + gprbuild
2. Install [SDL2](https://github.com/libsdl-org/SDL/releases) development libraries
3. Install [FFmpeg shared builds](https://github.com/BtbN/FFmpeg-Builds/releases)
4. Place DLLs in `lib/`

---

### Build Commands

**Linux / macOS:**
```bash
chmod +x scripts/build.sh
./scripts/build.sh
```

**Windows:**
```batch
scripts\build.bat
```

**Manual (any OS):**
```bash
mkdir -p build/obj
gprbuild -P blackvideo_player.gpr -XOS_TARGET=linux -j0
# Replace linux with: windows | macos
```

---

## Running

```bash
# Linux / macOS
./build/blackvideo-player /path/to/video.mp4

# Windows
build\blackvideo-player.exe C:\Videos\movie.mp4

# Show help
./build/blackvideo-player --help
```

**Supported formats:** MP4, MKV, AVI, MOV, WebM, and anything FFmpeg can open.

---

## Tauri v2 Integration

### Step 1: Copy the binary

```
your-tauri-project/
 └── src-tauri/
      └── binaries/
           ├── blackvideo-player-x86_64-pc-windows-msvc.exe   ← Windows
           ├── blackvideo-player-x86_64-unknown-linux-gnu      ← Linux
           └── blackvideo-player-x86_64-apple-darwin           ← macOS
```

> Tauri v2 requires the target triple suffix on sidecar binaries.

### Step 2: Register in tauri.conf.json
```json
{
  "bundle": {
    "externalBin": [
      "binaries/blackvideo-player"
    ],
    "resources": [
      "binaries/SDL2.dll",
      "binaries/avcodec-61.dll",
      "binaries/avformat-61.dll",
      "binaries/avutil-59.dll",
      "binaries/swscale-8.dll"
    ]
  }
}
```

### Step 3: Launch from Rust
```rust
use tauri::api::process::Command;

#[tauri::command]
fn open_video(path: String) {
    Command::new_sidecar("blackvideo-player")
        .expect("Failed to find sidecar")
        .args([path])
        .spawn()
        .expect("Failed to launch player");
}
```

---

## Design Notes

### How YUV → RGB conversion works

```
FFmpeg decodes video → YUV420P frames (raw from codec)
        ↓
libswscale sws_scale() → converts to RGB24
        ↓
SDL2 streaming texture → GPU upload
        ↓
SDL2 RenderCopy with aspect-ratio rect → screen
```

### Audio pipeline

```
FFmpeg av_read_frame (audio packets)
        ↓
avcodec_receive_frame (PCM data)
        ↓
swresample → S16 stereo 44100 Hz
        ↓
SDL2 audio callback fills the device buffer
        ↓
OS audio driver → speakers
```

### Aspect ratio (letterbox / pillarbox)

The renderer always fits the video inside the window using:
```
scale = min(window_w / video_w, window_h / video_h)
dest_w = video_w * scale
dest_h = video_h * scale
dest_x = (window_w - dest_w) / 2
dest_y = (window_h - dest_h) / 2
```
Black bars fill the unused area automatically.

---

## Windows DLL List

Place these beside `blackvideo-player.exe`:

| DLL | From |
|-----|------|
| `SDL2.dll` | SDL2 release page |
| `avcodec-61.dll` | FFmpeg shared build |
| `avformat-61.dll` | FFmpeg shared build |
| `avutil-59.dll` | FFmpeg shared build |
| `swscale-8.dll` | FFmpeg shared build |
| `swresample-5.dll` | FFmpeg shared build |

Version numbers may differ; always match your FFmpeg build version.

---

## License

MIT — free to use.
