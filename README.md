![Visitors](https://api.visitorbadge.io/api/visitors?path=https%3A%2F%2Fgithub.com%2FBlackBlazent%2Fblackvideo-mini-player&label=BlackVideo%20Mini%20Player%3AVisitor&labelColor=%23000000&countColor=%2337d67a&style=flat&labelStyle=lower)

> # 2.2.0

New update **2.2.0** is here as a stable release. This includes fully working playback controls, a context menu, audio stability fixes for long videos, and a corrected SDL2 event struct layout.
<br/>

- **Download** it [here](https://sourceforge.net/projects/blackvideo-mini-player/files/blackvideo-mini-player-v2.2.0.zip/download) on SourceForge or in the [release](https://github.com/BlackBlazent/blackvideo-mini-player/releases/tag/v2.2.0) page.
- [**Change logs**](https://github.com/BlackBlazent/blackvideo-mini-player/compare/1.1.0...v2.2.0)

<img src="./public/screenshot.png" alt="Screenshot"/>

# BlackVideo Mini Player

Lightweight cross-platform video player (Ada + SDL2 + FFmpeg). Support player for the [BlackVideo](https://github.com/BlackBlazent/BlackVideo). Works standalone via **CLI** or **right-click** on any video file.

<p align="center">
<p align="center">
  <img src="https://img.shields.io/badge/version-2.2.0-37d67a?style=for-the-badge" />
  <img src="https://img.shields.io/badge/status-in%20active%20development-orange?style=for-the-badge" />
  <img src="https://img.shields.io/badge/license-MIT-black?style=for-the-badge" />
</p>
<p align="center">
  <img src="https://img.shields.io/badge/Ada-Language-0A84FF?style=for-the-badge&logo=ada&logoColor=white" />
  <img src="https://img.shields.io/badge/SDL2-Rendering-7B42BC?style=for-the-badge" />
  <img src="https://img.shields.io/badge/FFmpeg-Decoding-D90429?style=for-the-badge" />
  <img src="https://img.shields.io/badge/Alire-Package-009688?style=for-the-badge" />
  <img src="https://img.shields.io/badge/GNAT-Compiler-FFB703?style=for-the-badge" />
  <img src="https://img.shields.io/badge/gprbuild-Build%20System-6c757d?style=for-the-badge" />
</p>
<p align="center">
  <img src="https://img.shields.io/badge/BlackVideo-Utilities-000000?style=for-the-badge" />
</p>
</p>

---

<img src="./public/new_screenshot.png" alt="Screenshot"/>

## File Structure

```
blackvideo-mini-player/
│
├── src/
│   ├── main.adb                  ← Entry point
│   ├── player.ads / .adb         ← Core orchestrator + event loop
│   ├── video_decoder.ads / .adb  ← FFmpeg decode pipeline
│   ├── renderer.ads / .adb       ← SDL2 texture rendering (letterbox)
│   ├── audio.ads / .adb          ← SDL2 audio callback + volume/mute/flush
│   ├── ui_overlay.ads / .adb     ← Ada wrapper for the C UI overlay
│   └── utils.ads / .adb          ← Base_Name, Format_Time
│
├── csrc/                         ← C sources compiled separately (not scanned by gprbuild)
│   └── ui_overlay.c              ← SDL2-drawn control bar (buttons, seek bar, context menu)
│
├── bindings/
│   ├── ffmpeg/
│   │   ├── ffmpeg-avutil.ads       ← libavutil (AVFrame, pixel formats)
│   │   ├── ffmpeg-avcodec.ads      ← libavcodec (decode, context, packet)
│   │   ├── ffmpeg-avformat.ads     ← libavformat (container open/read/seek)
│   │   ├── ffmpeg-swscale.ads      ← libswscale (YUV→RGB)
│   │   └── ffmpeg-swresample.ads   ← libswresample (audio resample)
│   └── sdl2/
│       ├── sdl.ads                      ← SDL_Init / Quit / Delay
│       ├── sdl-video.ads                ← SDL.Video shared types (handles, SDL_Rect)
│       ├── sdl-video-windows.ads/adb    ← SDL.Video.Windows (child package)
│       ├── sdl-video-renderers.ads/adb  ← SDL.Video.Renderers
│       ├── sdl-video-textures.ads/adb   ← SDL.Video.Textures
│       ├── sdl-events.ads/adb           ← SDL.Events (flat byte extractor)
│       ├── sdl-events-keyboards.ads     ← SDL.Events.Keyboards (keycodes)
│       └── sdl-audio.ads                ← SDL.Audio (callback, device, lock/unlock)
│
├── tools/
│   └── context_menu/
│       ├── install_context_menu.bat    ← Windows: installs right-click (run as admin)
│       ├── install_context_menu.reg    ← Windows: manual .reg import
│       ├── uninstall_context_menu.bat  ← Windows: remove right-click
│       ├── uninstall_context_menu.reg
│       ├── install_linux.sh            ← Linux: installs binary + .desktop
│       ├── blackvideo-player.desktop   ← Linux: MIME association file
│       └── install_macos.sh            ← macOS: creates Quick Action service
│
├── lib/
│   ├── include/      ← FFmpeg headers and SDL2 headers go here
│   └── *.a           ← Import library (.a / .dll.a) files go here
│
├── build/            ← Compiled output and runtime DLLs go here
│
├── scripts/
│   ├── build.bat     ← Windows build (handles spaces in path, auto-finds gprbuild)
│   └── build.sh      ← Linux/macOS build
│
└── blackvideo_player.gpr   ← GNAT project file
```

---

## Keyboard Controls

| Key | Action |
|-----|--------|
| `SPACE` | Play / Pause |
| `←` | Seek −5 seconds |
| `→` | Seek +5 seconds |
| `↑` | Volume +10 |
| `↓` | Volume −10 |
| `M` | Mute / Unmute |
| `L` | Loop toggle |
| `F` | Fullscreen toggle |
| `ESC` / `Q` | Quit |

## Playback Controls (UI Bar)

| Control | Action |
|---------|--------|
| Seek bar | Click or drag to seek |
| ⏮ Prev | Seek to start |
| ⏯ Play/Pause | Toggle playback |
| ⏭ Next | Jump to near end |
| 🔁 Loop | Toggle loop |
| 🔊 Volume | Mute/unmute |
| `1.0x` Speed | Cycle 0.5× / 1.0× / 1.5× / 2.0× |
| ⛶ Fullscreen | Toggle fullscreen |
| ⋮ Menu | Open context menu |

> Right-click anywhere on the video to open the context menu (Open File, Subtitle tracks).

---

## Building

### Windows

#### Prerequisites
1. Install [Alire](https://alire.ada.dev/) (GNAT + gprbuild)
   ```
   alr toolchain --select
   ```
2. Download [SDL2 development libraries](https://github.com/libsdl-org/SDL/releases) (MinGW version)
3. Download [SDL2_ttf development libraries](https://github.com/libsdl-org/SDL_ttf/releases)
4. Download [FFmpeg shared builds](https://github.com/BtbN/FFmpeg-Builds/releases) (win64-lgpl)
5. Copy headers and `.a` import libraries into `lib/` — see [Required Files](#required-files) below.

> [**Download**](https://www.mediafire.com/file/at0tyazvbyqfc0l/prerequisites-setup.zip/file) this setup: `lib/` and `build/` (Windows) ✨ 

#### Build
```bat
cd blackvideo-mini-player
scripts\build.bat
```

> **If gprbuild is not on PATH:** The script searches common Alire install locations automatically. If it still cannot find it, add the `bin\` folder of your GNAT toolchain to your user PATH under System → Environment Variables. The toolchain folder is usually:
> `%LOCALAPPDATA%\alire\toolchains\gnat_native_X.X.X_...\bin`

#### Run
```bat
build\blackvideo-player.exe "C:\Videos\movie.mp4"
```

---

### Linux

```bash
sudo apt install gnat gprbuild \
    libsdl2-dev libsdl2-ttf-dev \
    libavcodec-dev libavformat-dev libavutil-dev \
    libswscale-dev libswresample-dev

chmod +x scripts/build.sh
./scripts/build.sh

./build/blackvideo-player /path/to/video.mp4
```

### macOS

```bash
brew install gnat gprbuild sdl2 sdl2_ttf ffmpeg
chmod +x scripts/build.sh
./scripts/build.sh

./build/blackvideo-player /path/to/video.mp4
```

---

## Using in GNAT Studio

1. Open `blackvideo_player.gpr` with GNAT Studio (File → Open Project)
2. Make sure bindings are visible: the project shows `src`, `bindings/ffmpeg`, `bindings/sdl2`
3. Build: Build menu → Build All (or press F4)
4. Set the scenario variable `OS_TARGET` to `windows` in the Scenario panel

> **Why each binding is its own file:** Ada's file-naming convention requires that package `SDL.Video.Windows` lives in `sdl-video-windows.ads`. GNAT cannot find child packages inside a parent file — each child unit must have its own `.ads` / `.adb` pair.

---

## Right-Click Integration

### Windows (Run as Administrator)
```
tools\context_menu\install_context_menu.bat
```
This auto-detects `blackvideo-player.exe` and adds "Open with BlackVideo Player" to the right-click menu for: `.mp4 .mkv .avi .mov .wmv .webm .flv .m4v .mpg .mpeg .ts`

To remove:
```
tools\context_menu\uninstall_context_menu.bat
```

### Linux (Nautilus, Dolphin, Thunar, Nemo...)
```bash
chmod +x tools/context_menu/install_linux.sh
./tools/context_menu/install_linux.sh
```
Installs to `/usr/local/bin` and registers a `.desktop` file with MIME types.

### macOS (Finder right-click → Services)
```bash
chmod +x tools/context_menu/install_macos.sh
./tools/context_menu/install_macos.sh
```
Creates an Automator Quick Action. Right-click a video → Services → "Open with BlackVideo Player".

---

## Tauri v2 Integration

```toml
# tauri.conf.json
"bundle": {
  "externalBin": ["binaries/blackvideo-player"],
  "resources": [
    "binaries/SDL2.dll",
    "binaries/SDL2_ttf.dll",
    "binaries/libfreetype-6.dll",
    "binaries/zlib1.dll",
    "binaries/avcodec-62.dll",
    "binaries/avformat-62.dll",
    "binaries/avutil-60.dll",
    "binaries/swscale-9.dll",
    "binaries/swresample-6.dll"
  ]
}
```

```rust
use tauri::api::process::Command;

#[tauri::command]
fn open_video(path: String) {
    Command::new_sidecar("blackvideo-player")
        .expect("sidecar not found")
        .args([path])
        .spawn()
        .expect("failed to launch player");
}
```

---

## Required Files

### DLL Libraries — place beside `blackvideo-player.exe` in `build\`

**`build/`**

| DLL | Source |
|-----|--------|
| `SDL2.dll` | SDL2 release page (MinGW) |
| `SDL2_ttf.dll` | `SDL2_ttf-devel-2.0.12-VC` |
| `libfreetype-6.dll` | Bundled with `SDL2_ttf-devel-2.0.12-VC` |
| `zlib1.dll` | Bundled with `SDL2_ttf-devel-2.0.12-VC` |
| `avcodec-62.dll` | FFmpeg shared build |
| `avdevice-62.dll` | FFmpeg shared build |
| `avfilter-11.dll` | FFmpeg shared build |
| `avformat-62.dll` | FFmpeg shared build |
| `avutil-60.dll` | FFmpeg shared build |
| `swresample-6.dll` | FFmpeg shared build |
| `swscale-9.dll` | FFmpeg shared build |

> **Important:** Use `SDL2_ttf-devel-2.0.12-VC` instead of `SDL2_ttf-devel-2.24.0-mingw`. The 2.0.12 VC build's `SDL2_ttf.dll` requires `libfreetype-6.dll` and `zlib1.dll` — include both alongside `SDL2_ttf.dll` in `build\`.

### FFmpeg Headers — `lib/include/`

**`lib/include/`**
- `libavcodec/`
- `libavdevice/`
- `libavfilter/`
- `libavformat/`
- `libavutil/`
- `libswresample/`
- `libswscale/`
- `SDL2/`

---

### Static Import Libraries — `lib/`

**`lib/`**
- `libavcodec.a`
- `libavdevice.a`
- `libavfilter.a`
- `libavformat.a`
- `libavutil.a`
- `libSDL2.a`
- `libSDL2main.a`
- `libSDL2_ttf.dll.a` ← use the import lib, not the static build
- `libswresample.a`
- `libswscale.a`

---

## Design Notes

**Why flat byte extraction for SDL events?**
Ada's discriminated unions cannot map to C's anonymous unions without `Unchecked_Union`, which makes field access unsafe at compile time. The flat `Raw_Bytes` + offset extractors in `SDL.Events` are simpler, safer, and work identically on all platforms.

**Why is `ui_overlay.c` in `csrc/` and not `src/`?**
GPRbuild scans every file in `src/` and tries to compile it as Ada (since `for Languages use ("Ada")`). Placing the C overlay in `csrc/` keeps it invisible to GPRbuild. `build.bat` compiles it explicitly with `gcc` and passes the resulting `ui_overlay_c.o` directly to the linker — the same approach used for `ffmpeg_helpers.c`.

**Why child packages have separate files?**
GNAT maps Ada package `Foo.Bar.Baz` to file `foo-bar-baz.ads`. You cannot embed child package specs inside a parent file — GNAT will report "file not found" for any `with Foo.Bar.Baz` that does not have its own file.

**Letterbox rendering:**
`Renderer.Fit_Rect` computes `scale = min(win_w/vid_w, win_h/vid_h)` and centres the video with black bars, preserving the original aspect ratio at any window size or fullscreen resolution.

**Audio thread safety:**
`SDL_LockAudioDevice` / `SDL_UnlockAudioDevice` wrap all writes to the ring buffer in `Push_Audio`. Without this, torn reads between the SDL audio callback thread and the main decode thread can corrupt ring state after extended playback.

---

## Contributing

If you want to contribute to this project, please follow these steps:

1. Fork the repository
2. Create a new branch
3. Make your changes
4. Commit your changes
5. Push your changes to the remote repository
6. Create a pull request

Please read our [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on how to contribute to this project.

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.