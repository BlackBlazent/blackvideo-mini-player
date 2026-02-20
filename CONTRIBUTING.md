# Contributing to BlackVideo Mini Player

Thank you for your interest in contributing! This document explains the project structure, coding conventions, and how to submit changes.

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Development Environment Setup](#development-environment-setup)
3. [Project Structure](#project-structure)
4. [Ada Coding Conventions](#ada-coding-conventions)
5. [Key Ada + C Binding Rules](#key-ada--c-binding-rules)
6. [How to Build](#how-to-build)
7. [How to Submit Changes](#how-to-submit-changes)
8. [Reporting Bugs](#reporting-bugs)
9. [Adding a New Feature](#adding-a-new-feature)
10. [Common Pitfalls](#common-pitfalls)

---

## Project Overview

BlackVideo Mini Player is a lightweight, cross-platform video player written in **Ada 2012** using:

- **SDL2** — window, rendering, audio output
- **FFmpeg** (libavcodec / libavformat / libavutil / libswscale) — video decoding, YUV→RGB conversion

It is designed as a **sidecar player** for the BlackVideo Tauri v2 desktop application, but also works standalone from the command line and via OS right-click context menus.

---

## Development Environment Setup

### Windows (required)
| Tool | Version | Where to get |
|------|---------|-------------|
| GNAT | 2021 (tested) | [GNAT Community](https://www.adacore.com/community) or [Alire](https://alire.ada.dev/) |
| gprbuild | bundled with GNAT | — |
| SDL2 | 2.0.5 MinGW | [SDL Releases](https://github.com/libsdl-org/SDL/releases) — use `SDL2-devel-X.X.X-mingw.zip`, x86_64 |
| FFmpeg | 8.x shared | [BtbN FFmpeg Builds](https://github.com/BtbN/FFmpeg-Builds/releases) — use `ffmpeg-master-latest-win64-lgpl-shared` |

### Linux
```bash
sudo apt install gnat gprbuild \
    libsdl2-dev \
    libavcodec-dev libavformat-dev libavutil-dev \
    libswscale-dev libswresample-dev
```

### macOS
```bash
brew install gnat gprbuild sdl2 ffmpeg
```

---

## Project Structure

```
blackvideo-mini-player/
│
├── blackvideo_player.gpr      ← GPRbuild project file (entry point)
├── CLEANUP.bat                ← Delete leftover files from old versions (Windows)
│
├── src/                       ← Application logic (Ada)
│   ├── main.adb               ← Entry point, CLI arg parsing
│   ├── player.ads / .adb      ← Main loop, event handling, state machine
│   ├── video_decoder.ads/.adb ← FFmpeg decode pipeline (Open/Seek/NextFrame)
│   ├── renderer.ads / .adb    ← SDL2 texture + letterbox rendering
│   ├── audio.ads / .adb       ← SDL2 audio callback + ring buffer
│   └── utils.ads / .adb       ← Base_Name, Format_Time helpers
│
├── bindings/
│   ├── ffmpeg/
│   │   ├── ffmpeg.ads         ← Parent package (required by Ada hierarchy rules)
│   │   ├── ffmpeg-avutil.ads  ← libavutil: AVFrame, pixel formats, av_image_*
│   │   ├── ffmpeg-avcodec.ads ← libavcodec: decode context, packet, codec
│   │   ├── ffmpeg-avformat.ads← libavformat: container open/read/seek
│   │   ├── ffmpeg-swscale.ads ← libswscale: YUV→RGB
│   │   └── ffmpeg-swresample.ads ← libswresample: audio resampling
│   └── sdl2/
│       ├── sdl.ads                  ← SDL_Init / SDL_Quit / SDL_Delay
│       ├── sdl-video.ads            ← Shared types: handles, SDL_Rect, pixel formats
│       ├── sdl-video-windows.ads/adb   ← Window create/destroy/fullscreen
│       ├── sdl-video-renderers.ads/adb ← Renderer create/draw/present
│       ├── sdl-video-textures.ads/adb  ← Texture create/lock/update
│       ├── sdl-events.ads/adb       ← Flat SDL_Event polling + field extractors
│       ├── sdl-events-keyboards.ads ← SDL keycode constants
│       └── sdl-audio.ads            ← Audio device + callback binding
│
├── lib/                       ← Import libraries (.a files) for Windows linker
├── build/                     ← Compiled output (auto-created by gprbuild)
├── scripts/
│   ├── build.bat              ← Windows build script
│   └── build.sh               ← Linux/macOS build script
└── tools/
    └── context_menu/          ← OS right-click integration installers
```

---

## Ada Coding Conventions

### File naming (critical — GNAT enforces this)
Ada package `Foo.Bar.Baz` **must** be in file `foo-bar-baz.ads` (spec) and `foo-bar-baz.adb` (body). GNAT cannot find a child package declared inside a parent file.

```ada
-- WRONG: do not put SDL.Video.Windows inside sdl-video.ads
-- RIGHT: put SDL.Video.Windows in its own sdl-video-windows.ads
```

### Every child package needs a parent
`with FFmpeg.AVUtil` requires `ffmpeg.ads` to exist (even if empty).  
`with SDL.Video.Windows` requires `sdl-video.ads` to exist. ✓

### Types vs Integers
| C type | Ada type | Notes |
|--------|----------|-------|
| `int` | `Interfaces.C.int` | Signed — cannot use `or` |
| `unsigned` | `Interfaces.C.unsigned` | Modular — `or`/`and`/`xor` work |
| `Uint32 flags` | `constant unsigned` | Must be modular for bitwise ops |
| `SDL_Keycode` | `Interfaces.C.int` | Signed (can be negative for scan-masked keys) |

### `or` operator on flags
```ada
-- WRONG — int is not modular, has no 'or':
SDL_AUDIO_ALLOW_FREQUENCY_CHANGE : constant int := 1;  -- int!

-- RIGHT — unsigned is modular, 'or' works:
SDL_AUDIO_ALLOW_FREQUENCY_CHANGE : constant unsigned := 1;

-- Usage:
Dev := SDL_OpenAudioDevice (...,
    SDL_AUDIO_ALLOW_FREQUENCY_CHANGE or SDL_AUDIO_ALLOW_CHANNELS_CHANGE);
```

### C callback convention
```ada
-- WRONG — Ada procedure, cannot take 'Access for C callback type:
procedure Fill_Audio (User_Data : Address; Stream : Address; Len : int);

-- RIGHT — add the Convention pragma immediately after the declaration:
procedure Fill_Audio (User_Data : Address; Stream : Address; Len : int);
pragma Convention (C, Fill_Audio);  -- now Fill_Audio'Access is legal
```

### Unchecked bit-reinterpretation
When you need `unsigned` → `int` without range checking (e.g. SDL keycodes, event byte fields):
```ada
with Ada.Unchecked_Conversion;
function U32_To_S32 is new Ada.Unchecked_Conversion
  (Source => Interfaces.C.unsigned, Target => Interfaces.C.int);
```
**Never** write `int(unsigned_value)` when the value might exceed `int'Last` — it raises `Constraint_Error` at runtime.

### Unused return values
GNAT warns on unused variables. Use `pragma Unreferenced`:
```ada
procedure Clear (Rend : Renderer) is
   Discard : int;
   pragma Unreferenced (Discard);
begin
   Discard := SDL_RenderClear (Rend);
end Clear;
```

---

## Key Ada + C Binding Rules

### Do NOT ship `sdl-video.adb`
The `SDL.Video` package is declared `pure` (only types and constants, no subprogram bodies). Ada forbids a package body for a `pragma Pure` package. If `sdl-video.adb` exists in the source directory, GNAT will fail with:
```
sdl-video.adb:7: spec of this package does not allow a body
```
Run `CLEANUP.bat` if you see this error.

### SDL_WINDOWPOS_CENTERED is `unsigned`, not `int`
`0x2FFF0000` is larger than `int'Last`. Declare it as `unsigned`:
```ada
SDL_WINDOWPOS_CENTERED : constant unsigned := 16#2FFF_0000#;
```

### Forward declarations in package bodies
In Ada, if procedure `B` calls procedure `A`, but `A` is defined *after* `B` in the same body, you need a forward declaration:
```ada
package body Foo is
   procedure A;   -- forward declaration (no body here)

   procedure B is
   begin
      A;          -- legal because A is declared above
   end B;

   procedure A is -- full body
   begin ...
   end A;
end Foo;
```

---

## How to Build

### Windows (from any location)
```bat
scripts\build.bat
```
Or double-click `scripts\build.bat` from Explorer. It auto-detects the project root.

### Windows (GNAT Studio)
1. File → Open Project → select `blackvideo_player.gpr`
2. Set Scenario variable `OS_TARGET = windows`
3. Build → Build All

### Linux / macOS
```bash
chmod +x scripts/build.sh
./scripts/build.sh
```

---

## How to Submit Changes

1. Fork the repository on GitHub
2. Create a feature branch: `git checkout -b feature/my-improvement`
3. Make your changes, ensuring the project builds cleanly with no errors
4. Run a quick smoke test: `build\blackvideo-player.exe test_video.mp4`
5. Commit with a descriptive message:
   ```
   fix(audio): add pragma Convention for C callback
   feat(renderer): add pillarbox mode for portrait video
   ```
6. Push and open a Pull Request with:
   - What the change does
   - Which errors/warnings it fixes (if applicable)
   - Which platforms you tested on

---

## Reporting Bugs

Please include:
- OS and version (e.g. Windows 11, Ubuntu 24.04)
- GNAT version (`gnat --version`)
- SDL2 version and FFmpeg version (DLL names in `build\`)
- Full error output from `scripts\build.bat` or GNAT Studio Builder Results
- The video file format/codec that caused a runtime issue (if applicable)

Open an issue at: `https://github.com/BlackBlazent/blackvideo-mini-player/issues`

---

## Adding a New Feature

### New keyboard shortcut
Edit `src/player.adb`, in `Handle_Key`:
```ada
elsif K = SDL.Events.Keyboards.SDLK_p then
   -- your action here
   Put_Line ("[Player] P pressed");
end if;
```
Add the keycode constant to `bindings/sdl2/sdl-events-keyboards.ads` if it's missing.

### New video output format
The RGB24 pipeline is in `src/video_decoder.adb`. Change `AV_PIX_FMT_RGB24` and the SDL texture format (`SDL_PIXELFORMAT_RGB24`) in `src/renderer.adb` to match.

### New SDL2 binding
1. Create `bindings/sdl2/sdl-newpackage.ads` for package `SDL.New_Package`
2. Create `bindings/sdl2/sdl-newpackage.adb` if it has Ada-side implementations
3. The `.gpr` already includes `bindings/sdl2` as a source dir — no changes needed

### New FFmpeg binding
1. Create `bindings/ffmpeg/ffmpeg-newlib.ads` for package `FFmpeg.NewLib`
2. Add the corresponding `-lnewlib` linker flag in `blackvideo_player.gpr`

---

## Common Pitfalls

| Error | Cause | Fix |
|-------|-------|-----|
| `spec does not allow a body` | Old `sdl-video.adb` in source dir | Run `CLEANUP.bat` or delete it manually |
| `file "ffmpeg.ads" not found` | Missing parent package | Ensure `bindings/ffmpeg/ffmpeg.ads` exists |
| `value not in range of int` | Assigning unsigned literal to int | Use `Ada.Unchecked_Conversion` or declare as `unsigned` |
| `no operator Or for type int` | Bit flags declared as `int` | Change to `unsigned` |
| `has wrong convention` | C callback without `pragma Convention (C, ...)` | Add the pragma after the procedure declaration |
| `not directly visible` | Missing `use PackageName` | Add `use Video_Decoder` (or relevant package) in body |
| `gprbuild.gpr not found` | Running from wrong directory | `build.bat` auto-corrects this; or `cd` to project root |
| `Handle_Key is undefined` | Called before declaration in same body | Add forward declaration at top of package body |

---

*BlackVideo Mini Player is part of the BlackBlazent / BlackVideo project.*  
*Licensed under MIT — see LICENSE for details.*
