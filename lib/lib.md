# lib\ — Required Files for Windows Build

This folder must contain the **import libraries** (.a files) for the linker.
The actual **DLLs** go in `build\` beside the final .exe.

---

## SDL2 — From `SDL2-devel-2.0.5-mingw`

Inside that zip, go to: `SDL2-devel-2.0.5-mingw\x86_64-w64-mingw32\`

Copy these into `lib\`:
```
lib\libSDL2.a          ← from x86_64-w64-mingw32\lib\libSDL2.a
lib\libSDL2main.a      ← from x86_64-w64-mingw32\lib\libSDL2main.a
```

Copy this into `build\` (beside the .exe):
```
build\SDL2.dll         ← from x86_64-w64-mingw32\bin\SDL2.dll
```

---

## FFmpeg — From `ffmpeg-8.0.1-full_build-shared`

Inside that zip/folder, go to: `ffmpeg-8.0.1-full_build-shared\lib\`

Copy these into `lib\`:
```
lib\libavcodec.dll.a
lib\libavdevice.dll.a
lib\libavfilter.dll.a
lib\libavformat.dll.a
lib\libavutil.dll.a
lib\libswresample.dll.a
lib\libswscale.dll.a
```

Copy these into `build\` (beside the .exe):
```
build\avcodec-62.dll
build\avdevice-62.dll
build\avfilter-11.dll
build\avformat-62.dll
build\avutil-60.dll
build\swresample-6.dll
build\swscale-9.dll
```

---

## Important: GPR Linker Flag Naming

GPRbuild's `-lSDL2` tells the MinGW linker to look for `libSDL2.a` (it auto-adds the `lib` prefix and `.a` suffix).

GPRbuild's `-lavcodec` looks for `libavcodec.dll.a` OR `libavcodec.a` — the FFmpeg shared build provides `libavcodec.dll.a`, which is correct.

**Rename rule:** If your linker says "cannot find -lavcodec", rename:
```
libavcodec.dll.a  →  (keep as-is, MinGW finds it automatically)
```
If it still fails, try creating a symlink or copy:
```
copy lib\libavcodec.dll.a lib\libavcodec.a
```

---

## Verification Checklist

Run `scripts\build.bat` — it will tell you exactly which files are missing.

Your current lib\ already has (from screenshots):
- [x] libSDL2.a
- [x] libSDL2.dll.a
- [x] libavcodec.dll.a
- [x] libavdevice.dll.a
- [x] libavfilter.dll.a
- [x] libavformat.dll.a
- [x] libavutil.dll.a
- [x] libswresample.dll.a
- [x] libswscale.dll.a
- [x] SDL2.dll (should be in build\ not lib\, but either works)
