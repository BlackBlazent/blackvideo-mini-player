# Whisper Offline Captions — Setup Guide

BlackVideo v2.3 supports **fully offline** subtitle generation and translation via [whisper.cpp](https://github.com/ggerganov/whisper.cpp).

---

## Quick Setup (Production)

Place these files in `build\` beside `blackvideo-player.exe`:

```
build\
├── blackvideo-player.exe
├── whisper-cli.exe         ← from whisper-bin-x64.zip
├── whisper.dll             ← from whisper-bin-x64.zip
├── ggml.dll                ← from whisper-bin-x64.zip
├── ggml-base.dll           ← from whisper-bin-x64.zip
├── ggml-cpu.dll            ← from whisper-bin-x64.zip
└── models\
    └── ggml-base.bin       ← downloaded model file
```

That's it. No `.env` file needed in production.

---

## How to Use

1. Open a video in BlackVideo.
2. **Right-click** anywhere on the video.
3. Choose:
   - **Generate Captions (Whisper)** — transcribes in the video's original language
   - **Translate to English (Whisper)** — transcribes and translates to English

The player stays responsive while Whisper runs in the background. A status message appears in the control bar. When complete, the SRT is auto-loaded into **Track 1**.

---

## Upgrading the Model

Models are hot-swappable — just replace the `.bin` file. No rebuild needed.

| Model | Size | Speed | Accuracy |
|-------|------|-------|----------|
| `ggml-tiny.bin`   | 75 MB   | Fastest | Low    |
| `ggml-base.bin`   | 142 MB  | Fast    | Good   |
| `ggml-small.bin`  | 466 MB  | Medium  | Better |
| `ggml-medium.bin` | 1.5 GB  | Slow    | High   |
| `ggml-large.bin`  | 2.9 GB  | Slowest | Best   |

Download from: https://huggingface.co/ggerganov/whisper.cpp

To override the model path without moving files, use `.env` (dev mode only):

```
BLACKVIDEO_WHISPER_MODEL=C:\path\to\ggml-medium.bin
```

---

## Developer: .env File

For development, rename `.env.example` to `.env` and set:

```env
BLACKVIDEO_WHISPER_PATH=C:\Projects\whisper-bin-x64\Release\whisper-cli.exe
BLACKVIDEO_WHISPER_MODEL=C:\Projects\whisper-bin-x64\Release\models\ggml-base.bin
```

The `.env` file is loaded at runtime and sets environment variables. It is silently skipped in production (when absent).

**Never commit `.env` to version control.**

---

## Requirements

- `ffmpeg.exe` on `PATH` **or** `ffmpeg.exe` in `build\` (for audio extraction)
- Windows 10/11 x64
- At least 4 GB RAM for `base` model; 8 GB+ for `medium`/`large`

---

## Troubleshooting

| Console message | Fix |
|-----------------|-----|
| `whisper-cli.exe not found` | Place in `build\` or set `BLACKVIDEO_WHISPER_PATH` |
| `model not found` | Place `ggml-base.bin` in `build\models\` or set `BLACKVIDEO_WHISPER_MODEL` |
| `audio extraction failed` | Install `ffmpeg` and add it to `PATH` |
| `whisper-cli failed (exit N)` | Check console output; model may be corrupted |
