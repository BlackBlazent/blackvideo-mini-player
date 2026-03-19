/*
 * thumb_preview.c  —  BlackVideo Mini Player  v2.4
 *
 * Loads JPEG thumbnails and renders a preview popup above the seek bar.
 *
 * JPEG decoding strategy (no stb_image.h required):
 *   Windows : Windows Imaging Component (WIC) — built into all Windows 7+.
 *             wincodec.h + ole32 + windowscodecs — zero extra DLLs.
 *   Linux   : SDL_image (SDL2_image) if available; else silently skipped.
 *   macOS   : Same as Linux fallback.
 *
 * Exports called from ui_overlay.c:
 *   bv_tp_init()
 *   bv_tp_set_preview(path, timestamp_sec)
 *   bv_tp_draw(renderer, seek_x, seek_y, font)
 *   bv_tp_free(renderer)
 */

#include <SDL.h>
#include <SDL_ttf.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define THUMB_W   160
#define THUMB_H    90
#define POPUP_PAD   6
#define POPUP_W   (THUMB_W + POPUP_PAD * 2)
#define POPUP_H   (THUMB_H + POPUP_PAD * 2 + 20)

/* ── State ──────────────────────────────────────────────────────────────── */
static char        g_thumb_path[2048]      = {0};
static float       g_thumb_timestamp       = 0.0f;
static char        g_thumb_path_last[2048] = {0};
static SDL_Texture *g_thumb_tex            = NULL;

/* ══════════════════════════════════════════════════════════════════════════
 *  Windows WIC JPEG loader
 * ══════════════════════════════════════════════════════════════════════════ */
#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#define COBJMACROS          /* use C-style COM macros, no C++ needed */
#include <windows.h>
#include <wincodec.h>       /* WIC — ships with Windows 7+ */

/*
 * Load a JPEG file via WIC and return an SDL_Texture.
 * Returns NULL on any error (file not found, not a JPEG, etc.).
 */
static SDL_Texture *load_jpeg_wic(SDL_Renderer *ren, const char *path)
{
    IWICImagingFactory   *factory  = NULL;
    IWICBitmapDecoder    *decoder  = NULL;
    IWICBitmapFrameDecode *frame   = NULL;
    IWICFormatConverter  *conv     = NULL;
    SDL_Texture          *tex      = NULL;
    UINT                  img_w    = 0, img_h = 0;
    BYTE                 *pixels   = NULL;
    HRESULT               hr;

    /* Convert path to wide string */
    int wlen = MultiByteToWideChar(CP_UTF8, 0, path, -1, NULL, 0);
    if (wlen <= 0) return NULL;
    WCHAR *wpath = (WCHAR *)_alloca(wlen * sizeof(WCHAR));
    MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, wlen);

    /* Create WIC factory */
    hr = CoCreateInstance(
            &CLSID_WICImagingFactory, NULL,
            CLSCTX_INPROC_SERVER,
            &IID_IWICImagingFactory,
            (void **)&factory);
    if (FAILED(hr)) return NULL;

    /* Open file */
    hr = IWICImagingFactory_CreateDecoderFromFilename(
            factory, wpath, NULL,
            GENERIC_READ,
            WICDecodeMetadataCacheOnLoad,
            &decoder);
    if (FAILED(hr)) goto cleanup;

    /* Get first frame */
    hr = IWICBitmapDecoder_GetFrame(decoder, 0, &frame);
    if (FAILED(hr)) goto cleanup;

    /* Create format converter (→ 32bpp BGRA) */
    hr = IWICImagingFactory_CreateFormatConverter(factory, &conv);
    if (FAILED(hr)) goto cleanup;

    hr = IWICFormatConverter_Initialize(
            conv,
            (IWICBitmapSource *)frame,
            &GUID_WICPixelFormat32bppBGRA,
            WICBitmapDitherTypeNone,
            NULL, 0.0,
            WICBitmapPaletteTypeMedianCut);
    if (FAILED(hr)) goto cleanup;

    hr = IWICBitmapSource_GetSize((IWICBitmapSource *)conv, &img_w, &img_h);
    if (FAILED(hr) || img_w == 0 || img_h == 0) goto cleanup;

    pixels = (BYTE *)malloc((size_t)img_w * img_h * 4);
    if (!pixels) goto cleanup;

    hr = IWICBitmapSource_CopyPixels(
            (IWICBitmapSource *)conv,
            NULL,
            img_w * 4,
            img_w * img_h * 4,
            pixels);
    if (FAILED(hr)) goto cleanup;

    /* WIC gives us BGRA; SDL wants BGRA too on little-endian — create surface */
    {
        SDL_Surface *surf = SDL_CreateRGBSurfaceFrom(
            pixels,
            (int)img_w, (int)img_h,
            32, (int)(img_w * 4),
            0x00FF0000,   /* R mask (BGRA → B=0x00FF0000 is wrong; fix below) */
            0x0000FF00,
            0x000000FF,
            0xFF000000);
        /* WIC BGRA: B at byte 0, G at 1, R at 2, A at 3 (little-endian) */
        /* SDL_CreateRGBSurfaceFrom masks are for pixel as Uint32:           */
        /*   pixel = 0xAARRGGBB on little-endian memory BGRA → 0xAARRGGBB  */
        /* So: R=0x00FF0000, G=0x0000FF00, B=0x000000FF, A=0xFF000000       */
        if (surf) {
            tex = SDL_CreateTextureFromSurface(ren, surf);
            SDL_FreeSurface(surf);
        }
    }

cleanup:
    if (pixels)  free(pixels);
    if (conv)    IWICFormatConverter_Release(conv);
    if (frame)   IWICBitmapFrameDecode_Release(frame);
    if (decoder) IWICBitmapDecoder_Release(decoder);
    if (factory) IWICImagingFactory_Release(factory);
    return tex;
}

#else  /* ── POSIX fallback: try SDL_image, else skip ──────────────────── */

/* Weak-link to SDL_image: if libSDL2_image is present use it, else NULL */
#if defined(__has_include) && __has_include(<SDL_image.h>)
#  include <SDL_image.h>
#  define HAVE_SDL_IMAGE 1
#endif

static SDL_Texture *load_jpeg_wic(SDL_Renderer *ren, const char *path)
{
#ifdef HAVE_SDL_IMAGE
    SDL_Surface *surf = IMG_Load(path);
    if (!surf) return NULL;
    SDL_Texture *tex = SDL_CreateTextureFromSurface(ren, surf);
    SDL_FreeSurface(surf);
    return tex;
#else
    (void)ren; (void)path;
    return NULL;   /* thumbnail preview not supported without SDL_image */
#endif
}

#endif /* _WIN32 */

/* ── Public API ─────────────────────────────────────────────────────────── */

void bv_tp_init(void)
{
    g_thumb_path[0]      = '\0';
    g_thumb_path_last[0] = '\0';
    g_thumb_tex          = NULL;

#ifdef _WIN32
    CoInitializeEx(NULL, COINIT_MULTITHREADED);
#endif
}

void bv_tp_set_preview(const char *path, float timestamp_sec)
{
    if (!path) { g_thumb_path[0] = '\0'; return; }
    strncpy(g_thumb_path, path, sizeof(g_thumb_path) - 1);
    g_thumb_path[sizeof(g_thumb_path) - 1] = '\0';
    g_thumb_timestamp = timestamp_sec;
}

void bv_tp_draw(SDL_Renderer *ren, int seek_x, int seek_y, TTF_Font *font)
{
    if (g_thumb_path[0] == '\0' || !ren) return;

    /* Reload texture if path changed */
    if (strcmp(g_thumb_path, g_thumb_path_last) != 0) {
        if (g_thumb_tex) { SDL_DestroyTexture(g_thumb_tex); g_thumb_tex = NULL; }
        g_thumb_tex = load_jpeg_wic(ren, g_thumb_path);
        strncpy(g_thumb_path_last, g_thumb_path, sizeof(g_thumb_path_last) - 1);
    }
    if (!g_thumb_tex) return;

    /* Position popup centred on seek_x, just above the seek bar */
    int px = seek_x - POPUP_W / 2;
    int py = seek_y - POPUP_H - 4;

    int win_w, win_h;
    SDL_GetRendererOutputSize(ren, &win_w, &win_h);
    if (px < 0) px = 0;
    if (px + POPUP_W > win_w) px = win_w - POPUP_W;
    if (py < 0) py = 0;

    /* Shadow box */
    SDL_SetRenderDrawBlendMode(ren, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(ren, 0, 0, 0, 200);
    SDL_Rect bg = {px, py, POPUP_W, POPUP_H};
    SDL_RenderFillRect(ren, &bg);

    /* Border */
    SDL_SetRenderDrawColor(ren, 80, 80, 80, 255);
    SDL_RenderDrawRect(ren, &bg);

    /* Thumbnail */
    SDL_Rect dst = {px + POPUP_PAD, py + POPUP_PAD, THUMB_W, THUMB_H};
    SDL_RenderCopy(ren, g_thumb_tex, NULL, &dst);

    /* Timestamp label */
    if (font) {
        int total_s = (int)g_thumb_timestamp;
        int mins    = total_s / 60;
        int secs    = total_s % 60;
        char label[32];
        snprintf(label, sizeof(label), "%d:%02d", mins, secs);
        SDL_Color white = {220, 220, 220, 255};
        SDL_Surface *ts = TTF_RenderUTF8_Blended(font, label, white);
        if (ts) {
            SDL_Texture *tt = SDL_CreateTextureFromSurface(ren, ts);
            if (tt) {
                SDL_Rect tr = {
                    px + POPUP_W / 2 - ts->w / 2,
                    py + POPUP_PAD + THUMB_H + 2,
                    ts->w, ts->h
                };
                SDL_RenderCopy(ren, tt, NULL, &tr);
                SDL_DestroyTexture(tt);
            }
            SDL_FreeSurface(ts);
        }
    }
}

void bv_tp_free(SDL_Renderer *ren)
{
    (void)ren;
    if (g_thumb_tex) {
        SDL_DestroyTexture(g_thumb_tex);
        g_thumb_tex = NULL;
    }
    g_thumb_path[0]      = '\0';
    g_thumb_path_last[0] = '\0';

#ifdef _WIN32
    CoUninitialize();
#endif
}
