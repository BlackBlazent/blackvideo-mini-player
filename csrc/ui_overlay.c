/*
 * ui_overlay.c  —  BlackVideo Mini Player UI Overlay  v2.4
 *
 * v2.4 changes:
 *   - Welcome screen (g_no_video): centred play icon, prompt, Open button
 *   - Thumbnail preview popup above seek bar (delegates to thumb_preview.c)
 *   - Progress bar drawn beside Whisper / LLM status text in banner
 *   - Update notification badge on ⋮ menu button
 *   - Context menu: LLM sub-menu (Claude/OpenAI/Gemini/DeepSeek/Grok)
 *   - Context menu: Clear Thumbnail Cache + Check for Updates items
 *   - API key input overlay (SDL2 text input, drawn in C)
 *   - Update available overlay (version / notes / Download / Later)
 *   - CTX_COUNT expanded to 14
 */

#include <SDL.h>
#include <SDL_ttf.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/* ── forward declaration from thumb_preview.c ──────────────────────────── */
void bv_tp_draw(SDL_Renderer *ren, int seek_x, int seek_y, TTF_Font *font);
void bv_tp_set_preview(const char *path, float ts);
void bv_tp_free(SDL_Renderer *ren);

/* ── Layout ───────────────────────────────────────────────────────────── */
#define BAR_H         72
#define SEEK_MARGIN   12
#define SEEK_TOP      8
#define SEEK_H        5
#define KNOB_R        7
#define BTN_ROW_TOP   30
#define BTN_W         38
#define BTN_H         34
#define ICON_SZ       16
#define TIME_FONT_SZ  12
#define CTX_W         280
#define CTX_ITEM_H    30
#define CTX_PAD_X     14
#define CTX_FONT_SZ   13
#define PROG_BAR_W    80
#define PROG_BAR_H    6

/* ── Colours ──────────────────────────────────────────────────────────── */
#define C_BAR_BG      16, 16, 16, 215
#define C_SEEK_TRACK  55, 55, 55, 255
#define C_SEEK_FILL   210, 45, 45, 255
#define C_KNOB        240, 240, 240, 255
#define C_ICON        210, 210, 210, 255
#define C_ICON_ON     210, 45,  45, 255
#define C_BTN_HOVER   255, 255, 255, 28
#define C_TIME        195, 195, 195, 255
#define C_CTX_BG      24, 24, 24, 248
#define C_CTX_BORDER  60, 60, 60, 255
#define C_CTX_HOVER   48, 48, 48, 255
#define C_CTX_SEP     52, 52, 52, 255
#define C_CTX_TEXT    205, 205, 205, 255
#define C_CTX_DIM     85,  85,  85, 255
#define C_CTX_ACTIVE  210, 45,  45, 255
#define C_CTX_WHISPER 80, 160, 220, 255
#define C_CTX_LLM     120, 200, 140, 255
#define C_SUB_TEXT    255, 255, 255, 255
#define C_SUB_SHADOW  0,   0,   0, 200
#define C_STATUS      180, 200, 255, 255
#define C_BADGE       210,  45,  45, 255
#define C_WELCOME_BG  14,  14,  14, 255
#define C_WELCOME_TXT 160, 160, 160, 255
#define C_OPEN_BTN    210,  45,  45, 255
#define C_OVERLAY_BG  20,  20,  20, 240
#define C_OVERLAY_BDR 70,  70,  70, 255

/* ── Button indices ───────────────────────────────────────────────────── */
#define BTN_PREV       0
#define BTN_PLAYPAUSE  1
#define BTN_NEXT       2
#define BTN_LOOP       3
#define BTN_VOLUME     4
#define BTN_SPEED      5
#define BTN_FULLSCREEN 6
#define BTN_MENU       7
#define BTN_COUNT      8

/* ── Context menu item indices ────────────────────────────────────────── */
#define CTX_OPEN_FILE         0
#define CTX_SUB_NONE          1
#define CTX_SUB_1             2
#define CTX_SUB_2             3
#define CTX_SUB_3             4
#define CTX_WHISPER_GEN       5
#define CTX_WHISPER_TRANS     6
#define CTX_LLM_CLAUDE        7
#define CTX_LLM_OPENAI        8
#define CTX_LLM_GEMINI        9
#define CTX_LLM_DEEPSEEK      10
#define CTX_LLM_GROK          11
#define CTX_CLEAR_THUMB_CACHE 12
#define CTX_CHECK_UPDATES     13
#define CTX_COUNT             14

/* ── State ────────────────────────────────────────────────────────────── */
static TTF_Font *g_font_time = NULL;
static TTF_Font *g_font_ui   = NULL;
static TTF_Font *g_font_sub  = NULL;
static TTF_Font *g_font_big  = NULL;   /* welcome screen large text */
static int       g_init      = 0;
static int       g_win_w     = 1280;
static int       g_win_h     = 720;

static int g_bar_y   = 0;
static int g_sk_x    = 0, g_sk_y = 0, g_sk_w = 0;
static SDL_Rect g_btn[BTN_COUNT];

static int g_hover_btn = -1;
static int g_seeking   = 0;

static int g_ctx_open  = 0;
static int g_ctx_x     = 0, g_ctx_y = 0;
static int g_hover_ctx = -1;

/* Subtitles */
static char g_sub[3][512];
static int  g_sub_count  = 0;
static int  g_sub_active = -1;

/* Current subtitle cue text */
static char g_sub_text[1024] = "";

/* Whisper / LLM status line */
static char g_whisper_status[256] = "";

/* Welcome screen */
static int g_no_video = 0;

/* Update badge / overlay */
static int  g_update_available = 0;
static char g_update_version[64] = "";
static int  g_show_update_overlay = 0;
static char g_update_notes[256]   = "";
static char g_update_dl_url[512]  = "";
static int  g_update_download_clicked = 0;
static int  g_update_later_clicked    = 0;
static SDL_Rect g_update_btn_dl   = {0};
static SDL_Rect g_update_btn_late = {0};

/* API key input overlay */
static int  g_show_key_input     = 0;
static char g_key_provider[64]   = "";
static char g_key_value[256]     = "";
static int  g_key_input_submitted = 0;
static SDL_Rect g_key_ok_btn     = {0};
static SDL_Rect g_key_close_btn  = {0};
static SDL_Rect g_key_cancel_btn = {0};
static int  g_key_cancelled      = 0;

/* ── Draw helpers ─────────────────────────────────────────────────────── */
static void col(SDL_Renderer *r, Uint8 R, Uint8 G, Uint8 B, Uint8 A)
{
    SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(r, R, G, B, A);
}

static void frect(SDL_Renderer *r, int x, int y, int w, int h)
{
    SDL_Rect rc = {x, y, w, h};
    SDL_RenderFillRect(r, &rc);
}

static void rrect(SDL_Renderer *r, int x, int y, int w, int h)
{
    SDL_Rect rc = {x, y, w, h};
    SDL_RenderDrawRect(r, &rc);
}

static void fcircle(SDL_Renderer *r, int cx, int cy, int rad)
{
    for (int dy = -rad; dy <= rad; dy++) {
        int dx = (int)sqrt((double)(rad*rad - dy*dy));
        SDL_RenderDrawLine(r, cx-dx, cy+dy, cx+dx, cy+dy);
    }
}

static int draw_text(SDL_Renderer *r, TTF_Font *f,
                     const char *s, int x, int y,
                     Uint8 R, Uint8 G, Uint8 B, Uint8 A)
{
    if (!f || !s || !s[0]) return 0;
    SDL_Color c = {R, G, B, A};
    SDL_Surface *surf = TTF_RenderUTF8_Blended(f, s, c);
    if (!surf) return 0;
    SDL_Texture *tex = SDL_CreateTextureFromSurface(r, surf);
    int tw = surf->w, th = surf->h;
    SDL_FreeSurface(surf);
    if (!tex) return 0;
    SDL_Rect dst = {x, y, tw, th};
    SDL_RenderCopy(r, tex, NULL, &dst);
    SDL_DestroyTexture(tex);
    return tw;
}

static int text_width(TTF_Font *f, const char *s)
{
    if (!f || !s) return 0;
    int w = 0, h = 0;
    TTF_SizeUTF8(f, s, &w, &h);
    return w;
}

/* ── Subtitle overlay ─────────────────────────────────────────────────── */
static void draw_subtitle_overlay(SDL_Renderer *r)
{
    if (!g_sub_text[0] || !g_font_sub) return;
    SDL_Color white = {255, 255, 255, 255};
    SDL_Surface *surf = TTF_RenderUTF8_Blended(g_font_sub, g_sub_text, white);
    if (!surf) return;
    SDL_Texture *tex = SDL_CreateTextureFromSurface(r, surf);
    int tw = surf->w, th = surf->h;
    SDL_FreeSurface(surf);
    if (!tex) return;
    int x = (g_win_w - tw) / 2;
    int y = g_bar_y - th - 10;
    if (y < 4) y = 4;
    SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(r, 0, 0, 0, 160);
    SDL_Rect shadow = {x - 6, y - 3, tw + 12, th + 6};
    SDL_RenderFillRect(r, &shadow);
    SDL_Rect dst = {x, y, tw, th};
    SDL_RenderCopy(r, tex, NULL, &dst);
    SDL_DestroyTexture(tex);
}

/* ── Welcome screen ───────────────────────────────────────────────────── */
static void draw_welcome_screen(SDL_Renderer *r)
{
    /* Background */
    col(r, C_WELCOME_BG);
    frect(r, 0, 0, g_win_w, g_win_h);

    int cx = g_win_w / 2;
    int cy = g_win_h / 2;

    /* Large play triangle */
    int ps = 80;
    col(r, 55, 55, 55, 255);
    for (int row = 0; row < ps * 2; row++) {
        float t  = (float)row / (ps * 2);
        float t2 = (t < 0.5f) ? t * 2 : (1.0f - t) * 2;
        int span = (int)(t2 * ps * 0.85f);
        SDL_RenderDrawLine(r,
            cx - ps/2, cy - ps + row,
            cx - ps/2 + span * 2, cy - ps + row);
    }

    /* Prompt text */
    int ty = cy + ps + 24;
    if (g_font_ui) {
        const char *msg = "Open a video or drop a file here";
        int tw = text_width(g_font_ui, msg);
        draw_text(r, g_font_ui, msg, cx - tw/2, ty, C_WELCOME_TXT);
    }

    /* Open button */
    int bw = 160, bh = 36;
    int bx = cx - bw/2, by = ty + 32;
    col(r, C_OPEN_BTN);
    frect(r, bx, by, bw, bh);
    if (g_font_ui) {
        const char *lbl = "Open File...";
        int tw = text_width(g_font_ui, lbl);
        draw_text(r, g_font_ui, lbl,
                  bx + bw/2 - tw/2, by + bh/2 - CTX_FONT_SZ/2,
                  255, 255, 255, 255);
    }
}

/* ── Progress bar helper ──────────────────────────────────────────────── */
/* Parse "NN%" from a status string like "Whisper: generating...  42%" */
static int parse_pct(const char *s)
{
    if (!s) return -1;
    int len = (int)strlen(s);
    /* scan backwards for digit before '%' */
    for (int i = len - 1; i >= 0; i--) {
        if (s[i] == '%') {
            int end = i;
            int start = end - 1;
            while (start > 0 && s[start-1] >= '0' && s[start-1] <= '9')
                start--;
            if (start < end) {
                int val = 0;
                for (int j = start; j < end; j++)
                    val = val * 10 + (s[j] - '0');
                return (val < 0) ? 0 : (val > 100) ? 100 : val;
            }
        }
    }
    return -1;
}

static void draw_progress_bar(SDL_Renderer *r, int x, int y, int pct)
{
    /* Track */
    col(r, 55, 55, 55, 200);
    frect(r, x, y, PROG_BAR_W, PROG_BAR_H);
    /* Fill */
    int fw = (PROG_BAR_W * pct) / 100;
    col(r, 80, 160, 220, 255);
    frect(r, x, y, fw, PROG_BAR_H);
    /* Border */
    col(r, 90, 90, 90, 200);
    rrect(r, x, y, PROG_BAR_W, PROG_BAR_H);
}

/* ── Update overlay ───────────────────────────────────────────────────── */
static void draw_update_overlay(SDL_Renderer *r)
{
    int ow = 340, oh = 140;
    int ox = g_win_w/2 - ow/2;
    int oy = g_win_h/2 - oh/2;

    /* Background */
    col(r, C_OVERLAY_BG);
    frect(r, ox, oy, ow, oh);
    col(r, C_OVERLAY_BDR);
    rrect(r, ox, oy, ow, oh);

    if (g_font_ui) {
        char title[96];
        snprintf(title, sizeof(title), "Update available:  v%s", g_update_version);
        draw_text(r, g_font_ui, title, ox+14, oy+14, 210, 210, 210, 255);
        draw_text(r, g_font_ui, g_update_notes, ox+14, oy+36, 150, 150, 150, 255);
    }

    /* Buttons */
    int bw = 100, bh = 30, gap = 12;
    int btotal = bw * 2 + gap;
    int bx = ox + ow/2 - btotal/2;
    int by = oy + oh - bh - 14;

    /* Download */
    col(r, C_OPEN_BTN);
    frect(r, bx, by, bw, bh);
    g_update_btn_dl = (SDL_Rect){bx, by, bw, bh};
    if (g_font_ui) {
        int tw = text_width(g_font_ui, "Download");
        draw_text(r, g_font_ui, "Download",
                  bx + bw/2 - tw/2, by + bh/2 - CTX_FONT_SZ/2,
                  255, 255, 255, 255);
    }

    /* Later */
    bx += bw + gap;
    col(r, 50, 50, 50, 255);
    frect(r, bx, by, bw, bh);
    col(r, C_OVERLAY_BDR);
    rrect(r, bx, by, bw, bh);
    g_update_btn_late = (SDL_Rect){bx, by, bw, bh};
    if (g_font_ui) {
        int tw = text_width(g_font_ui, "Later");
        draw_text(r, g_font_ui, "Later",
                  bx + bw/2 - tw/2, by + bh/2 - CTX_FONT_SZ/2,
                  195, 195, 195, 255);
    }
}

/* ── API key input overlay ────────────────────────────────────────────── */
static void draw_key_input_overlay(SDL_Renderer *r)
{
    /* Dim the whole screen first */
    col(r, 0, 0, 0, 160);
    frect(r, 0, 0, g_win_w, g_win_h);

    int ow = 420, oh = 160;
    int ox = g_win_w/2 - ow/2;
    int oy = g_win_h/2 - oh/2;

    /* Dialog background + border */
    col(r, 28, 28, 32, 255);
    frect(r, ox, oy, ow, oh);
    col(r, C_OVERLAY_BDR);
    rrect(r, ox, oy, ow, oh);

    /* Title */
    if (g_font_ui) {
        char title[128];
        snprintf(title, sizeof(title), "Enter API key for %s", g_key_provider);
        draw_text(r, g_font_ui, title, ox+14, oy+14, 220, 220, 220, 255);
    }

    /* Close (X) button — top-right corner */
    int cw = 22, ch = 22;
    int cx = ox + ow - cw - 8;
    int cy = oy + 8;
    col(r, 80, 40, 40, 220);
    frect(r, cx, cy, cw, ch);
    col(r, 180, 60, 60, 255);
    rrect(r, cx, cy, cw, ch);
    g_key_close_btn = (SDL_Rect){cx, cy, cw, ch};
    if (g_font_ui) {
        int tw = text_width(g_font_ui, "X");
        draw_text(r, g_font_ui, "X",
                  cx + cw/2 - tw/2, cy + ch/2 - CTX_FONT_SZ/2,
                  255, 200, 200, 255);
    }

    /* Text input box */
    int fx = ox+14, fy = oy+44, fw = ow-28, fh = 32;
    col(r, 20, 20, 24, 255);
    frect(r, fx, fy, fw, fh);
    col(r, 100, 100, 120, 255);
    rrect(r, fx, fy, fw, fh);

    if (g_font_ui) {
        /* Show masked key: first 4 + last 2 chars visible, rest as '*' */
        char masked[260] = "";
        int klen = (int)strlen(g_key_value);
        if (klen == 0) {
            draw_text(r, g_font_ui, "Paste or type your key...",
                      fx+8, fy+8, 80, 80, 90, 255);
        } else {
            int show_start = (klen > 6) ? 4 : klen;
            int show_end   = (klen > 6) ? 2 : 0;
            for (int i = 0; i < klen && i < (int)sizeof(masked)-1; i++) {
                int from_end = klen - 1 - i;
                masked[i] = (i < show_start || from_end < show_end)
                            ? g_key_value[i] : '*';
            }
            masked[klen] = '\0';
            draw_text(r, g_font_ui, masked, fx+8, fy+8, 210, 230, 210, 255);
        }
        /* Blinking cursor */
        int cur_x = fx + 8;
        if (klen > 0) cur_x += text_width(g_font_ui, "sk-ant-...") + 4;
        (void)cur_x; /* cursor cosmetic only */
    }

    /* Hint text */
    if (g_font_ui)
        draw_text(r, g_font_ui,
                  "Ctrl+V to paste  |  Enter or OK to save  |  Esc to cancel",
                  ox+14, fy+fh+6, 100, 100, 110, 255);

    /* OK button */
    int bw = 90, bh = 28;
    int bx = ox + ow - bw - 14;
    int by = oy + oh - bh - 12;
    col(r, C_OPEN_BTN);
    frect(r, bx, by, bw, bh);
    col(r, 60, 130, 60, 255);
    rrect(r, bx, by, bw, bh);
    g_key_ok_btn = (SDL_Rect){bx, by, bw, bh};
    if (g_font_ui) {
        int tw = text_width(g_font_ui, "OK");
        draw_text(r, g_font_ui, "OK",
                  bx + bw/2 - tw/2, by + bh/2 - CTX_FONT_SZ/2,
                  255, 255, 255, 255);
    }

    /* Cancel button */
    int cbw = 90, cbh = 28;
    int cbx = ox + 14;
    int cby = oy + oh - cbh - 12;
    col(r, 60, 60, 65, 255);
    frect(r, cbx, cby, cbw, cbh);
    col(r, 90, 90, 95, 255);
    rrect(r, cbx, cby, cbw, cbh);
    /* reuse close rect logic — clicking Cancel = same as X */
    if (g_font_ui) {
        int tw = text_width(g_font_ui, "Cancel");
        draw_text(r, g_font_ui, "Cancel",
                  cbx + cbw/2 - tw/2, cby + cbh/2 - CTX_FONT_SZ/2,
                  200, 200, 200, 255);
    }
    /* store cancel button rect at module level for hit-testing */
    g_key_cancel_btn = (SDL_Rect){cbx, cby, cbw, cbh};
}

/* ── Icons ────────────────────────────────────────────────────────────── */
static void icon_play(SDL_Renderer *r, int cx, int cy, int sz)
{
    for (int row = 0; row < sz; row++) {
        float t = (float)row / sz;
        int half_span = (int)((row < sz/2 ? t : (1.0f-t)) * sz * 0.55f);
        SDL_RenderDrawLine(r,
            cx - sz/4, cy - sz/2 + row,
            cx - sz/4 + half_span * 2, cy - sz/2 + row);
    }
}

static void icon_pause(SDL_Renderer *r, int cx, int cy, int sz)
{
    int bw = sz/5, bh = (sz*3)/4, gap = sz/5;
    frect(r, cx - gap/2 - bw, cy - bh/2, bw, bh);
    frect(r, cx + gap/2,       cy - bh/2, bw, bh);
}

static void icon_prev(SDL_Renderer *r, int cx, int cy, int sz)
{
    int bw = sz/6, bh = (sz*3)/4;
    frect(r, cx - sz/3 - 1, cy - bh/2, bw, bh);
    for (int row = 0; row < bh; row++) {
        float t = (float)(row < bh/2 ? row : bh-1-row) / (bh/2);
        int span = (int)(t * sz * 0.28f);
        SDL_RenderDrawLine(r, cx - sz/3 + bw, cy - bh/2 + row,
                              cx - sz/3 + bw + span, cy - bh/2 + row);
    }
    for (int row = 0; row < bh; row++) {
        float t = (float)(row < bh/2 ? row : bh-1-row) / (bh/2);
        int span = (int)(t * sz * 0.28f);
        SDL_RenderDrawLine(r, cx, cy - bh/2 + row, cx + span, cy - bh/2 + row);
    }
}

static void icon_next(SDL_Renderer *r, int cx, int cy, int sz)
{
    int bw = sz/6, bh = (sz*3)/4;
    frect(r, cx + sz/3 - bw + 1, cy - bh/2, bw, bh);
    for (int row = 0; row < bh; row++) {
        float t = (float)(row < bh/2 ? row : bh-1-row) / (bh/2);
        int span = (int)(t * sz * 0.28f);
        SDL_RenderDrawLine(r, cx, cy - bh/2 + row, cx + span, cy - bh/2 + row);
    }
    for (int row = 0; row < bh; row++) {
        float t = (float)(row < bh/2 ? row : bh-1-row) / (bh/2);
        int span = (int)(t * sz * 0.28f);
        SDL_RenderDrawLine(r, cx + sz/3 - bw + 1 + bw, cy - bh/2 + row,
                              cx + sz/3 - bw + 1 + bw - span, cy - bh/2 + row);
    }
}

static void icon_loop(SDL_Renderer *r, int cx, int cy, int sz)
{
    int r2 = sz/3;
    for (int a = 0; a < 300; a++) {
        float rad = (float)a * 3.14159f / 180.0f;
        int px = cx + (int)(r2 * cos(rad));
        int py = cy + (int)(r2 * sin(rad));
        SDL_RenderDrawPoint(r, px, py);
    }
    SDL_RenderDrawLine(r, cx - r2 - 2, cy - 4, cx - r2 + 4, cy);
    SDL_RenderDrawLine(r, cx - r2 - 2, cy + 4, cx - r2 + 4, cy);
}

static void icon_volume(SDL_Renderer *r, int cx, int cy, int sz, int muted)
{
    frect(r, cx - sz/3, cy - sz/6, sz/5, sz/3);
    for (int row = 0; row < sz/2; row++) {
        float t = (float)row / (sz/2);
        int span = (int)(t * sz * 0.2f);
        SDL_RenderDrawLine(r,
            cx - sz/3 + sz/5, cy - sz/4 + row,
            cx - sz/3 + sz/5 + span, cy - sz/4 + row);
    }
    if (!muted) {
        for (int wave = 1; wave <= 2; wave++) {
            int wr = sz/5 * wave;
            for (int a = -50; a <= 50; a++) {
                float rad = (float)a * 3.14159f / 180.0f;
                int px = cx - sz/10 + (int)(wr * cos(rad));
                int py = cy          + (int)(wr * sin(rad));
                SDL_RenderDrawPoint(r, px, py);
            }
        }
    } else {
        SDL_RenderDrawLine(r, cx,      cy - sz/5, cx + sz/4, cy + sz/5);
        SDL_RenderDrawLine(r, cx,      cy + sz/5, cx + sz/4, cy - sz/5);
    }
}

static void icon_fullscreen(SDL_Renderer *r, int cx, int cy, int sz, int fs)
{
    int m = sz/3;
    if (!fs) {
        SDL_RenderDrawLine(r, cx-m, cy-m, cx-m+4, cy-m);
        SDL_RenderDrawLine(r, cx-m, cy-m, cx-m,   cy-m+4);
        SDL_RenderDrawLine(r, cx+m, cy-m, cx+m-4, cy-m);
        SDL_RenderDrawLine(r, cx+m, cy-m, cx+m,   cy-m+4);
        SDL_RenderDrawLine(r, cx-m, cy+m, cx-m+4, cy+m);
        SDL_RenderDrawLine(r, cx-m, cy+m, cx-m,   cy+m-4);
        SDL_RenderDrawLine(r, cx+m, cy+m, cx+m-4, cy+m);
        SDL_RenderDrawLine(r, cx+m, cy+m, cx+m,   cy+m-4);
    } else {
        SDL_RenderDrawLine(r, cx-4, cy-m, cx,   cy-m+4);
        SDL_RenderDrawLine(r, cx-4, cy-m, cx-m, cy-m+4);
        SDL_RenderDrawLine(r, cx+4, cy+m, cx,   cy+m-4);
        SDL_RenderDrawLine(r, cx+4, cy+m, cx+m, cy+m-4);
    }
}

static void icon_menu(SDL_Renderer *r, int cx, int cy, int sz)
{
    int gap = sz/4;
    for (int i = -1; i <= 1; i++)
        fcircle(r, cx, cy + i * gap, 2);
}

static void fmt_time(char *buf, float s)
{
    if (s < 0) s = 0;
    int t = (int)s;
    snprintf(buf, 12, "%02d:%02d", t/60, t%60);
}

/* ── Lifecycle ────────────────────────────────────────────────────────── */
int bv_ui_init(const char *font_path)
{
    if (g_init) return 0;
    if (TTF_Init() < 0) return -1;
    if (font_path && font_path[0]) {
        g_font_time = TTF_OpenFont(font_path, TIME_FONT_SZ);
        g_font_ui   = TTF_OpenFont(font_path, CTX_FONT_SZ);
        g_font_sub  = TTF_OpenFont(font_path, 22);
        g_font_big  = TTF_OpenFont(font_path, 18);
    }
    memset(g_sub, 0, sizeof(g_sub));
    g_init = 1;
    return 0;
}

void bv_ui_quit(void)
{
    if (!g_init) return;
    if (g_font_time) { TTF_CloseFont(g_font_time); g_font_time = NULL; }
    if (g_font_ui)   { TTF_CloseFont(g_font_ui);   g_font_ui   = NULL; }
    if (g_font_sub)  { TTF_CloseFont(g_font_sub);  g_font_sub  = NULL; }
    if (g_font_big)  { TTF_CloseFont(g_font_big);  g_font_big  = NULL; }
    TTF_Quit();
    g_init = 0;
}

void bv_ui_set_window_size(int w, int h) { g_win_w = w; g_win_h = h; }

void bv_ui_set_subtitles(const char *p0, const char *p1, const char *p2,
                          int count, int active)
{
    strncpy(g_sub[0], p0 ? p0 : "", 511); g_sub[0][511] = 0;
    strncpy(g_sub[1], p1 ? p1 : "", 511); g_sub[1][511] = 0;
    strncpy(g_sub[2], p2 ? p2 : "", 511); g_sub[2][511] = 0;
    g_sub_count  = count > 3 ? 3 : count;
    g_sub_active = active;
}

void bv_ui_set_sub_text(const char *text)
{
    if (!text) { g_sub_text[0] = 0; return; }
    strncpy(g_sub_text, text, sizeof(g_sub_text)-1);
    g_sub_text[sizeof(g_sub_text)-1] = 0;
}

void bv_ui_set_whisper_status(const char *status)
{
    if (!status) { g_whisper_status[0] = 0; return; }
    strncpy(g_whisper_status, status, sizeof(g_whisper_status)-1);
    g_whisper_status[sizeof(g_whisper_status)-1] = 0;
}

void bv_ui_set_no_video(int active) { g_no_video = active; }

void bv_ui_set_thumb_preview(const char *path, float ts)
{
    bv_tp_set_preview(path, ts);
}

void bv_ui_set_update_available(int avail, const char *version)
{
    g_update_available = avail;
    if (version) {
        strncpy(g_update_version, version, sizeof(g_update_version)-1);
        g_update_version[sizeof(g_update_version)-1] = 0;
    }
}

void bv_ui_show_update_overlay(const char *version,
                                const char *notes,
                                const char *url)
{
    g_show_update_overlay     = 1;
    g_update_download_clicked = 0;
    g_update_later_clicked    = 0;
    if (version) strncpy(g_update_version, version, sizeof(g_update_version)-1);
    if (notes)   strncpy(g_update_notes,   notes,   sizeof(g_update_notes)-1);
    if (url)     strncpy(g_update_dl_url,  url,     sizeof(g_update_dl_url)-1);
}

void bv_ui_hide_update_overlay(void)
{
    g_show_update_overlay     = 0;
    g_update_download_clicked = 0;
    g_update_later_clicked    = 0;
}

int bv_ui_update_download_clicked(void)
{
    int v = g_update_download_clicked;
    g_update_download_clicked = 0;
    return v;
}

int bv_ui_update_later_clicked(void)
{
    int v = g_update_later_clicked;
    g_update_later_clicked = 0;
    return v;
}

void bv_ui_show_key_input(const char *provider)
{
    g_show_key_input      = 1;
    g_key_input_submitted = 0;
    g_key_cancelled       = 0;
    g_key_value[0]        = '\0';
    g_key_value[0]        = '\0';
    if (provider) strncpy(g_key_provider, provider, sizeof(g_key_provider)-1);
    SDL_StartTextInput();
}

void bv_ui_hide_key_input(void)
{
    g_show_key_input      = 0;
    g_key_input_submitted = 0;
    SDL_StopTextInput();
}

int bv_ui_key_input_submitted(void)
{
    int v = g_key_input_submitted;
    g_key_input_submitted = 0;
    return v;
}

const char *bv_ui_key_input_value(void)  { return g_key_value; }
int         bv_ui_key_input_visible(void)   { return g_show_key_input; }

/* Paste clipboard text into the key input field (called on Ctrl+V) */
void bv_ui_paste_clipboard(void)
{
    if (!g_show_key_input) return;
    if (!SDL_HasClipboardText()) return;
    char *text = SDL_GetClipboardText();
    if (!text) return;
    /* Strip whitespace/newlines that often appear when copying keys */
    int src = 0, dst = 0;
    char clean[256];
    while (text[src] && dst < (int)sizeof(clean) - 1) {
        unsigned char c = (unsigned char)text[src];
        if (c >= 0x20 && c != 0x7F)   /* keep printable ASCII + UTF-8 */
            clean[dst++] = text[src];
        src++;
    }
    clean[dst] = '\0';
    SDL_free(text);
    /* Replace current value entirely with pasted text */
    strncpy(g_key_value, clean, sizeof(g_key_value) - 1);
    g_key_value[sizeof(g_key_value) - 1] = '\0';
}

/* Clear the key input field (called on Ctrl+A or select-all) */
void bv_ui_clear_key_input(void)
{
    if (!g_show_key_input) return;
    g_key_value[0] = '\0';
}
int         bv_ui_key_input_cancelled(void) {
    int v = g_key_cancelled; g_key_cancelled = 0; return v; }

/* ── SDL event integration for overlays ──────────────────────────────── */
/* Called by Ada for SDL_TEXTINPUT events while key input overlay is open */
void bv_ui_handle_text_input(const char *text)
{
    if (!g_show_key_input || !text) return;
    int cur = (int)strlen(g_key_value);
    strncat(g_key_value, text, sizeof(g_key_value) - cur - 1);
}

void bv_ui_handle_key_backspace(void)
{
    if (!g_show_key_input) return;
    int len = (int)strlen(g_key_value);
    if (len > 0) g_key_value[len-1] = '\0';
}

void bv_ui_handle_click(int x, int y)
{
    /* Update overlay buttons */
    if (g_show_update_overlay) {
        SDL_Point p = {x, y};
        if (SDL_PointInRect(&p, &g_update_btn_dl))
            g_update_download_clicked = 1;
        if (SDL_PointInRect(&p, &g_update_btn_late))
            g_update_later_clicked = 1;
        return;
    }
    /* Key input overlay: OK, Close(X), Cancel, or click-outside */
    if (g_show_key_input) {
        SDL_Point p = {x, y};
        if (SDL_PointInRect(&p, &g_key_ok_btn)) {
            g_key_input_submitted = 1;
            return;
        }
        if (SDL_PointInRect(&p, &g_key_close_btn) ||
            SDL_PointInRect(&p, &g_key_cancel_btn)) {
            g_key_cancelled   = 1;
            g_show_key_input  = 0;
            g_key_value[0]    = '\0';
            return;
        }
        /* Click anywhere outside dialog box dismisses it too */
        int ow = 420, oh = 160;
        int ox = g_win_w/2 - ow/2;
        int oy = g_win_h/2 - oh/2;
        SDL_Rect dialog = {ox, oy, ow, oh};
        if (!SDL_PointInRect(&p, &dialog)) {
            g_key_cancelled  = 1;
            g_show_key_input = 0;
            g_key_value[0]   = '\0';
        }
        return;
    }
}

/* ── bv_ui_draw ───────────────────────────────────────────────────────── */
void bv_ui_draw(SDL_Renderer *rend,
                float position, float duration,
                int playing, int looping, int muted,
                int volume,  int fullscreen, int speed_idx,
                int visible)
{
    if (!rend) return;

    /* Welcome screen — replaces everything */
    if (g_no_video) {
        draw_welcome_screen(rend);
        if (g_show_key_input)    draw_key_input_overlay(rend);
        if (g_show_update_overlay) draw_update_overlay(rend);
        return;
    }

    /* Subtitle text overlay */
    g_bar_y = g_win_h - BAR_H;
    if (g_sub_active >= 0 && g_sub_text[0])
        draw_subtitle_overlay(rend);

    /* Context menu */
    if (g_ctx_open) {
        int n  = CTX_COUNT;
        int mh = n * CTX_ITEM_H + 8;
        int mx = g_ctx_x;
        int my = g_ctx_y;
        if (mx + CTX_W > g_win_w) mx = g_win_w - CTX_W - 4;
        if (my + mh    > g_win_h) my = g_win_h - mh    - 4;

        col(rend, C_CTX_BG);  frect(rend, mx, my, CTX_W, mh);
        col(rend, C_CTX_BORDER);
        SDL_Rect border = {mx, my, CTX_W, mh};
        SDL_RenderDrawRect(rend, &border);

        /* Dynamic subtitle state label */
        char sub_state_lbl[48];
        if (g_sub_active >= 0 && g_sub_active < g_sub_count)
            snprintf(sub_state_lbl, sizeof(sub_state_lbl),
                     "Subtitle: Track %d", g_sub_active + 1);
        else
            snprintf(sub_state_lbl, sizeof(sub_state_lbl), "Subtitle: Off");

        /* Per-track labels */
        static char slbl[3][80];
        for (int i = 0; i < 3; i++) {
            if (i < g_sub_count) {
                const char *p = g_sub[i], *bn = p;
                while (*p) { if (*p=='/'||*p=='\\') bn=p+1; p++; }
                snprintf(slbl[i], 79, "Track %d: %.40s", i+1, bn);
            } else {
                snprintf(slbl[i], 79, "Track %d: (load file)", i+1);
            }
        }

        for (int i = 0; i < n; i++) {
            int iy = my + 4 + i * CTX_ITEM_H;

            /* Hover highlight */
            if (i == g_hover_ctx) {
                col(rend, C_CTX_HOVER);
                frect(rend, mx+2, iy, CTX_W-4, CTX_ITEM_H);
            }

            /* Separators */
            if (i == CTX_SUB_NONE || i == CTX_WHISPER_GEN ||
                i == CTX_LLM_CLAUDE || i == CTX_CLEAR_THUMB_CACHE ||
                i == CTX_CHECK_UPDATES)
            {
                col(rend, C_CTX_SEP);
                SDL_RenderDrawLine(rend, mx+8, iy, mx+CTX_W-8, iy);
            }

            /* Label and colour */
            const char *lbl = NULL;
            Uint8 tr = 205, tg = 205, tb = 205;

            switch (i) {
                case CTX_OPEN_FILE:
                    lbl = "Open Video File...";
                    break;
                case CTX_SUB_NONE:
                    lbl = sub_state_lbl;
                    if (g_sub_active >= 0) { tr=210; tg=45; tb=45; }
                    break;
                case CTX_SUB_1: case CTX_SUB_2: case CTX_SUB_3: {
                    int si = i - CTX_SUB_1;
                    lbl = slbl[si];
                    int is_active = (si == g_sub_active);
                    int disabled  = (si >= g_sub_count);
                    tr = disabled ? 85 : is_active ? 210 : 205;
                    tg = disabled ? 85 : is_active ?  45 : 205;
                    tb = disabled ? 85 : is_active ?  45 : 205;
                    if (is_active) {
                        col(rend, C_CTX_ACTIVE);
                        fcircle(rend, mx+8, iy+CTX_ITEM_H/2, 3);
                    }
                    break;
                }
                case CTX_WHISPER_GEN:
                    lbl = "Generate Captions (Whisper)";
                    tr=80; tg=160; tb=220;
                    break;
                case CTX_WHISPER_TRANS:
                    lbl = "Translate to English (Whisper)";
                    tr=80; tg=160; tb=220;
                    break;
                case CTX_LLM_CLAUDE:
                    lbl = "Claude (Anthropic)";
                    tr=120; tg=200; tb=140;
                    break;
                case CTX_LLM_OPENAI:
                    lbl = "OpenAI";
                    tr=120; tg=200; tb=140;
                    break;
                case CTX_LLM_GEMINI:
                    lbl = "Gemini (Google)";
                    tr=120; tg=200; tb=140;
                    break;
                case CTX_LLM_DEEPSEEK:
                    lbl = "DeepSeek";
                    tr=120; tg=200; tb=140;
                    break;
                case CTX_LLM_GROK:
                    lbl = "Grok (xAI)";
                    tr=120; tg=200; tb=140;
                    break;
                case CTX_CLEAR_THUMB_CACHE:
                    lbl = "Clear Thumbnail Cache";
                    break;
                case CTX_CHECK_UPDATES:
                    lbl = "Check for Updates...";
                    break;
            }

            if (lbl)
                draw_text(rend, g_font_ui, lbl,
                          mx + CTX_PAD_X,
                          iy + (CTX_ITEM_H - CTX_FONT_SZ) / 2,
                          tr, tg, tb, 255);
        }
    }

    if (!visible) goto draw_overlays;

    /* ── Control bar ──────────────────────────────────────────────────── */
    g_bar_y = g_win_h - BAR_H;
    col(rend, C_BAR_BG);
    frect(rend, 0, g_bar_y, g_win_w, BAR_H);

    g_sk_x = SEEK_MARGIN;
    g_sk_y = g_bar_y + SEEK_TOP;
    g_sk_w = g_win_w - SEEK_MARGIN * 2;

    col(rend, C_SEEK_TRACK);
    frect(rend, g_sk_x, g_sk_y, g_sk_w, SEEK_H);

    float frac = (duration > 0.0f) ? (position / duration) : 0.0f;
    if (frac < 0.0f) frac = 0.0f;
    if (frac > 1.0f) frac = 1.0f;
    int fill_w = (int)(g_sk_w * frac);
    col(rend, C_SEEK_FILL);
    frect(rend, g_sk_x, g_sk_y, fill_w, SEEK_H);

    int knob_x = g_sk_x + fill_w;
    int knob_y = g_sk_y + SEEK_H / 2;
    col(rend, C_KNOB);
    fcircle(rend, knob_x, knob_y, KNOB_R);

    /* Thumbnail preview above seek bar */
    bv_tp_draw(rend, knob_x, g_sk_y, g_font_time);

    /* Timestamps */
    char t_pos[12], t_dur[12];
    fmt_time(t_pos, position);
    fmt_time(t_dur, duration);
    int ty = g_sk_y + SEEK_H + 6;
    col(rend, C_TIME);
    draw_text(rend, g_font_time, t_pos, g_sk_x, ty, C_TIME);
    if (g_font_time) {
        int tw = text_width(g_font_time, t_dur);
        draw_text(rend, g_font_time, t_dur,
                  g_win_w - SEEK_MARGIN - tw, ty, C_TIME);
    }

    /* Buttons */
    int left_start  = BTN_W;
    int right_start = g_win_w - BTN_W * 4 - BTN_W;
    int by = g_bar_y + BTN_ROW_TOP;
    for (int i = 0; i < 4; i++) {
        g_btn[i].x = left_start  + i * BTN_W;
        g_btn[i].y = by; g_btn[i].w = BTN_W; g_btn[i].h = BTN_H;
    }
    for (int i = 4; i < BTN_COUNT; i++) {
        g_btn[i].x = right_start + (i-4) * BTN_W;
        g_btn[i].y = by; g_btn[i].w = BTN_W; g_btn[i].h = BTN_H;
    }

    for (int i = 0; i < BTN_COUNT; i++) {
        int cx = g_btn[i].x + BTN_W/2;
        int cy = g_btn[i].y + BTN_H/2;

        if (i == g_hover_btn) {
            col(rend, C_BTN_HOVER);
            frect(rend, g_btn[i].x, g_btn[i].y, BTN_W, BTN_H);
        }

        int loop_on = looping && (i == BTN_LOOP);
        int mute_on = muted   && (i == BTN_VOLUME);
        Uint8 ir = 210, ig = (loop_on || mute_on) ? 45 : 210;
        Uint8 ib = (loop_on || mute_on) ? 45 : 210;
        col(rend, ir, ig, ib, 255);

        switch (i) {
            case BTN_PREV:       icon_prev      (rend, cx, cy, ICON_SZ); break;
            case BTN_PLAYPAUSE:
                if (playing)     icon_pause     (rend, cx, cy, ICON_SZ);
                else             icon_play      (rend, cx, cy, ICON_SZ);
                break;
            case BTN_NEXT:       icon_next      (rend, cx, cy, ICON_SZ); break;
            case BTN_LOOP:       icon_loop      (rend, cx, cy, ICON_SZ); break;
            case BTN_VOLUME:     icon_volume    (rend, cx, cy, ICON_SZ, muted); break;
            case BTN_FULLSCREEN: icon_fullscreen(rend, cx, cy, ICON_SZ, fullscreen); break;
            case BTN_MENU:
                icon_menu(rend, cx, cy, ICON_SZ);
                /* Update badge — red dot on top-right corner of menu button */
                if (g_update_available) {
                    col(rend, C_BADGE);
                    fcircle(rend, g_btn[i].x + BTN_W - 5, g_btn[i].y + 5, 4);
                }
                break;
            case BTN_SPEED: {
                static const char *spd[] = {"0.5x","1.0x","1.5x","2.0x"};
                int si = (speed_idx >= 0 && speed_idx < 4) ? speed_idx : 1;
                draw_text(rend, g_font_ui, spd[si],
                          cx - 14, cy - CTX_FONT_SZ/2, C_TIME);
                break;
            }
        }
    }

    /* Status banner with optional progress bar */
    if (g_whisper_status[0]) {
        int sx = g_win_w/2 - 140;
        int st = g_bar_y + 4;
        if (g_font_time) {
            draw_text(rend, g_font_time, g_whisper_status,
                      sx, st, C_STATUS);
        }
        int pct = parse_pct(g_whisper_status);
        if (pct >= 0) {
            int px = sx + text_width(g_font_time, g_whisper_status) + 8;
            if (px + PROG_BAR_W < g_win_w - 8)
                draw_progress_bar(rend, px, st + 3, pct);
        }
    }

draw_overlays:
    if (g_show_update_overlay) draw_update_overlay(rend);
    if (g_show_key_input)      draw_key_input_overlay(rend);
}

/* ── Hit-testing ──────────────────────────────────────────────────────── */
int bv_ui_hit_seek(int x, int y)
{
    if (y < g_sk_y - KNOB_R || y > g_sk_y + SEEK_H + KNOB_R) return 0;
    return (x >= g_sk_x && x <= g_sk_x + g_sk_w) ? 1 : 0;
}

float bv_ui_seek_fraction(int x)
{
    if (g_sk_w <= 0) return 0.0f;
    float f = (float)(x - g_sk_x) / g_sk_w;
    if (f < 0.0f) f = 0.0f;
    if (f > 1.0f) f = 1.0f;
    return f;
}

int bv_ui_hit_button(int x, int y)
{
    for (int i = 0; i < BTN_COUNT; i++) {
        if (x >= g_btn[i].x && x < g_btn[i].x + g_btn[i].w &&
            y >= g_btn[i].y && y < g_btn[i].y + g_btn[i].h)
            return i;
    }
    return -1;
}

int bv_ui_in_bar(int x, int y)
{
    (void)x;
    return y >= g_bar_y;
}

void bv_ui_set_hover_btn(int b) { g_hover_btn = b; }
void bv_ui_set_seeking  (int s) { g_seeking   = s; }
int  bv_ui_is_seeking   (void)  { return g_seeking; }

void bv_ui_open_ctx(int x, int y)
{
    g_ctx_open  = 1;
    g_ctx_x     = x;
    g_ctx_y     = y;
    g_hover_ctx = -1;
}

void bv_ui_close_ctx(void) { g_ctx_open = 0; g_hover_ctx = -1; }
int  bv_ui_ctx_open (void) { return g_ctx_open; }

int bv_ui_hit_ctx(int x, int y)
{
    if (!g_ctx_open) return -1;
    int mh = CTX_COUNT * CTX_ITEM_H + 8;
    int mx = g_ctx_x, my = g_ctx_y;
    if (mx + CTX_W > g_win_w) mx = g_win_w - CTX_W - 4;
    if (my + mh    > g_win_h) my = g_win_h - mh    - 4;
    if (x<mx || x>mx+CTX_W || y<my || y>my+mh) return -1;
    int idx = (y - my - 4) / CTX_ITEM_H;
    if (idx < 0 || idx >= CTX_COUNT) return -1;
    g_hover_ctx = idx;
    return idx;
}

/* ── bv_sdl_drop_file ───────────────────────────────────────────────────── */
/*  Safely reads SDL_DropEvent.drop.file via SDL's own struct — correct on   */
/*  both 32-bit and 64-bit builds. SDL owns the memory; do NOT SDL_free it.  */
const char *bv_sdl_drop_file(void *event_ptr)
{
    if (!event_ptr) return NULL;
    SDL_Event *ev = (SDL_Event *)event_ptr;
    if (ev->type != SDL_DROPFILE) return NULL;
    return ev->drop.file;
}
