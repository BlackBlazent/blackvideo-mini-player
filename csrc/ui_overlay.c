/*
 * ui_overlay.c  —  BlackVideo Mini Player UI Overlay  v2.3
 *
 * Draws the control bar + context menu + subtitle overlay into the SDL renderer.
 * Ada owns ALL state and logic. This file is pure drawing + hit-testing.
 *
 * v2.3 changes:
 *   - Context menu: "Subtitle: Off" label now updates to "Subtitle: Track N"
 *     when a track is active, so the user can see what is loaded.
 *   - Added CTX items for Whisper: Generate Captions, Translate to English.
 *   - Added g_whisper_status string displayed in the bar while Whisper runs.
 *   - Subtitle text overlay renders current SRT cue at bottom of video.
 *   - bv_ui_set_sub_text() lets Ada push the current cue string.
 */

#include <SDL.h>
#include <SDL_ttf.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

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
#define CTX_W         260
#define CTX_ITEM_H    30
#define CTX_PAD_X     14
#define CTX_FONT_SZ   13

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
#define C_SUB_TEXT    255, 255, 255, 255
#define C_SUB_SHADOW  0,   0,   0, 200
#define C_STATUS      180, 200, 255, 255

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
/* Items 0-4: standard controls  */
/* Items 5-6: Whisper actions    */
#define CTX_OPEN_FILE      0
#define CTX_SUB_NONE       1
#define CTX_SUB_1          2
#define CTX_SUB_2          3
#define CTX_SUB_3          4
#define CTX_WHISPER_GEN    5   /* Generate captions for this video */
#define CTX_WHISPER_TRANS  6   /* Translate + generate (-> English) */
#define CTX_COUNT          7

/* ── State ────────────────────────────────────────────────────────────── */
static TTF_Font *g_font_time = NULL;
static TTF_Font *g_font_ui   = NULL;
static TTF_Font *g_font_sub  = NULL;   /* larger font for subtitle overlay */
static int       g_init      = 0;
static int       g_win_w     = 1280;
static int       g_win_h     = 720;

static int g_bar_y   = 0;
static int g_sk_x    = 0, g_sk_y = 0, g_sk_w = 0;
static SDL_Rect g_btn[BTN_COUNT];

static int g_hover_btn = -1;
static int g_seeking   = 0;

static int  g_ctx_open  = 0;
static int  g_ctx_x     = 0, g_ctx_y = 0;
static int  g_hover_ctx = -1;

/* Subtitles */
static char g_sub[3][512];
static int  g_sub_count  = 0;
static int  g_sub_active = -1;

/* Current subtitle cue text (pushed by Ada SRT parser) */
static char g_sub_text[1024] = "";

/* Whisper status line (shown in the control bar) */
static char g_whisper_status[256] = "";

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

/* Draw subtitle text with a semi-transparent drop-shadow for readability */
static void draw_subtitle_overlay(SDL_Renderer *r)
{
    if (!g_sub_text[0]) return;
    if (!g_font_sub)    return;

    SDL_Color white = {255, 255, 255, 255};
    SDL_Surface *surf = TTF_RenderUTF8_Blended(g_font_sub, g_sub_text, white);
    if (!surf) return;
    SDL_Texture *tex = SDL_CreateTextureFromSurface(r, surf);
    int tw = surf->w, th = surf->h;
    SDL_FreeSurface(surf);
    if (!tex) return;

    /* Position: centred horizontally, just above the control bar */
    int x = (g_win_w - tw) / 2;
    int y = g_bar_y - th - 10;
    if (y < 4) y = 4;

    /* Shadow box */
    SDL_SetRenderDrawBlendMode(r, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(r, 0, 0, 0, 160);
    SDL_Rect shadow = {x - 6, y - 3, tw + 12, th + 6};
    SDL_RenderFillRect(r, &shadow);

    /* Text */
    SDL_Rect dst = {x, y, tw, th};
    SDL_RenderCopy(r, tex, NULL, &dst);
    SDL_DestroyTexture(tex);
}

/* ── Icon drawing ─────────────────────────────────────────────────────── */
static void icon_play(SDL_Renderer *r, int cx, int cy, int sz)
{
    for (int row = 0; row < sz; row++) {
        float t = (float)row / sz;
        int half_span = (int)((row < sz/2 ? t : (1.0f - t)) * sz * 0.55f);
        SDL_RenderDrawLine(r,
            cx - sz/4, cy - sz/2 + row,
            cx - sz/4 + half_span * 2, cy - sz/2 + row);
    }
}

static void icon_pause(SDL_Renderer *r, int cx, int cy, int sz)
{
    int bw = sz / 5, bh = (sz * 3) / 4, gap = sz / 5;
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
    int r2 = sz / 3;
    for (int a = 0; a < 300; a++) {
        float rad = (float)a * 3.14159f / 180.0f;
        int px = cx + (int)(r2 * cos(rad));
        int py = cy + (int)(r2 * sin(rad));
        SDL_RenderDrawPoint(r, px, py);
    }
    /* arrowhead */
    SDL_RenderDrawLine(r, cx - r2 - 2, cy - 4, cx - r2 + 4, cy);
    SDL_RenderDrawLine(r, cx - r2 - 2, cy + 4, cx - r2 + 4, cy);
}

static void icon_volume(SDL_Renderer *r, int cx, int cy, int sz, int muted)
{
    /* speaker body */
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
        /* X cross for muted */
        SDL_RenderDrawLine(r, cx,      cy - sz/5, cx + sz/4, cy + sz/5);
        SDL_RenderDrawLine(r, cx,      cy + sz/5, cx + sz/4, cy - sz/5);
    }
}

static void icon_fullscreen(SDL_Renderer *r, int cx, int cy, int sz, int fs)
{
    int m = sz/3;
    if (!fs) {
        /* expand arrows at corners */
        SDL_RenderDrawLine(r, cx-m, cy-m, cx-m+4, cy-m);
        SDL_RenderDrawLine(r, cx-m, cy-m, cx-m,   cy-m+4);
        SDL_RenderDrawLine(r, cx+m, cy-m, cx+m-4, cy-m);
        SDL_RenderDrawLine(r, cx+m, cy-m, cx+m,   cy-m+4);
        SDL_RenderDrawLine(r, cx-m, cy+m, cx-m+4, cy+m);
        SDL_RenderDrawLine(r, cx-m, cy+m, cx-m,   cy+m-4);
        SDL_RenderDrawLine(r, cx+m, cy+m, cx+m-4, cy+m);
        SDL_RenderDrawLine(r, cx+m, cy+m, cx+m,   cy+m-4);
    } else {
        /* compress arrows */
        SDL_RenderDrawLine(r, cx-4, cy-m, cx,   cy-m+4);
        SDL_RenderDrawLine(r, cx-4, cy-m, cx-m, cy-m+4);
        SDL_RenderDrawLine(r, cx+4, cy+m, cx,   cy+m-4);
        SDL_RenderDrawLine(r, cx+4, cy+m, cx+m, cy+m-4);
    }
}

static void icon_menu(SDL_Renderer *r, int cx, int cy, int sz)
{
    int gap = sz / 4;
    for (int i = -1; i <= 1; i++) {
        int py = cy + i * gap;
        fcircle(r, cx, py, 2);
    }
}

/* ── Format time mm:ss ────────────────────────────────────────────────── */
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
        g_font_sub  = TTF_OpenFont(font_path, 22);   /* subtitle overlay */
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

/* Push current subtitle cue text from the Ada SRT parser */
void bv_ui_set_sub_text(const char *text)
{
    if (!text) { g_sub_text[0] = 0; return; }
    strncpy(g_sub_text, text, sizeof(g_sub_text)-1);
    g_sub_text[sizeof(g_sub_text)-1] = 0;
}

/* Push Whisper status string ("Generating... 42%" or "" when done) */
void bv_ui_set_whisper_status(const char *status)
{
    if (!status) { g_whisper_status[0] = 0; return; }
    strncpy(g_whisper_status, status, sizeof(g_whisper_status)-1);
    g_whisper_status[sizeof(g_whisper_status)-1] = 0;
}

/* ── bv_ui_draw ───────────────────────────────────────────────────────── */
void bv_ui_draw(SDL_Renderer *rend,
                float position, float duration,
                int playing, int looping, int muted,
                int volume,  int fullscreen, int speed_idx,
                int visible)
{
    if (!rend) return;

    /* ── Subtitle text overlay (always shown when active) ─────────────── */
    if (g_sub_active >= 0 && g_sub_text[0])
        draw_subtitle_overlay(rend);

    /* ── Context menu (always drawn if open, bar may be hidden) ────────── */
    if (g_ctx_open) {
        int n     = CTX_COUNT;
        int mh    = n * CTX_ITEM_H + 8;
        int mx    = g_ctx_x;
        int my    = g_ctx_y;
        if (mx + CTX_W > g_win_w) mx = g_win_w - CTX_W - 4;
        if (my + mh    > g_win_h) my = g_win_h - mh    - 4;

        col(rend, C_CTX_BG);  frect(rend, mx, my, CTX_W, mh);
        col(rend, C_CTX_BORDER);
        SDL_Rect border = {mx, my, CTX_W, mh};
        SDL_RenderDrawRect(rend, &border);

        /* "Subtitle: Off" label updates to "Subtitle: Track N" when active */
        char sub_state_lbl[48];
        if (g_sub_active >= 0 && g_sub_active < g_sub_count)
            snprintf(sub_state_lbl, sizeof(sub_state_lbl),
                     "Subtitle: Track %d", g_sub_active + 1);
        else
            snprintf(sub_state_lbl, sizeof(sub_state_lbl), "Subtitle: Off");

        /* Build per-track labels */
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

        /* Whisper labels */
        static const char *whisper_lbl[2] = {
            "Generate Captions (Whisper)",
            "Translate to English (Whisper)"
        };

        for (int i = 0; i < n; i++) {
            int iy = my + 4 + i * CTX_ITEM_H;

            /* Hover highlight */
            if (i == g_hover_ctx) {
                col(rend, C_CTX_HOVER);
                frect(rend, mx+2, iy, CTX_W-4, CTX_ITEM_H);
            }

            /* Separators */
            if (i == 1) {
                /* before "Subtitle: ..." */
                col(rend, C_CTX_SEP);
                SDL_RenderDrawLine(rend, mx+8, iy, mx+CTX_W-8, iy);
            }
            if (i == CTX_WHISPER_GEN) {
                /* before Whisper section */
                col(rend, C_CTX_SEP);
                SDL_RenderDrawLine(rend, mx+8, iy, mx+CTX_W-8, iy);
            }

            /* Choose label and colour */
            const char *lbl = NULL;
            Uint8 tr, tg, tb;

            if (i == CTX_OPEN_FILE) {
                lbl = "Open Video File...";
                tr = 205; tg = 205; tb = 205;
            } else if (i == CTX_SUB_NONE) {
                lbl = sub_state_lbl;
                tr = (g_sub_active < 0) ? 205 : 210;
                tg = (g_sub_active < 0) ? 205 :  45;
                tb = (g_sub_active < 0) ? 205 :  45;
            } else if (i >= CTX_SUB_1 && i <= CTX_SUB_3) {
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
            } else if (i == CTX_WHISPER_GEN || i == CTX_WHISPER_TRANS) {
                lbl = whisper_lbl[i - CTX_WHISPER_GEN];
                tr = 80; tg = 160; tb = 220;
            }

            if (lbl)
                draw_text(rend, g_font_ui, lbl,
                          mx + CTX_PAD_X,
                          iy + (CTX_ITEM_H - CTX_FONT_SZ) / 2,
                          tr, tg, tb, 255);
        }
    }

    if (!visible) return;

    /* ── Control bar ─────────────────────────────────────────────────────── */
    g_bar_y = g_win_h - BAR_H;
    col(rend, C_BAR_BG);
    frect(rend, 0, g_bar_y, g_win_w, BAR_H);

    /* Seek bar geometry */
    g_sk_x = SEEK_MARGIN;
    g_sk_y = g_bar_y + SEEK_TOP;
    g_sk_w = g_win_w - SEEK_MARGIN * 2;

    /* Track */
    col(rend, C_SEEK_TRACK);
    frect(rend, g_sk_x, g_sk_y, g_sk_w, SEEK_H);

    /* Fill */
    float frac = (duration > 0.0f) ? (position / duration) : 0.0f;
    if (frac < 0.0f) frac = 0.0f;
    if (frac > 1.0f) frac = 1.0f;
    int fill_w = (int)(g_sk_w * frac);
    col(rend, C_SEEK_FILL);
    frect(rend, g_sk_x, g_sk_y, fill_w, SEEK_H);

    /* Knob */
    int knob_x = g_sk_x + fill_w;
    int knob_y = g_sk_y + SEEK_H / 2;
    col(rend, C_KNOB);
    fcircle(rend, knob_x, knob_y, KNOB_R);

    /* Timestamps */
    char t_pos[12], t_dur[12];
    fmt_time(t_pos, position);
    fmt_time(t_dur, duration);
    int ty = g_sk_y + SEEK_H + 6;
    col(rend, C_TIME);
    draw_text(rend, g_font_time, t_pos, g_sk_x, ty, C_TIME);

    char t_dur_right[12];
    snprintf(t_dur_right, 12, "%s", t_dur);
    if (g_font_time) {
        int tw = 0, th = 0;
        TTF_SizeUTF8(g_font_time, t_dur_right, &tw, &th);
        draw_text(rend, g_font_time, t_dur_right,
                  g_win_w - SEEK_MARGIN - tw, ty, C_TIME);
    }

    /* ── Buttons ──────────────────────────────────────────────────────── */
    static const int btn_x_offsets[] = {
        0, 1, 2, 3,           /* left group: prev / play / next / loop */
        -3, -2, -1, 0         /* right group offsets from right edge   */
    };
    /* Lay out buttons evenly — 4 left, 4 right, centred in bar */
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

        /* Hover highlight */
        if (i == g_hover_btn) {
            col(rend, C_BTN_HOVER);
            frect(rend, g_btn[i].x, g_btn[i].y, BTN_W, BTN_H);
        }

        /* Icon */
        int loop_on = looping && (i == BTN_LOOP);
        int mute_on = muted   && (i == BTN_VOLUME);
        Uint8 ir = loop_on || mute_on ? 210 : 210;
        Uint8 ig = loop_on || mute_on ?  45 : 210;
        Uint8 ib = loop_on || mute_on ?  45 : 210;
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
            case BTN_MENU:       icon_menu      (rend, cx, cy, ICON_SZ); break;
            case BTN_SPEED: {
                static const char *spd[] = {"0.5x","1.0x","1.5x","2.0x"};
                int si = (speed_idx >= 0 && speed_idx < 4) ? speed_idx : 1;
                draw_text(rend, g_font_ui, spd[si],
                          cx - 14, cy - CTX_FONT_SZ/2, C_TIME);
                break;
            }
        }
    }

    /* Whisper status line */
    if (g_whisper_status[0] && g_font_time) {
        draw_text(rend, g_font_time, g_whisper_status,
                  g_win_w/2 - 100, g_bar_y + 4,
                  C_STATUS);
    }
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
