/*
 * ui_overlay.c  —  BlackVideo Mini Player UI Overlay
 *
 * Draws the control bar + context menu into the SDL renderer.
 * Ada owns ALL state and logic. This file is pure drawing + hit-testing.
 * Same pattern as ffmpeg_helpers.c — C handles SDL/TTF types correctly,
 * Ada never touches opaque SDL structs directly.
 *
 * Layout (bottom of window):
 *
 *  ┌──────────────────────────────────────────────────────────────┐
 *  │  00:00 ●━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  01:23   │  ← seek
 *  │  |◄  ►‖  ►|  ↺    ·················    1.0x  🔊  ⛶          │  ← btns
 *  └──────────────────────────────────────────────────────────────┘
 */

/* SDL2 headers.
 * Works with both flat layout (lib/include/SDL.h) and
 * subfolder layout (lib/include/SDL2/SDL.h).
 * build.bat passes -I lib/include and -I lib/include/SDL2
 * so bare includes always resolve correctly. */
#include <SDL.h>
#include <SDL_ttf.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/* ── Layout ───────────────────────────────────────────────────────────── */
#define BAR_H         72     /* total control bar height px               */
#define SEEK_MARGIN   12     /* left/right padding for seek bar           */
#define SEEK_TOP      8      /* seek bar y-offset from bar top            */
#define SEEK_H        5      /* track thickness                           */
#define KNOB_R        7      /* scrubber knob radius                      */
#define BTN_ROW_TOP   30     /* button row y-offset from bar top          */
#define BTN_W         38     /* button hit width                          */
#define BTN_H         34     /* button hit height                         */
#define ICON_SZ       16     /* icon draw size                            */
#define TIME_FONT_SZ  12     /* font size for timestamps                  */
#define CTX_W         230    /* context menu width                        */
#define CTX_ITEM_H    30     /* context menu item height                  */
#define CTX_PAD_X     14     /* context menu text x padding               */
#define CTX_FONT_SZ   13

/* ── Colours (R,G,B,A) ────────────────────────────────────────────────── */
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

/* ── Button indices (must match Ada constants) ────────────────────────── */
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
#define CTX_OPEN_FILE  0
#define CTX_SUB_NONE   1
#define CTX_SUB_1      2
#define CTX_SUB_2      3
#define CTX_SUB_3      4
#define CTX_COUNT      5

/* ── State ────────────────────────────────────────────────────────────── */
static TTF_Font *g_font_time = NULL;  /* small: timestamps          */
static TTF_Font *g_font_ui   = NULL;  /* slightly larger: speed btn */
static int       g_init      = 0;
static int       g_win_w     = 1280;
static int       g_win_h     = 720;

/* Geometry (computed each draw, used for hit-testing between frames) */
static int g_bar_y   = 0;
static int g_sk_x    = 0, g_sk_y = 0, g_sk_w = 0;
static SDL_Rect g_btn[BTN_COUNT];

/* Interaction state */
static int g_hover_btn = -1;
static int g_seeking   = 0;

/* Context menu */
static int  g_ctx_open = 0;
static int  g_ctx_x    = 0, g_ctx_y = 0;
static int  g_hover_ctx = -1;

/* Subtitles */
static char g_sub[3][512];
static int  g_sub_count  = 0;
static int  g_sub_active = -1;

/* ── Draw helpers ─────────────────────────────────────────────────────── */

static void col(SDL_Renderer *r,
                Uint8 R, Uint8 G, Uint8 B, Uint8 A)
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

/* Draw UTF-8 text; return rendered pixel width */
static int draw_text(SDL_Renderer *r, TTF_Font *f,
                     const char *s,
                     int x, int y,
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

/* ── Icon drawing ─────────────────────────────────────────────────────── */

/* ▶ play triangle */
static void icon_play(SDL_Renderer *r, int cx, int cy, int sz)
{
    for (int row = 0; row < sz; row++) {
        float t = (float)row / sz;
        /* expand top half, contract bottom half */
        int half_span = (int)((row < sz/2 ? t : (1.0f - t)) * sz * 0.55f);
        SDL_RenderDrawLine(r,
            cx - sz/4, cy - sz/2 + row,
            cx - sz/4 + half_span * 2, cy - sz/2 + row);
    }
}

/* ⏸ pause double-bar */
static void icon_pause(SDL_Renderer *r, int cx, int cy, int sz)
{
    int bw = sz / 5, bh = (sz * 3) / 4, gap = sz / 5;
    frect(r, cx - gap/2 - bw, cy - bh/2, bw, bh);
    frect(r, cx + gap/2,       cy - bh/2, bw, bh);
}

/* |◄ previous */
static void icon_prev(SDL_Renderer *r, int cx, int cy, int sz)
{
    int bw = sz/6, bh = (sz*3)/4;
    frect(r, cx - sz/3 - 1, cy - bh/2, bw, bh);          /* bar */
    for (int row = 0; row < bh; row++) {                   /* left triangle */
        float t = (float)(row < bh/2 ? row : bh-1-row) / (bh/2);
        int span = (int)(t * sz * 0.28f);
        SDL_RenderDrawLine(r,
            cx - sz/3 + bw, cy - bh/2 + row,
            cx - sz/3 + bw + span, cy - bh/2 + row);
    }
    for (int row = 0; row < bh; row++) {                   /* right triangle*/
        float t = (float)(row < bh/2 ? row : bh-1-row) / (bh/2);
        int span = (int)(t * sz * 0.28f);
        SDL_RenderDrawLine(r,
            cx, cy - bh/2 + row,
            cx + span, cy - bh/2 + row);
    }
}

/* ►| next */
static void icon_next(SDL_Renderer *r, int cx, int cy, int sz)
{
    int bw = sz/6, bh = (sz*3)/4;
    frect(r, cx + sz/3 - bw + 1, cy - bh/2, bw, bh);     /* bar */
    for (int row = 0; row < bh; row++) {                   /* left triangle */
        float t = (float)(row < bh/2 ? row : bh-1-row) / (bh/2);
        int span = (int)(t * sz * 0.28f);
        SDL_RenderDrawLine(r,
            cx - span, cy - bh/2 + row,
            cx,        cy - bh/2 + row);
    }
    for (int row = 0; row < bh; row++) {                   /* right triangle*/
        float t = (float)(row < bh/2 ? row : bh-1-row) / (bh/2);
        int span = (int)(t * sz * 0.28f);
        SDL_RenderDrawLine(r,
            cx + sz/3 - bw + 1 - span, cy - bh/2 + row,
            cx + sz/3 - bw + 1,        cy - bh/2 + row);
    }
}

/* ↺ loop circle with arrows */
static void icon_loop(SDL_Renderer *r, int cx, int cy, int sz)
{
    int R = sz * 5 / 12;
    for (int a = 10; a <= 350; a += 3) {
        float r0 = a       * (float)M_PI / 180.f;
        float r1 = (a + 3) * (float)M_PI / 180.f;
        SDL_RenderDrawLine(r,
            cx + (int)(R * cosf(r0)), cy + (int)(R * sinf(r0)),
            cx + (int)(R * cosf(r1)), cy + (int)(R * sinf(r1)));
    }
    /* arrow head at top-right */
    int ax = cx + R, ay = cy;
    SDL_RenderDrawLine(r, ax - 4, ay - 4, ax + 1, ay);
    SDL_RenderDrawLine(r, ax - 4, ay + 4, ax + 1, ay);
}

/* 🔊 speaker */
static void icon_volume(SDL_Renderer *r, int cx, int cy, int sz, int muted)
{
    int bw = sz/4, bh = sz/2;
    int lx = cx - sz/3;
    frect(r, lx, cy - bh/2, bw, bh);   /* speaker body */
    for (int row = 0; row < bh; row++) {/* cone */
        float t = (float)row / bh;
        float dist = fabsf(t - 0.5f) * 2.f;
        int span = (int)((1.f - dist) * sz * 0.22f);
        SDL_RenderDrawLine(r,
            lx + bw,        cy - bh/2 + row,
            lx + bw + span, cy - bh/2 + row);
    }
    if (muted) {
        int x0 = cx + 2, x1 = cx + sz/4 + 2;
        int y0 = cy - sz/5, y1 = cy + sz/5;
        SDL_RenderDrawLine(r, x0, y0, x1, y1);
        SDL_RenderDrawLine(r, x0, y1, x1, y0);
    } else {
        /* small arc */
        int Rarc = sz * 5 / 14;
        for (int a = -35; a <= 35; a += 4) {
            float rad = a * (float)M_PI / 180.f;
            SDL_RenderDrawLine(r,
                cx + (int)((Rarc-2)*cosf(rad)), cy + (int)((Rarc-2)*sinf(rad)),
                cx + (int)(Rarc    *cosf(rad)), cy + (int)(Rarc    *sinf(rad)));
        }
    }
}

/* ⛶ fullscreen corners */
static void icon_fullscreen(SDL_Renderer *r, int cx, int cy, int sz, int fs)
{
    int m = sz * 5 / 12, a = 5;
    if (!fs) {
        /* expand: four outward corners */
        SDL_RenderDrawLine(r, cx-m, cy-m, cx-m+a, cy-m  );
        SDL_RenderDrawLine(r, cx-m, cy-m, cx-m,   cy-m+a);
        SDL_RenderDrawLine(r, cx+m, cy-m, cx+m-a, cy-m  );
        SDL_RenderDrawLine(r, cx+m, cy-m, cx+m,   cy-m+a);
        SDL_RenderDrawLine(r, cx-m, cy+m, cx-m+a, cy+m  );
        SDL_RenderDrawLine(r, cx-m, cy+m, cx-m,   cy+m-a);
        SDL_RenderDrawLine(r, cx+m, cy+m, cx+m-a, cy+m  );
        SDL_RenderDrawLine(r, cx+m, cy+m, cx+m,   cy+m-a);
    } else {
        /* contract: four inward corners */
        int i = m - a;
        SDL_RenderDrawLine(r, cx-m, cy-m, cx-i, cy-m  );
        SDL_RenderDrawLine(r, cx-m, cy-m, cx-m, cy-i  );
        SDL_RenderDrawLine(r, cx+m, cy-m, cx+i, cy-m  );
        SDL_RenderDrawLine(r, cx+m, cy-m, cx+m, cy-i  );
        SDL_RenderDrawLine(r, cx-m, cy+m, cx-i, cy+m  );
        SDL_RenderDrawLine(r, cx-m, cy+m, cx-m, cy+i  );
        SDL_RenderDrawLine(r, cx+m, cy+m, cx+i, cy+m  );
        SDL_RenderDrawLine(r, cx+m, cy+m, cx+m, cy+i  );
    }
}

/* ⋮ vertical three-dot menu */
static void icon_menu(SDL_Renderer *r, int cx, int cy)
{
    for (int d = -5; d <= 5; d += 5)
        fcircle(r, cx, cy + d, 2);
}

/* ═══════════════════════════════════════════════════════════════════════ *
 *  Public API                                                             *
 * ═══════════════════════════════════════════════════════════════════════ */

int bv_ui_init(const char *font_path)
{
    if (TTF_Init() < 0) {
        fprintf(stderr, "[UI] TTF_Init: %s\n", TTF_GetError());
        return -1;
    }
    if (font_path && font_path[0]) {
        g_font_time = TTF_OpenFont(font_path, TIME_FONT_SZ);
        g_font_ui   = TTF_OpenFont(font_path, CTX_FONT_SZ);
        if (!g_font_time)
            fprintf(stderr, "[UI] Font open failed: %s\n", TTF_GetError());
    }
    g_init = 1;
    return 0;
}

void bv_ui_quit(void)
{
    if (g_font_time) { TTF_CloseFont(g_font_time); g_font_time = NULL; }
    if (g_font_ui)   { TTF_CloseFont(g_font_ui);   g_font_ui   = NULL; }
    TTF_Quit();
    g_init = 0;
}

void bv_ui_set_window_size(int w, int h) { g_win_w = w; g_win_h = h; }

void bv_ui_set_subtitles(const char *p0, const char *p1, const char *p2,
                          int count, int active)
{
    strncpy(g_sub[0], p0 ? p0 : "", 511);
    strncpy(g_sub[1], p1 ? p1 : "", 511);
    strncpy(g_sub[2], p2 ? p2 : "", 511);
    g_sub_count  = count > 3 ? 3 : count;
    g_sub_active = active;
}

/* ── bv_ui_draw ───────────────────────────────────────────────────────── */
void bv_ui_draw(SDL_Renderer *rend,
                float position, float duration,
                int playing, int looping, int muted,
                int volume,  int fullscreen, int speed_idx,
                int visible)
{
    if (!rend) return;

    /* ── Context menu (always draw if open) ─────────────────────────── */
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

        static const char *base_labels[CTX_COUNT] = {
            "Open Video File...",
            "Subtitle: Off",
            NULL, NULL, NULL
        };
        static char slbl[3][80];

        for (int i = 0; i < 3; i++) {
            if (i < g_sub_count) {
                /* extract filename from path */
                const char *p = g_sub[i], *bn = p;
                while (*p) { if (*p=='/'||*p=='\\') bn=p+1; p++; }
                snprintf(slbl[i], 79, "Track %d: %.40s", i+1, bn);
            } else {
                snprintf(slbl[i], 79, "Track %d: (load file)", i+1);
            }
        }

        for (int i = 0; i < n; i++) {
            int iy = my + 4 + i * CTX_ITEM_H;
            const char *lbl = (i < 2) ? base_labels[i] : slbl[i-2];

            if (i == g_hover_ctx) {
                col(rend, C_CTX_HOVER);
                frect(rend, mx+2, iy, CTX_W-4, CTX_ITEM_H);
            }

            /* separator after "Open File" */
            if (i == 1) {
                col(rend, C_CTX_SEP);
                SDL_RenderDrawLine(rend, mx+8, iy, mx+CTX_W-8, iy);
            }

            /* active subtitle marker dot */
            int is_active = (i >= 2 && (i-2) == g_sub_active);
            int disabled  = (i >= 2 && (i-2) >= g_sub_count);

            Uint8 tr = disabled ? 85 : is_active ? 210 : 205;
            Uint8 tg = disabled ? 85 : is_active ?  45 : 205;
            Uint8 tb = disabled ? 85 : is_active ?  45 : 205;

            if (is_active) fcircle(rend, mx+8, iy+CTX_ITEM_H/2, 3);

            if (lbl)
                draw_text(rend, g_font_ui, lbl,
                          mx + CTX_PAD_X + (is_active ? 8 : 0),
                          iy + (CTX_ITEM_H - CTX_FONT_SZ) / 2,
                          tr, tg, tb, 255);
        }
    }

    if (!visible) return;

    /* ── Bar background ─────────────────────────────────────────────── */
    g_bar_y = g_win_h - BAR_H;
    col(rend, C_BAR_BG);
    frect(rend, 0, g_bar_y, g_win_w, BAR_H);

    /* ── Seek bar ───────────────────────────────────────────────────── */
    g_sk_x = SEEK_MARGIN;
    g_sk_y = g_bar_y + SEEK_TOP;
    g_sk_w = g_win_w - SEEK_MARGIN * 2;

    /* track */
    col(rend, C_SEEK_TRACK);
    frect(rend, g_sk_x, g_sk_y, g_sk_w, SEEK_H);

    /* fill */
    float frac = (duration > 0.f) ? position / duration : 0.f;
    if (frac < 0.f) frac = 0.f;
    if (frac > 1.f) frac = 1.f;
    int fill_w = (int)(g_sk_w * frac);
    col(rend, C_SEEK_FILL);
    frect(rend, g_sk_x, g_sk_y, fill_w, SEEK_H);

    /* knob */
    col(rend, C_KNOB);
    fcircle(rend, g_sk_x + fill_w, g_sk_y + SEEK_H / 2, KNOB_R);

    /* ── Timestamps ─────────────────────────────────────────────────── */
    if (g_font_time) {
        int ps = (int)position, ds = (int)duration;
        char t0[10], t1[10];
        snprintf(t0, 10, "%02d:%02d", ps/60, ps%60);
        snprintf(t1, 10, "%02d:%02d", ds/60, ds%60);

        int ty = g_sk_y + SEEK_H + 4;
        draw_text(rend, g_font_time, t0, g_sk_x, ty, C_TIME);

        int tw = 0, th_ign = 0;
        TTF_SizeUTF8(g_font_time, t1, &tw, &th_ign);
        draw_text(rend, g_font_time, t1, g_sk_x + g_sk_w - tw, ty, C_TIME);
    }

    /* ── Buttons ────────────────────────────────────────────────────── */
    /*  Left group:  Prev  PlayPause  Next  Loop
        Right group: Speed  Volume  Fullscreen  Menu                    */
    int by    = g_bar_y + BTN_ROW_TOP;
    int left  = SEEK_MARGIN;
    int right = g_win_w - SEEK_MARGIN - BTN_W;

    int bx[BTN_COUNT];
    bx[BTN_PREV]       = left + 0*(BTN_W+4);
    bx[BTN_PLAYPAUSE]  = left + 1*(BTN_W+4);
    bx[BTN_NEXT]       = left + 2*(BTN_W+4);
    bx[BTN_LOOP]       = left + 3*(BTN_W+4);
    bx[BTN_MENU]       = right - 0*(BTN_W+4);
    bx[BTN_FULLSCREEN] = right - 1*(BTN_W+4);
    bx[BTN_VOLUME]     = right - 2*(BTN_W+4);
    bx[BTN_SPEED]      = right - 3*(BTN_W+4);

    for (int i = 0; i < BTN_COUNT; i++) {
        g_btn[i] = (SDL_Rect){bx[i], by, BTN_W, BTN_H};
        int cx = bx[i] + BTN_W/2, cy = by + BTN_H/2;

        /* hover highlight */
        if (i == g_hover_btn) {
            col(rend, C_BTN_HOVER);
            frect(rend, bx[i], by, BTN_W, BTN_H);
        }

        /* icon colour: red when active state (loop on, muted) */
        int is_on = (i == BTN_LOOP && looping) || (i == BTN_VOLUME && muted);
        if (is_on) col(rend, C_ICON_ON);
        else        col(rend, C_ICON);

        switch (i) {
            case BTN_PREV:      icon_prev(rend, cx, cy, ICON_SZ);              break;
            case BTN_PLAYPAUSE:
                if (playing) icon_pause(rend, cx, cy, ICON_SZ);
                else         icon_play (rend, cx, cy, ICON_SZ);
                break;
            case BTN_NEXT:      icon_next(rend, cx, cy, ICON_SZ);              break;
            case BTN_LOOP:      icon_loop(rend, cx, cy, ICON_SZ);              break;
            case BTN_VOLUME:    icon_volume(rend, cx, cy, ICON_SZ, muted);     break;
            case BTN_FULLSCREEN:icon_fullscreen(rend, cx, cy, ICON_SZ, fullscreen); break;
            case BTN_MENU:      icon_menu(rend, cx, cy);                       break;
            case BTN_SPEED: {
                static const char *spds[] = {"0.5x","1.0x","1.5x","2.0x"};
                int si = (speed_idx>=0&&speed_idx<4) ? speed_idx : 1;
                col(rend, C_ICON);
                if (g_font_ui)
                    draw_text(rend, g_font_ui, spds[si],
                              cx - 14, cy - CTX_FONT_SZ/2,
                              C_ICON);
                break;
            }
        }
    }

    (void)volume; /* volume bar future work */
}

/* ═══════════════════════════════════════════════════════════════════════ *
 *  Hit-testing                                                            *
 * ═══════════════════════════════════════════════════════════════════════ */

int bv_ui_hit_seek(int x, int y)
{
    return (x >= g_sk_x - 4 &&
            x <= g_sk_x + g_sk_w + 4 &&
            y >= g_sk_y - 10 &&
            y <= g_sk_y + SEEK_H + 10);
}

float bv_ui_seek_fraction(int x)
{
    if (g_sk_w <= 0) return 0.f;
    float f = (float)(x - g_sk_x) / (float)g_sk_w;
    return f < 0.f ? 0.f : f > 1.f ? 1.f : f;
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

void bv_ui_set_hover_btn(int idx) { g_hover_btn = idx; }
void bv_ui_set_seeking(int s)     { g_seeking   = s;   }
int  bv_ui_is_seeking(void)       { return g_seeking;  }

/* Context menu */
void bv_ui_open_ctx(int x, int y)
{
    g_ctx_open  = 1;
    g_ctx_x     = x;
    g_ctx_y     = y;
    g_hover_ctx = -1;
}

void bv_ui_close_ctx(void) { g_ctx_open = 0; g_hover_ctx = -1; }
int  bv_ui_ctx_open(void)  { return g_ctx_open; }

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
