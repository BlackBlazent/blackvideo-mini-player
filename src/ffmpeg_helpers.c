/*
 * ffmpeg_helpers.c
 * Thin C wrapper functions that access FFmpeg struct fields using the
 * real FFmpeg headers. The C compiler resolves all offsets correctly
 * regardless of FFmpeg version — no Ada pointer arithmetic guessing.
 *
 * Called from: ffmpeg-avformat.adb, ffmpeg-avcodec.adb
 * Headers:     lib\include\  (libavformat, libavcodec, libavutil)
 */

#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/avutil.h>

/* ── AVFormatContext / AVStream accessors ─────────────────────────────── */

AVCodecParameters* bv_stream_codecpar(AVFormatContext *ic, int index) {
    if (!ic || index < 0 || (unsigned)index >= ic->nb_streams) return NULL;
    if (!ic->streams[index]) return NULL;
    return ic->streams[index]->codecpar;
}

void bv_stream_framerate(AVFormatContext *ic, int index, int *num, int *den) {
    if (!ic || index < 0 || (unsigned)index >= ic->nb_streams
            || !ic->streams[index]) {
        *num = 25; *den = 1; return;
    }
    *num = ic->streams[index]->avg_frame_rate.num;
    *den = ic->streams[index]->avg_frame_rate.den;
}

void bv_stream_timebase(AVFormatContext *ic, int index, int *num, int *den) {
    if (!ic || index < 0 || (unsigned)index >= ic->nb_streams
            || !ic->streams[index]) {
        *num = 1; *den = 1000; return;
    }
    *num = ic->streams[index]->time_base.num;
    *den = ic->streams[index]->time_base.den;
}

int64_t bv_stream_start_time(AVFormatContext *ic, int index) {
    if (!ic || index < 0 || (unsigned)index >= ic->nb_streams
            || !ic->streams[index]) return 0;
    return ic->streams[index]->start_time;
}

/* ── AVCodecParameters accessors ─────────────────────────────────────── */

int     bv_codecpar_codec_type (AVCodecParameters *p) { return p ? p->codec_type  : -1; }
unsigned bv_codecpar_codec_id  (AVCodecParameters *p) { return p ? p->codec_id    : 0;  }
int     bv_codecpar_width      (AVCodecParameters *p) { return p ? p->width       : 0;  }
int     bv_codecpar_height     (AVCodecParameters *p) { return p ? p->height      : 0;  }
int     bv_codecpar_pix_fmt    (AVCodecParameters *p) { return p ? p->format      : -1; }
