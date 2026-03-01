/*
 * ffmpeg_helpers.c
 * C wrappers for FFmpeg struct field access.
 */

#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/avutil.h>
#include <libavutil/log.h>
#include <libavutil/opt.h>
#include <libavutil/channel_layout.h>
#include <libswresample/swresample.h>

/* ── Log suppression ──────────────────────────────────────────────────── */
void bv_suppress_logs(void) {
    av_log_set_level(AV_LOG_ERROR);
}

/* ── AVFormatContext / AVStream accessors ─────────────────────────────── */

AVCodecParameters* bv_stream_codecpar(AVFormatContext *ic, int index) {
    if (!ic || index < 0 || (unsigned)index >= ic->nb_streams) return NULL;
    if (!ic->streams[index]) return NULL;
    return ic->streams[index]->codecpar;
}

void bv_stream_framerate(AVFormatContext *ic, int index, int *num, int *den) {
    if (!ic || index < 0 || (unsigned)index >= ic->nb_streams
            || !ic->streams[index]) { *num = 25; *den = 1; return; }
    *num = (int)ic->streams[index]->avg_frame_rate.num;
    *den = (int)ic->streams[index]->avg_frame_rate.den;
}

void bv_stream_timebase(AVFormatContext *ic, int index, int *num, int *den) {
    if (!ic || index < 0 || (unsigned)index >= ic->nb_streams
            || !ic->streams[index]) { *num = 1; *den = 1000; return; }
    *num = (int)ic->streams[index]->time_base.num;
    *den = (int)ic->streams[index]->time_base.den;
}

int64_t bv_stream_start_time(AVFormatContext *ic, int index) {
    if (!ic || index < 0 || (unsigned)index >= ic->nb_streams
            || !ic->streams[index]) return 0;
    int64_t st = ic->streams[index]->start_time;
    return (st == AV_NOPTS_VALUE) ? 0 : st;
}

/* ── AVCodecParameters accessors ─────────────────────────────────────── */

int      bv_codecpar_codec_type (AVCodecParameters *p) { return p ? (int)p->codec_type : -1; }
unsigned bv_codecpar_codec_id   (AVCodecParameters *p) { return p ? (unsigned)p->codec_id : 0; }
int      bv_codecpar_width      (AVCodecParameters *p) { return p ? p->width  : 0; }
int      bv_codecpar_height     (AVCodecParameters *p) { return p ? p->height : 0; }
int      bv_codecpar_pix_fmt    (AVCodecParameters *p) { return p ? p->format : -1; }
int      bv_codecpar_sample_rate(AVCodecParameters *p) { return p ? p->sample_rate : 0; }
int      bv_codecpar_sample_fmt (AVCodecParameters *p) { return p ? p->format : -1; }

int bv_codecpar_channels(AVCodecParameters *p) {
#if LIBAVCODEC_VERSION_MAJOR >= 60
    return p ? p->ch_layout.nb_channels : 0;
#else
    return p ? p->channels : 0;
#endif
}

uint64_t bv_codecpar_channel_layout(AVCodecParameters *p) {
#if LIBAVCODEC_VERSION_MAJOR >= 60
    if (!p) return 0;
    if (p->ch_layout.order == AV_CHANNEL_ORDER_NATIVE)
        return p->ch_layout.u.mask;
    return (p->ch_layout.nb_channels == 1)
           ? AV_CH_LAYOUT_MONO : AV_CH_LAYOUT_STEREO;
#else
    return p ? p->channel_layout : 0;
#endif
}

/* ── AVPacket accessor ────────────────────────────────────────────────── */
int bv_packet_stream_index(AVPacket *pkt) {
    return pkt ? pkt->stream_index : -1;
}

/* ── AVFrame audio accessors ─────────────────────────────────────────── */
int   bv_frame_nb_samples (AVFrame *f) { return f ? f->nb_samples : 0; }
int   bv_frame_sample_rate(AVFrame *f) { return f ? f->sample_rate : 0; }

int bv_frame_channels(AVFrame *f) {
#if LIBAVCODEC_VERSION_MAJOR >= 60
    return f ? f->ch_layout.nb_channels : 0;
#else
    return f ? f->channels : 0;
#endif
}

/* ── Audio resampling ────────────────────────────────────────────────────
 *
 * bv_swr_convert_frame — convert a full AVFrame to S16 stereo output.
 *
 * This replaces the old two-step (bv_frame_data0 + swr_convert in Ada)
 * which only passed data[0] — BROKEN for planar formats like FLTP (fmt=8)
 * where each channel lives in a separate plane (data[0], data[1], ...).
 * swr_convert needs ALL plane pointers for planar input.
 * Doing it in C means the compiler handles the pointer array correctly.
 *
 * out_buf    : caller-allocated S16 stereo output buffer
 * out_count  : capacity in samples (per channel)
 * frame      : decoded audio frame (any format, any channel layout)
 * Returns    : number of output samples written (per channel), or <0 on error
 */
int bv_swr_convert_frame(SwrContext *swr, void *out_buf, int out_count,
                          AVFrame *frame)
{
    if (!swr || !out_buf || !frame) return -1;
    uint8_t *out_ptr = (uint8_t *)out_buf;
    return swr_convert(swr,
                       &out_ptr,               /* output: one interleaved plane */
                       out_count,
                       (const uint8_t **)frame->data,  /* ALL input planes */
                       frame->nb_samples);
}

/* ── SwrContext setup ─────────────────────────────────────────────────── */
SwrContext* bv_swr_setup(
    uint64_t out_ch_layout, int out_sample_fmt, int out_sample_rate,
    uint64_t in_ch_layout,  int in_sample_fmt,  int in_sample_rate)
{
    SwrContext *swr = NULL;
    int ret;

#if LIBSWRESAMPLE_VERSION_MAJOR < 5
    swr = swr_alloc_set_opts(NULL,
        (int64_t)out_ch_layout, out_sample_fmt, out_sample_rate,
        (int64_t)in_ch_layout,  in_sample_fmt,  in_sample_rate,
        0, NULL);
    if (!swr) return NULL;
#else
    swr = swr_alloc();
    if (!swr) return NULL;

    av_opt_set_chlayout(swr, "in_chlayout",
        &(AVChannelLayout)AV_CHANNEL_LAYOUT_MASK(
            av_popcount64(in_ch_layout), in_ch_layout), 0);
    av_opt_set_int       (swr, "in_sample_rate",  in_sample_rate,  0);
    av_opt_set_sample_fmt(swr, "in_sample_fmt",   in_sample_fmt,   0);
    av_opt_set_chlayout(swr, "out_chlayout",
        &(AVChannelLayout)AV_CHANNEL_LAYOUT_MASK(
            av_popcount64(out_ch_layout), out_ch_layout), 0);
    av_opt_set_int       (swr, "out_sample_rate", out_sample_rate, 0);
    av_opt_set_sample_fmt(swr, "out_sample_fmt",  out_sample_fmt,  0);
#endif

    ret = swr_init(swr);
    if (ret < 0) { swr_free(&swr); return NULL; }
    return swr;
}
