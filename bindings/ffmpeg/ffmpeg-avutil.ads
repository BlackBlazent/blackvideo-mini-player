-- ffmpeg-avutil.ads
-- Ada bindings for libavutil

with Interfaces.C;
with System;

package FFmpeg.AVUtil is

   use Interfaces.C;

   subtype int64_t  is long;
   subtype uint64_t is unsigned_long;

   type AVRational is record
      Num : int;
      Den : int;
   end record with Convention => C;

   -- ── AVMediaType ───────────────────────────────────────────────────────
   AVMEDIA_TYPE_UNKNOWN  : constant int := -1;
   AVMEDIA_TYPE_VIDEO    : constant int :=  0;
   AVMEDIA_TYPE_AUDIO    : constant int :=  1;
   AVMEDIA_TYPE_DATA     : constant int :=  2;
   AVMEDIA_TYPE_SUBTITLE : constant int :=  3;

   -- ── AVPixelFormat (common subset) ─────────────────────────────────────
   AV_PIX_FMT_NONE     : constant int := -1;
   AV_PIX_FMT_YUV420P  : constant int :=  0;
   AV_PIX_FMT_RGB24    : constant int :=  2;
   AV_PIX_FMT_BGR24    : constant int :=  3;
   AV_PIX_FMT_NV12     : constant int := 23;
   AV_PIX_FMT_YUVJ420P : constant int := 12;

   -- ── AVSampleFormat ────────────────────────────────────────────────────
   AV_SAMPLE_FMT_NONE : constant int := -1;
   AV_SAMPLE_FMT_U8   : constant int :=  0;
   AV_SAMPLE_FMT_S16  : constant int :=  1;
   AV_SAMPLE_FMT_S32  : constant int :=  2;
   AV_SAMPLE_FMT_FLT  : constant int :=  3;
   AV_SAMPLE_FMT_S16P : constant int :=  6;
   AV_SAMPLE_FMT_FLTP : constant int :=  8;

   -- ── AVFrame data planes ───────────────────────────────────────────────
   AV_NUM_DATA_POINTERS : constant := 8;

   type Plane_Data_Array is array (0 .. AV_NUM_DATA_POINTERS - 1)
      of System.Address with Convention => C;

   type Plane_Linesize_Array is array (0 .. AV_NUM_DATA_POINTERS - 1)
      of int with Convention => C;

   -- ── AVFrame (partial — enough for video decode) ───────────────────────
   type AVFrame is record
      Data          : Plane_Data_Array;
      Linesize      : Plane_Linesize_Array;
      Extended_Data : System.Address;
      Width         : int;
      Height        : int;
      Nb_Samples    : int;
      Format        : int;
      Key_Frame     : int;
      PTS           : int64_t;
      Pkt_DTS       : int64_t;
   end record with Convention => C;

   type AVFrame_Ptr is access AVFrame;

   -- ── Log suppression (implemented in ffmpeg_helpers.c) ─────────────────
   -- Call once at startup to silence FFmpeg verbose output.
   -- Sets log level to AV_LOG_ERROR — only real errors are printed.
   procedure bv_suppress_logs
   with Import, Convention => C, External_Name => "bv_suppress_logs";

   -- ── Log level control ─────────────────────────────────────────────────
   AV_LOG_QUIET   : constant int := -8;
   AV_LOG_ERROR   : constant int := 16;
   AV_LOG_WARNING : constant int := 24;
   AV_LOG_INFO    : constant int := 32;

   procedure av_log_set_level (Level : int)
   with Import, Convention => C, External_Name => "av_log_set_level";

   -- ── Frame API ─────────────────────────────────────────────────────────
   function av_frame_alloc return AVFrame_Ptr
   with Import, Convention => C, External_Name => "av_frame_alloc";

   procedure av_frame_free (Frame : in out AVFrame_Ptr)
   with Import, Convention => C, External_Name => "av_frame_free";

   procedure av_frame_unref (Frame : AVFrame_Ptr)
   with Import, Convention => C, External_Name => "av_frame_unref";

   function av_image_fill_arrays
     (Dst_Data     : out Plane_Data_Array;
      Dst_Linesize : out Plane_Linesize_Array;
      Src          : System.Address;
      Pix_Fmt      : int;
      Width        : int;
      Height       : int;
      Align        : int) return int
   with Import, Convention => C, External_Name => "av_image_fill_arrays";

   function av_image_get_buffer_size
     (Pix_Fmt : int;
      Width   : int;
      Height  : int;
      Align   : int) return int
   with Import, Convention => C, External_Name => "av_image_get_buffer_size";

   function av_malloc (Size : unsigned_long) return System.Address
   with Import, Convention => C, External_Name => "av_malloc";

   procedure av_free (Ptr : System.Address)
   with Import, Convention => C, External_Name => "av_free";

end FFmpeg.AVUtil;
