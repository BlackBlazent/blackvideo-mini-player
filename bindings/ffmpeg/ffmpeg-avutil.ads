-- bindings/ffmpeg/ffmpeg-avutil.ads
-- Ada bindings for libavutil (FFmpeg)
-- Covers: AVFrame, AVRational, pixel formats, media types

with Interfaces.C;
with System;

package FFmpeg.AVUtil is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  Basic types
   -- ─────────────────────────────────────────────
   type int64_t  is new Interfaces.C.long;
   type uint8_ptr is access Interfaces.C.unsigned_char;
   type uint8_ptr_array is array (Natural range <>) of uint8_ptr;

   -- ─────────────────────────────────────────────
   --  AVRational
   -- ─────────────────────────────────────────────
   type AVRational is record
      Num : int;
      Den : int;
   end record with Convention => C;

   -- ─────────────────────────────────────────────
   --  AVMediaType
   -- ─────────────────────────────────────────────
   AVMEDIA_TYPE_UNKNOWN    : constant int := -1;
   AVMEDIA_TYPE_VIDEO      : constant int := 0;
   AVMEDIA_TYPE_AUDIO      : constant int := 1;
   AVMEDIA_TYPE_DATA       : constant int := 2;
   AVMEDIA_TYPE_SUBTITLE   : constant int := 3;
   AVMEDIA_TYPE_ATTACHMENT : constant int := 4;

   -- ─────────────────────────────────────────────
   --  AVPixelFormat (common subset)
   -- ─────────────────────────────────────────────
   AV_PIX_FMT_NONE    : constant int := -1;
   AV_PIX_FMT_YUV420P : constant int := 0;
   AV_PIX_FMT_RGB24   : constant int := 2;
   AV_PIX_FMT_BGR24   : constant int := 3;
   AV_PIX_FMT_YUVJ420P : constant int := 12;
   AV_PIX_FMT_NV12    : constant int := 23;

   -- ─────────────────────────────────────────────
   --  AVSampleFormat
   -- ─────────────────────────────────────────────
   AV_SAMPLE_FMT_S16  : constant int := 1;
   AV_SAMPLE_FMT_FLT  : constant int := 3;
   AV_SAMPLE_FMT_S16P : constant int := 6;
   AV_SAMPLE_FMT_FLTP : constant int := 8;

   -- ─────────────────────────────────────────────
   --  AVFrame
   -- ─────────────────────────────────────────────
   AV_NUM_DATA_POINTERS : constant := 8;

   type Data_Plane_Array     is array (0 .. AV_NUM_DATA_POINTERS - 1)
      of System.Address with Convention => C;
   type Linesize_Plane_Array is array (0 .. AV_NUM_DATA_POINTERS - 1)
      of int with Convention => C;

   type AVFrame is record
      Data          : Data_Plane_Array;
      Linesize      : Linesize_Plane_Array;
      Extended_Data : System.Address;
      Width         : int;
      Height        : int;
      Nb_Samples    : int;
      Format        : int;
      Key_Frame     : int;
      PTS           : int64_t;
      Pkt_DTS       : int64_t;
      --  ... rest opaque
   end record with Convention => C;
   type AVFrame_Ptr is access AVFrame;

   -- ─────────────────────────────────────────────
   --  API Functions
   -- ─────────────────────────────────────────────

   function av_frame_alloc return AVFrame_Ptr
   with Import, Convention => C, External_Name => "av_frame_alloc";

   procedure av_frame_free (Frame : in out AVFrame_Ptr)
   with Import, Convention => C, External_Name => "av_frame_free";

   procedure av_frame_unref (Frame : AVFrame_Ptr)
   with Import, Convention => C, External_Name => "av_frame_unref";

   --  Fill frame planes with a pre-allocated buffer
   function avpicture_fill
     (Picture  : AVFrame_Ptr;
      Ptr      : System.Address;
      Pix_Fmt  : int;
      Width    : int;
      Height   : int) return int
   with Import, Convention => C, External_Name => "av_image_fill_arrays";

   procedure av_log_set_level (Level : int)
   with Import, Convention => C, External_Name => "av_log_set_level";

   AV_LOG_QUIET   : constant int := -8;
   AV_LOG_ERROR   : constant int := 16;
   AV_LOG_WARNING : constant int := 24;
   AV_LOG_INFO    : constant int := 32;

end FFmpeg.AVUtil;
