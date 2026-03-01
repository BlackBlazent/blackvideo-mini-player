-- ffmpeg-avformat.ads
-- Ada bindings for libavformat (FFmpeg 8.x)
-- Struct field accessors delegate to ffmpeg_helpers.c (C wrappers).
-- No Ada pointer arithmetic. Works with any FFmpeg version.

with Interfaces.C;
with Interfaces.C.Strings;
with System;
with FFmpeg.AVUtil;
with FFmpeg.AVCodec;

package FFmpeg.AVFormat is

   use Interfaces.C;
   use FFmpeg.AVUtil;

   AVSEEK_FLAG_BACKWARD : constant int := 1;
   AVSEEK_FLAG_ANY      : constant int := 4;

   subtype AVFormatContext_Ptr is System.Address;
   Null_Format_Ctx : constant AVFormatContext_Ptr := System.Null_Address;

   -- ── Core open/close ───────────────────────────────────────────────────
   function avformat_open_input
     (PS      : in out AVFormatContext_Ptr;
      URL     : Interfaces.C.Strings.chars_ptr;
      Fmt     : System.Address;
      Options : System.Address) return int
   with Import, Convention => C, External_Name => "avformat_open_input";

   function avformat_find_stream_info
     (IC      : AVFormatContext_Ptr;
      Options : System.Address) return int
   with Import, Convention => C, External_Name => "avformat_find_stream_info";

   procedure avformat_close_input (S : in out AVFormatContext_Ptr)
   with Import, Convention => C, External_Name => "avformat_close_input";

   -- ── Stream discovery ──────────────────────────────────────────────────
   function av_find_best_stream
     (IC             : AVFormatContext_Ptr;
      Media_Type     : int;
      Wanted_Stream  : int;
      Related_Stream : int;
      Decoder_Ret    : System.Address;
      Flags          : int) return int
   with Import, Convention => C, External_Name => "av_find_best_stream";

   -- ── Stream field accessors (implemented in ffmpeg_helpers.c) ─────────
   -- C wrappers use real FFmpeg headers so offsets are always correct.

   function Get_Stream_Codecpar
     (IC    : AVFormatContext_Ptr;
      Index : int) return FFmpeg.AVCodec.AVCodecParameters_Ptr
   with Import, Convention => C, External_Name => "bv_stream_codecpar";

   procedure Get_Stream_Framerate
     (IC    : AVFormatContext_Ptr;
      Index : int;
      Num   : out int;
      Den   : out int)
   with Import, Convention => C, External_Name => "bv_stream_framerate";

   procedure Get_Stream_Timebase
     (IC    : AVFormatContext_Ptr;
      Index : int;
      Num   : out int;
      Den   : out int)
   with Import, Convention => C, External_Name => "bv_stream_timebase";

   function Get_Stream_Start_Time
     (IC    : AVFormatContext_Ptr;
      Index : int) return int64_t
   with Import, Convention => C, External_Name => "bv_stream_start_time";

   -- ── Packet I/O ────────────────────────────────────────────────────────
   function av_read_frame
     (S   : AVFormatContext_Ptr;
      Pkt : FFmpeg.AVCodec.AVPacket_Ptr) return int
   with Import, Convention => C, External_Name => "av_read_frame";

   function av_seek_frame
     (S            : AVFormatContext_Ptr;
      Stream_Index : int;
      Timestamp    : int64_t;
      Flags        : int) return int
   with Import, Convention => C, External_Name => "av_seek_frame";

end FFmpeg.AVFormat;
