-- bindings/ffmpeg/ffmpeg-avformat.ads
-- Ada bindings for libavformat (FFmpeg)
-- Covers: avformat_open_input, avformat_find_stream_info,
--         av_read_frame, av_seek_frame, avformat_close_input

with Interfaces.C;
with Interfaces.C.Strings;
with System;
with FFmpeg.AVUtil;
with FFmpeg.AVCodec;

package FFmpeg.AVFormat is

   use Interfaces.C;
   use Interfaces.C.Strings;
   use FFmpeg.AVUtil;

   -- ─────────────────────────────────────────────
   --  Seek flags
   -- ─────────────────────────────────────────────
   AVSEEK_FLAG_BACKWARD : constant int := 1;
   AVSEEK_FLAG_BYTE     : constant int := 2;
   AVSEEK_FLAG_ANY      : constant int := 4;
   AVSEEK_FLAG_FRAME    : constant int := 8;

   -- ─────────────────────────────────────────────
   --  AVStream (simplified)
   -- ─────────────────────────────────────────────
   type AVStream is record
      Index       : int;
      Codecpar    : FFmpeg.AVCodec.AVCodecParameters_Ptr;
      Time_Base   : AVRational;
      Avg_Frame_Rate : AVRational;
      Cur_Dts     : int64_t;
      Duration    : int64_t;
   end record with Convention => C;
   type AVStream_Ptr is access AVStream;

   type AVStream_Array is array (Natural range <>) of AVStream_Ptr
     with Convention => C;
   type AVStream_Array_Ptr is access AVStream_Array;

   -- ─────────────────────────────────────────────
   --  AVFormatContext (simplified)
   -- ─────────────────────────────────────────────
   type AVFormatContext is record
      Av_Class    : System.Address;   -- padding
      Iformat     : System.Address;
      Oformat     : System.Address;
      Priv_Data   : System.Address;
      Pb          : System.Address;
      Ctx_Flags   : int;
      Nb_Streams  : unsigned;
      Streams     : AVStream_Array_Ptr;
      Filename    : chars_ptr;
      Url         : chars_ptr;
      Start_Time  : int64_t;
      Duration    : int64_t;
      Bit_Rate    : int64_t;
      --  ... many more fields, unused here
   end record with Convention => C;
   type AVFormatContext_Ptr is access AVFormatContext;

   -- ─────────────────────────────────────────────
   --  AVDictionary (opaque)
   -- ─────────────────────────────────────────────
   type AVDictionary is null record with Convention => C;
   type AVDictionary_Ptr is access AVDictionary;
   type AVDictionary_Ptr_Ptr is access AVDictionary_Ptr;

   -- ─────────────────────────────────────────────
   --  AVInputFormat (opaque)
   -- ─────────────────────────────────────────────
   type AVInputFormat is null record with Convention => C;
   type AVInputFormat_Ptr is access AVInputFormat;

   -- ─────────────────────────────────────────────
   --  API Functions
   -- ─────────────────────────────────────────────

   function avformat_open_input
     (PS      : in out AVFormatContext_Ptr;
      URL     : chars_ptr;
      Fmt     : AVInputFormat_Ptr;
      Options : AVDictionary_Ptr_Ptr) return int
   with Import, Convention => C, External_Name => "avformat_open_input";

   function avformat_find_stream_info
     (IC      : AVFormatContext_Ptr;
      Options : AVDictionary_Ptr_Ptr) return int
   with Import, Convention => C, External_Name => "avformat_find_stream_info";

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

   procedure avformat_close_input
     (S : in out AVFormatContext_Ptr)
   with Import, Convention => C, External_Name => "avformat_close_input";

   procedure avformat_free_context (S : AVFormatContext_Ptr)
   with Import, Convention => C, External_Name => "avformat_free_context";

   --  Dump format info (debug)
   procedure av_dump_format
     (IC           : AVFormatContext_Ptr;
      Index        : int;
      URL          : chars_ptr;
      Is_Output    : int)
   with Import, Convention => C, External_Name => "av_dump_format";

end FFmpeg.AVFormat;
