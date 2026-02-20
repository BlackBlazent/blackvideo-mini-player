-- ffmpeg-avformat.ads
-- Ada bindings for libavformat
-- File name: ffmpeg-avformat.ads → package FFmpeg.AVFormat

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
   --  AVStream (partial)
   -- ─────────────────────────────────────────────
   type AVStream is record
      Index          : int;
      Id             : int;
      Codec          : System.Address;     -- deprecated, use Codecpar
      Priv_Data      : System.Address;
      Time_Base      : AVRational;
      Start_Time     : int64_t;
      Duration       : int64_t;
      Nb_Frames      : int64_t;
      Disposition    : int;
      Discard        : int;
      Sample_Aspect_Ratio : AVRational;
      Metadata       : System.Address;
      Avg_Frame_Rate : AVRational;
      Attached_Pic   : System.Address;    -- AVPacket embedded
      Side_Data      : System.Address;
      Nb_Side_Data   : int;
      Event_Flags    : int;
      R_Frame_Rate   : AVRational;
      Codecpar       : FFmpeg.AVCodec.AVCodecParameters_Ptr;
      --  opaque tail
   end record with Convention => C;

   type AVStream_Ptr is access AVStream;

   --  Array of stream pointers inside AVFormatContext
   type AVStream_Ptr_Array is array (Natural range <>) of AVStream_Ptr
     with Convention => C;
   type AVStream_Ptr_Array_Ptr is access AVStream_Ptr_Array;

   -- ─────────────────────────────────────────────
   --  AVFormatContext (partial)
   -- ─────────────────────────────────────────────
   type AVFormatContext is record
      Av_Class    : System.Address;
      Iformat     : System.Address;
      Oformat     : System.Address;
      Priv_Data   : System.Address;
      Pb          : System.Address;
      Ctx_Flags   : int;
      Nb_Streams  : unsigned;
      Streams     : AVStream_Ptr_Array_Ptr;
      Filename    : chars_ptr;             -- deprecated
      Url         : chars_ptr;
      Start_Time  : int64_t;
      Duration    : int64_t;
      Bit_Rate    : int64_t;
      --  opaque tail
   end record with Convention => C;

   type AVFormatContext_Ptr is access AVFormatContext;

   -- ─────────────────────────────────────────────
   --  AVDictionary (opaque)
   -- ─────────────────────────────────────────────
   type AVDictionary is null record with Convention => C;
   type AVDictionary_Ptr     is access AVDictionary;
   type AVDictionary_Ptr_Ptr is access AVDictionary_Ptr;

   -- ─────────────────────────────────────────────
   --  API
   -- ─────────────────────────────────────────────

   function avformat_open_input
     (PS      : in out AVFormatContext_Ptr;
      URL     : chars_ptr;
      Fmt     : System.Address;           -- null = auto-detect
      Options : System.Address) return int  -- null options
   with Import, Convention => C, External_Name => "avformat_open_input";

   function avformat_find_stream_info
     (IC      : AVFormatContext_Ptr;
      Options : System.Address) return int
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

   procedure avformat_close_input (S : in out AVFormatContext_Ptr)
   with Import, Convention => C, External_Name => "avformat_close_input";

   procedure avformat_free_context (S : AVFormatContext_Ptr)
   with Import, Convention => C, External_Name => "avformat_free_context";

   procedure av_dump_format
     (IC        : AVFormatContext_Ptr;
      Index     : int;
      URL       : chars_ptr;
      Is_Output : int)
   with Import, Convention => C, External_Name => "av_dump_format";

end FFmpeg.AVFormat;
