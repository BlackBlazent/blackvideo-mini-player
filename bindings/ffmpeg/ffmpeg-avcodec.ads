-- ffmpeg-avcodec.ads
-- Ada bindings for libavcodec
-- File name: ffmpeg-avcodec.ads → package FFmpeg.AVCodec

with Interfaces.C;
with System;
with FFmpeg.AVUtil;

package FFmpeg.AVCodec is

   use Interfaces.C;
   use FFmpeg.AVUtil;

   -- ─────────────────────────────────────────────
   --  Error codes
   -- ─────────────────────────────────────────────
   AVERROR_EAGAIN : constant int := -11;
   AVERROR_EOF    : constant int := -541_478_725;

   -- ─────────────────────────────────────────────
   --  AVCodecID
   -- ─────────────────────────────────────────────
   type AVCodecID is new unsigned;

   -- ─────────────────────────────────────────────
   --  AVPacket
   -- ─────────────────────────────────────────────
   --  We only need a few fields; the rest is opaque.
   --  Use av_packet_alloc/free rather than stack allocation.
   type AVPacket is record
      Buf          : System.Address;
      PTS          : int64_t;
      DTS          : int64_t;
      Data         : System.Address;
      Size         : int;
      Stream_Index : int;
      Flags        : int;
      Duration     : int64_t;
      Pos          : int64_t;
      --  opaque tail
   end record with Convention => C;

   type AVPacket_Ptr is access AVPacket;

   -- ─────────────────────────────────────────────
   --  AVCodecParameters
   -- ─────────────────────────────────────────────
   type AVCodecParameters is record
      Codec_Type     : int;       -- AVMediaType
      Codec_Id       : AVCodecID;
      Codec_Tag      : unsigned;
      Extra_Data     : System.Address;
      Extra_Size     : int;
      Bit_Rate       : int64_t;
      Width          : int;
      Height         : int;
      Sample_Rate    : int;
      Channels       : int;
      Channel_Layout : uint64_t;
      Sample_Fmt     : int;
      --  opaque tail
   end record with Convention => C;

   type AVCodecParameters_Ptr is access AVCodecParameters;

   -- ─────────────────────────────────────────────
   --  AVCodec (opaque — only accessed via pointer)
   -- ─────────────────────────────────────────────
   type AVCodec is limited private;
   type AVCodec_Ptr is access AVCodec;

   -- ─────────────────────────────────────────────
   --  AVCodecContext (partial)
   -- ─────────────────────────────────────────────
   type AVCodecContext is record
      Av_Class     : System.Address;
      Log_Level_Offset : int;
      Codec_Type   : int;
      Codec        : AVCodec_Ptr;
      Codec_Id     : AVCodecID;
      Codec_Tag    : unsigned;
      Priv_Data    : System.Address;
      Internal     : System.Address;
      Opaque       : System.Address;
      Bit_Rate     : int64_t;
      Bit_Rate_Tolerance : int;
      Global_Quality : int;
      Compression_Level : int;
      Flags        : int;
      Flags2       : int;
      --  Video
      Width        : int;
      Height       : int;
      Coded_Width  : int;
      Coded_Height : int;
      Gop_Size     : int;
      Pix_Fmt      : int;
      --  Threads
      Thread_Count : int;
      Thread_Type  : int;
      Active_Thread_Type : int;
      Thread_Opaque : System.Address;
      --  Audio
      Sample_Rate  : int;
      Channels     : int;
      Sample_Fmt   : int;
      Frame_Size   : int;
      --  opaque tail — do not add more fields
   end record with Convention => C;

   type AVCodecContext_Ptr is access AVCodecContext;

   -- ─────────────────────────────────────────────
   --  API
   -- ─────────────────────────────────────────────

   function avcodec_find_decoder (ID : AVCodecID) return AVCodec_Ptr
   with Import, Convention => C, External_Name => "avcodec_find_decoder";

   function avcodec_alloc_context3 (Codec : AVCodec_Ptr)
      return AVCodecContext_Ptr
   with Import, Convention => C, External_Name => "avcodec_alloc_context3";

   function avcodec_parameters_to_context
     (Ctx : AVCodecContext_Ptr;
      Par : AVCodecParameters_Ptr) return int
   with Import, Convention => C,
        External_Name => "avcodec_parameters_to_context";

   function avcodec_open2
     (Ctx     : AVCodecContext_Ptr;
      Codec   : AVCodec_Ptr;
      Options : System.Address) return int     -- null options
   with Import, Convention => C, External_Name => "avcodec_open2";

   procedure avcodec_free_context (Ctx : in out AVCodecContext_Ptr)
   with Import, Convention => C, External_Name => "avcodec_free_context";

   procedure avcodec_flush_buffers (Ctx : AVCodecContext_Ptr)
   with Import, Convention => C, External_Name => "avcodec_flush_buffers";

   function av_packet_alloc return AVPacket_Ptr
   with Import, Convention => C, External_Name => "av_packet_alloc";

   procedure av_packet_free (Pkt : in out AVPacket_Ptr)
   with Import, Convention => C, External_Name => "av_packet_free";

   procedure av_packet_unref (Pkt : AVPacket_Ptr)
   with Import, Convention => C, External_Name => "av_packet_unref";

   function avcodec_send_packet
     (Ctx : AVCodecContext_Ptr;
      Pkt : AVPacket_Ptr) return int
   with Import, Convention => C, External_Name => "avcodec_send_packet";

   function avcodec_receive_frame
     (Ctx   : AVCodecContext_Ptr;
      Frame : FFmpeg.AVUtil.AVFrame_Ptr) return int
   with Import, Convention => C, External_Name => "avcodec_receive_frame";

private
   type AVCodec is null record;

end FFmpeg.AVCodec;
