-- bindings/ffmpeg/ffmpeg-avcodec.ads
-- Ada bindings for libavcodec (FFmpeg)
-- Covers: codec, context, packet, decode

with Interfaces.C;
with System;
with FFmpeg.AVUtil;

package FFmpeg.AVCodec is

   use Interfaces.C;
   use FFmpeg.AVUtil;

   -- ─────────────────────────────────────────────
   --  Error codes
   -- ─────────────────────────────────────────────
   AVERROR_EAGAIN : constant int := -11;  -- EAGAIN / AVERROR(EAGAIN)
   AVERROR_EOF    : constant int := -541478725;

   -- ─────────────────────────────────────────────
   --  AVCodecID (subset of common IDs)
   -- ─────────────────────────────────────────────
   type AVCodecID is new unsigned;

   -- ─────────────────────────────────────────────
   --  AVPacket
   -- ─────────────────────────────────────────────
   AV_PKT_FLAG_KEY : constant int := 1;

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
      --  ... additional fields
   end record with Convention => C;
   type AVPacket_Ptr is access AVPacket;

   -- ─────────────────────────────────────────────
   --  AVCodecParameters
   -- ─────────────────────────────────────────────
   type AVCodecParameters is record
      Codec_Type    : int;    -- AVMediaType
      Codec_Id      : AVCodecID;
      Codec_Tag     : unsigned;
      Extra_Data    : System.Address;
      Extra_Size    : int;
      Bit_Rate      : int64_t;
      Width         : int;
      Height        : int;
      Sample_Rate   : int;
      Channels      : int;
      Channel_Layout : unsigned_long;
      --  ... more fields
   end record with Convention => C;
   type AVCodecParameters_Ptr is access AVCodecParameters;

   -- ─────────────────────────────────────────────
   --  AVCodec (opaque, we only need pointer)
   -- ─────────────────────────────────────────────
   type AVCodec is limited private;
   type AVCodec_Ptr is access AVCodec;

   -- ─────────────────────────────────────────────
   --  Frame data arrays (for pixel/audio planes)
   -- ─────────────────────────────────────────────
   MAX_PLANES : constant := 8;

   type Data_Array     is array (0 .. MAX_PLANES - 1) of System.Address;
   type Linesize_Array is array (0 .. MAX_PLANES - 1) of int;

   -- ─────────────────────────────────────────────
   --  AVCodecContext (simplified)
   -- ─────────────────────────────────────────────
   type AVCodecContext is record
      Av_Class     : System.Address;
      Log_Level    : int;
      Codec_Type   : int;
      Codec        : AVCodec_Ptr;
      Codec_Id     : AVCodecID;
      Codec_Tag    : unsigned;
      Priv_Data    : System.Address;
      Internal     : System.Address;
      Opaque       : System.Address;
      Bit_Rate     : int64_t;
      Flags        : int;
      Flags2       : int;
      Width        : int;
      Height       : int;
      Pix_Fmt      : int;    -- AVPixelFormat
      Thread_Count : int;
      Sample_Rate  : int;
      Channels     : int;
      Sample_Fmt   : int;
      --  ... many more; opaque rest via System.Address padding
   end record with Convention => C;
   type AVCodecContext_Ptr is access AVCodecContext;

   -- ─────────────────────────────────────────────
   --  API Functions
   -- ─────────────────────────────────────────────

   function avcodec_find_decoder (ID : AVCodecID) return AVCodec_Ptr
   with Import, Convention => C, External_Name => "avcodec_find_decoder";

   function avcodec_alloc_context3 (Codec : AVCodec_Ptr)
      return AVCodecContext_Ptr
   with Import, Convention => C, External_Name => "avcodec_alloc_context3";

   function avcodec_parameters_to_context
     (Codec_Ctx : AVCodecContext_Ptr;
      Par       : AVCodecParameters_Ptr) return int
   with Import, Convention => C,
        External_Name => "avcodec_parameters_to_context";

   function avcodec_open2
     (AVCtx   : AVCodecContext_Ptr;
      Codec   : AVCodec_Ptr;
      Options : System.Address) return int
   with Import, Convention => C, External_Name => "avcodec_open2";

   procedure avcodec_free_context (AVCtx : in out AVCodecContext_Ptr)
   with Import, Convention => C, External_Name => "avcodec_free_context";

   procedure avcodec_flush_buffers (AVCtx : AVCodecContext_Ptr)
   with Import, Convention => C, External_Name => "avcodec_flush_buffers";

   function av_packet_alloc return AVPacket_Ptr
   with Import, Convention => C, External_Name => "av_packet_alloc";

   procedure av_packet_free (Pkt : in out AVPacket_Ptr)
   with Import, Convention => C, External_Name => "av_packet_free";

   procedure av_packet_unref (Pkt : AVPacket_Ptr)
   with Import, Convention => C, External_Name => "av_packet_unref";

   function avcodec_send_packet
     (AVCtx : AVCodecContext_Ptr;
      Pkt   : AVPacket_Ptr) return int
   with Import, Convention => C, External_Name => "avcodec_send_packet";

   function avcodec_receive_frame
     (AVCtx : AVCodecContext_Ptr;
      Frame : FFmpeg.AVUtil.AVFrame_Ptr) return int
   with Import, Convention => C, External_Name => "avcodec_receive_frame";

private
   type AVCodec is null record;

end FFmpeg.AVCodec;
