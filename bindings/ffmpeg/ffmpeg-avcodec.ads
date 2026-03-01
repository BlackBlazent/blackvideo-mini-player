-- ffmpeg-avcodec.ads
-- Ada bindings for libavcodec (FFmpeg 8.x)

with Interfaces.C;
with System;
with FFmpeg.AVUtil;

package FFmpeg.AVCodec is

   use Interfaces.C;
   use FFmpeg.AVUtil;

   AVERROR_EAGAIN : constant int := -11;
   AVERROR_EOF    : constant int := -541_478_725;

   type AVCodecID is new unsigned;

   -- ── AVPacket — fully opaque, accessed only via C helper ───────────────
   -- DO NOT map AVPacket fields in Ada. FFmpeg 8.x changed the layout and
   -- the old offset-36 for stream_index is wrong. Use bv_packet_stream_index().
   type AVPacket     is limited private;
   type AVPacket_Ptr is access AVPacket;

   -- Read stream_index safely via C helper (real FFmpeg headers)
   function Get_Packet_Stream_Index (Pkt : AVPacket_Ptr) return int
   with Import, Convention => C, External_Name => "bv_packet_stream_index";

   -- ── Opaque handles ────────────────────────────────────────────────────
   subtype AVCodecParameters_Ptr is System.Address;
   subtype AVCodec_Ptr            is System.Address;
   subtype AVCodecContext_Ptr     is System.Address;

   Null_Codecpar  : constant AVCodecParameters_Ptr := System.Null_Address;
   Null_Codec     : constant AVCodec_Ptr           := System.Null_Address;
   Null_Codec_Ctx : constant AVCodecContext_Ptr    := System.Null_Address;

   -- ── AVCodecParameters video accessors (in ffmpeg_helpers.c) ──────────
   function Get_Codecpar_Codec_Type (P : AVCodecParameters_Ptr) return int
   with Import, Convention => C, External_Name => "bv_codecpar_codec_type";

   function Get_Codecpar_Codec_Id (P : AVCodecParameters_Ptr) return AVCodecID
   with Import, Convention => C, External_Name => "bv_codecpar_codec_id";

   function Get_Codecpar_Width (P : AVCodecParameters_Ptr) return int
   with Import, Convention => C, External_Name => "bv_codecpar_width";

   function Get_Codecpar_Height (P : AVCodecParameters_Ptr) return int
   with Import, Convention => C, External_Name => "bv_codecpar_height";

   function Get_Codecpar_Pix_Fmt (P : AVCodecParameters_Ptr) return int
   with Import, Convention => C, External_Name => "bv_codecpar_pix_fmt";

   -- ── AVCodecParameters audio accessors (in ffmpeg_helpers.c) ──────────
   function Get_Codecpar_Sample_Rate (P : AVCodecParameters_Ptr) return int
   with Import, Convention => C, External_Name => "bv_codecpar_sample_rate";

   function Get_Codecpar_Sample_Fmt (P : AVCodecParameters_Ptr) return int
   with Import, Convention => C, External_Name => "bv_codecpar_sample_fmt";

   function Get_Codecpar_Channels (P : AVCodecParameters_Ptr) return int
   with Import, Convention => C, External_Name => "bv_codecpar_channels";

   function Get_Codecpar_Channel_Layout (P : AVCodecParameters_Ptr) return unsigned_long
   with Import, Convention => C, External_Name => "bv_codecpar_channel_layout";

   -- ── AVFrame audio accessors (in ffmpeg_helpers.c) ────────────────────
   function Get_Frame_Nb_Samples (F : AVUtil.AVFrame_Ptr) return int
   with Import, Convention => C, External_Name => "bv_frame_nb_samples";

   function Get_Frame_Sample_Rate (F : AVUtil.AVFrame_Ptr) return int
   with Import, Convention => C, External_Name => "bv_frame_sample_rate";

   function Get_Frame_Data0 (F : AVUtil.AVFrame_Ptr) return System.Address
   with Import, Convention => C, External_Name => "bv_frame_data0";

   function Get_Frame_Data1 (F : AVUtil.AVFrame_Ptr) return System.Address
   with Import, Convention => C, External_Name => "bv_frame_data1";

   function Get_Frame_Channels (F : AVUtil.AVFrame_Ptr) return int
   with Import, Convention => C, External_Name => "bv_frame_channels";

   -- ── Core codec API ────────────────────────────────────────────────────
   function avcodec_find_decoder (ID : AVCodecID) return AVCodec_Ptr
   with Import, Convention => C, External_Name => "avcodec_find_decoder";

   function avcodec_alloc_context3 (Codec : AVCodec_Ptr) return AVCodecContext_Ptr
   with Import, Convention => C, External_Name => "avcodec_alloc_context3";

   function avcodec_parameters_to_context
     (Ctx : AVCodecContext_Ptr; Par : AVCodecParameters_Ptr) return int
   with Import, Convention => C, External_Name => "avcodec_parameters_to_context";

   function avcodec_open2
     (Ctx : AVCodecContext_Ptr; Codec : AVCodec_Ptr; Options : System.Address) return int
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
     (Ctx : AVCodecContext_Ptr; Pkt : AVPacket_Ptr) return int
   with Import, Convention => C, External_Name => "avcodec_send_packet";

   function avcodec_receive_frame
     (Ctx : AVCodecContext_Ptr; Frame : AVUtil.AVFrame_Ptr) return int
   with Import, Convention => C, External_Name => "avcodec_receive_frame";

private
   type AVPacket is null record;  -- opaque — never accessed directly in Ada

end FFmpeg.AVCodec;
