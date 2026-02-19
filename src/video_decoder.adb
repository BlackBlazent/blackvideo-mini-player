-- video_decoder.adb
-- BlackVideo Mini Player - FFmpeg Video Decoder Implementation
--
-- Uses C FFmpeg API through Ada bindings:
--   avformat_open_input → avcodec_find_decoder → avcodec_open2
--   av_read_frame → avcodec_send_packet → avcodec_receive_frame
--   sws_scale (YUV → RGB24)

with Ada.Text_IO;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

with FFmpeg.AVFormat;
with FFmpeg.AVCodec;
with FFmpeg.AVUtil;
with FFmpeg.SWScale;

package body Video_Decoder is

   use Ada.Text_IO;
   use Interfaces.C;
   use Interfaces.C.Strings;

   -- ─────────────────────────────────────────────
   --  Internal state
   -- ─────────────────────────────────────────────
   Fmt_Ctx      : FFmpeg.AVFormat.AVFormatContext_Ptr := null;
   Codec_Ctx    : FFmpeg.AVCodec.AVCodecContext_Ptr  := null;
   Sws_Ctx      : FFmpeg.SWScale.SwsContext_Ptr      := null;
   Packet       : FFmpeg.AVCodec.AVPacket_Ptr        := null;
   Raw_Frame    : FFmpeg.AVUtil.AVFrame_Ptr          := null;
   RGB_FramePtr : FFmpeg.AVUtil.AVFrame_Ptr          := null;

   Video_Stream_Index : Integer := -1;
   Audio_Stream_Index : Integer := -1;

   V_Width   : Integer := 0;
   V_Height  : Integer := 0;
   V_FPS     : Float   := 25.0;

   Paused_Flag : Boolean := False;
   EOF_Flag    : Boolean := False;

   --  Frame queue (simple circular buffer, capacity 8)
   Queue_Cap  : constant := 8;
   type Frame_Queue_Array is array (0 .. Queue_Cap - 1) of RGB_Frame;
   Frame_Queue  : Frame_Queue_Array;
   Queue_Head   : Integer := 0;
   Queue_Tail   : Integer := 0;
   Queue_Count  : Integer := 0;

   -- ─────────────────────────────────────────────
   --  Open
   -- ─────────────────────────────────────────────
   procedure Open
     (File_Path       : String;
      Width           : out Integer;
      Height          : out Integer;
      Frame_Delay_MS  : out Integer)
   is
      C_Path : chars_ptr := New_String (File_Path);
      Ret    : int;
   begin
      --  Open input
      Ret := FFmpeg.AVFormat.avformat_open_input
               (Fmt_Ctx, C_Path, null, null);
      Free (C_Path);

      if Ret < 0 then
         raise Program_Error with "avformat_open_input failed: " & File_Path;
      end if;

      --  Read stream info
      Ret := FFmpeg.AVFormat.avformat_find_stream_info (Fmt_Ctx, null);
      if Ret < 0 then
         raise Program_Error with "avformat_find_stream_info failed";
      end if;

      --  Find video stream
      declare
         N : constant Integer := Integer (Fmt_Ctx.Nb_Streams);
      begin
         for I in 0 .. N - 1 loop
            declare
               Codec_Par : FFmpeg.AVCodec.AVCodecParameters_Ptr
                         := Fmt_Ctx.Streams (I).Codecpar;
            begin
               if Codec_Par.Codec_Type = FFmpeg.AVUtil.AVMEDIA_TYPE_VIDEO
                  and Video_Stream_Index < 0
               then
                  Video_Stream_Index := I;
               elsif Codec_Par.Codec_Type = FFmpeg.AVUtil.AVMEDIA_TYPE_AUDIO
                     and Audio_Stream_Index < 0
               then
                  Audio_Stream_Index := I;
               end if;
            end;
         end loop;
      end;

      if Video_Stream_Index < 0 then
         raise Program_Error with "No video stream found in: " & File_Path;
      end if;

      Put_Line ("[Decoder] Video stream:" & Video_Stream_Index'Image);
      Put_Line ("[Decoder] Audio stream:" & Audio_Stream_Index'Image);

      --  Find decoder
      declare
         Codec_Par : FFmpeg.AVCodec.AVCodecParameters_Ptr
                   := Fmt_Ctx.Streams (Video_Stream_Index).Codecpar;
         Codec     : FFmpeg.AVCodec.AVCodec_Ptr;
      begin
         Codec := FFmpeg.AVCodec.avcodec_find_decoder (Codec_Par.Codec_Id);
         if Codec = null then
            raise Program_Error with "No decoder found for codec";
         end if;

         --  Allocate codec context
         Codec_Ctx := FFmpeg.AVCodec.avcodec_alloc_context3 (Codec);
         if Codec_Ctx = null then
            raise Program_Error with "avcodec_alloc_context3 failed";
         end if;

         --  Copy parameters to context
         Ret := FFmpeg.AVCodec.avcodec_parameters_to_context
                  (Codec_Ctx, Codec_Par);
         if Ret < 0 then
            raise Program_Error with "avcodec_parameters_to_context failed";
         end if;

         --  Enable multi-threading
         Codec_Ctx.Thread_Count := 4;

         --  Open codec
         Ret := FFmpeg.AVCodec.avcodec_open2 (Codec_Ctx, Codec, null);
         if Ret < 0 then
            raise Program_Error with "avcodec_open2 failed";
         end if;
      end;

      V_Width  := Integer (Codec_Ctx.Width);
      V_Height := Integer (Codec_Ctx.Height);

      --  Calculate FPS and frame delay
      declare
         TB : constant FFmpeg.AVUtil.AVRational
            := Fmt_Ctx.Streams (Video_Stream_Index).Avg_Frame_Rate;
      begin
         if TB.Den > 0 and TB.Num > 0 then
            V_FPS := Float (TB.Num) / Float (TB.Den);
         else
            V_FPS := 25.0;
         end if;
      end;

      Put_Line ("[Decoder] Resolution:" & V_Width'Image & "x" & V_Height'Image);
      Put_Line ("[Decoder] FPS:" & V_FPS'Image);

      --  Allocate frames
      Raw_Frame    := FFmpeg.AVUtil.av_frame_alloc;
      RGB_FramePtr := FFmpeg.AVUtil.av_frame_alloc;
      Packet       := FFmpeg.AVCodec.av_packet_alloc;

      --  Setup swscale: YUV → RGB24
      Sws_Ctx := FFmpeg.SWScale.sws_getContext
        (V_Width, V_Height, Integer (Codec_Ctx.Pix_Fmt),
         V_Width, V_Height, FFmpeg.AVUtil.AV_PIX_FMT_RGB24,
         FFmpeg.SWScale.SWS_BILINEAR, null, null, null);

      if Sws_Ctx = null then
         raise Program_Error with "sws_getContext failed";
      end if;

      --  Allocate RGB buffer
      declare
         Buf_Size : Integer := V_Width * V_Height * 3;
         Buf      : Byte_Array_Ptr := new Byte_Array (0 .. Buf_Size - 1);
      begin
         FFmpeg.AVUtil.avpicture_fill
           (RGB_FramePtr,
            Buf (0)'Address,
            FFmpeg.AVUtil.AV_PIX_FMT_RGB24,
            V_Width, V_Height);
      end;

      Width          := V_Width;
      Height         := V_Height;
      Frame_Delay_MS := Integer (1000.0 / V_FPS);

      Put_Line ("[Decoder] Frame delay (ms):" & Frame_Delay_MS'Image);
      Put_Line ("[Decoder] Ready.");
   end Open;

   -- ─────────────────────────────────────────────
   --  Close
   -- ─────────────────────────────────────────────
   procedure Close is
   begin
      if Sws_Ctx /= null then
         FFmpeg.SWScale.sws_freeContext (Sws_Ctx);
         Sws_Ctx := null;
      end if;
      if Codec_Ctx /= null then
         FFmpeg.AVCodec.avcodec_free_context (Codec_Ctx);
         Codec_Ctx := null;
      end if;
      if Fmt_Ctx /= null then
         FFmpeg.AVFormat.avformat_close_input (Fmt_Ctx);
         Fmt_Ctx := null;
      end if;
      if Raw_Frame /= null then
         FFmpeg.AVUtil.av_frame_free (Raw_Frame);
         Raw_Frame := null;
      end if;
      if RGB_FramePtr /= null then
         FFmpeg.AVUtil.av_frame_free (RGB_FramePtr);
         RGB_FramePtr := null;
      end if;
      if Packet /= null then
         FFmpeg.AVCodec.av_packet_free (Packet);
         Packet := null;
      end if;
      Put_Line ("[Decoder] Closed.");
   end Close;

   -- ─────────────────────────────────────────────
   --  Decode Control
   -- ─────────────────────────────────────────────
   procedure Start_Decoding is
   begin
      Paused_Flag := False;
      EOF_Flag    := False;
      Queue_Head  := 0;
      Queue_Tail  := 0;
      Queue_Count := 0;
      Put_Line ("[Decoder] Decoding started.");
   end Start_Decoding;

   procedure Pause  is begin Paused_Flag := True;  end Pause;
   procedure Resume is begin Paused_Flag := False; end Resume;

   function Is_EOF return Boolean is (EOF_Flag);

   -- ─────────────────────────────────────────────
   --  Seek
   -- ─────────────────────────────────────────────
   procedure Seek (Delta_Seconds : Float) is
      Current_PTS : FFmpeg.AVUtil.int64_t
                  := Fmt_Ctx.Streams (Video_Stream_Index).Cur_Dts;
      TB          : FFmpeg.AVUtil.AVRational
                  := Fmt_Ctx.Streams (Video_Stream_Index).Time_Base;
      Delta_PTS   : FFmpeg.AVUtil.int64_t
                  := FFmpeg.AVUtil.int64_t
                       (Delta_Seconds * Float (TB.Den) / Float (TB.Num));
      Target_PTS  : FFmpeg.AVUtil.int64_t := Current_PTS + Delta_PTS;
      Ret         : Interfaces.C.int;
   begin
      if Target_PTS < 0 then
         Target_PTS := 0;
      end if;

      Ret := FFmpeg.AVFormat.av_seek_frame
               (Fmt_Ctx, Video_Stream_Index, Target_PTS,
                FFmpeg.AVFormat.AVSEEK_FLAG_BACKWARD);

      if Ret < 0 then
         Put_Line ("[Decoder] Seek failed.");
      else
         FFmpeg.AVCodec.avcodec_flush_buffers (Codec_Ctx);
         --  Clear frame queue
         Queue_Head  := 0;
         Queue_Tail  := 0;
         Queue_Count := 0;
         EOF_Flag    := False;
      end if;
   end Seek;

   -- ─────────────────────────────────────────────
   --  Next_Frame (decode one frame on demand)
   -- ─────────────────────────────────────────────
   procedure Next_Frame (Frame : out RGB_Frame; Got : out Boolean) is
      Ret : Interfaces.C.int;
   begin
      Got   := False;
      Frame := (Data => null, Width => 0, Height => 0, Valid => False);

      if Paused_Flag or EOF_Flag then
         return;
      end if;

      --  Read packets until we get a video frame
      loop
         Ret := FFmpeg.AVFormat.av_read_frame (Fmt_Ctx, Packet);

         if Ret < 0 then
            --  EOF or error
            EOF_Flag := True;
            return;
         end if;

         --  Only process video packets
         if Integer (Packet.Stream_Index) = Video_Stream_Index then
            --  Send packet to decoder
            Ret := FFmpeg.AVCodec.avcodec_send_packet (Codec_Ctx, Packet);
            FFmpeg.AVCodec.av_packet_unref (Packet);

            if Ret < 0 then
               return;
            end if;

            --  Receive decoded frame
            Ret := FFmpeg.AVCodec.avcodec_receive_frame (Codec_Ctx, Raw_Frame);

            if Ret = 0 then
               --  Convert YUV → RGB
               FFmpeg.SWScale.sws_scale
                 (Sws_Ctx,
                  Raw_Frame.Data,  Raw_Frame.Linesize,
                  0, V_Height,
                  RGB_FramePtr.Data, RGB_FramePtr.Linesize);

               --  Build RGB_Frame record
               declare
                  Buf_Size : constant Integer := V_Width * V_Height * 3;
                  Buf      : Byte_Array_Ptr := new Byte_Array (0 .. Buf_Size - 1);
               begin
                  --  Copy pixel data into our managed buffer
                  for I in 0 .. Buf_Size - 1 loop
                     Buf (I) := RGB_FramePtr.Data (0)(I);
                  end loop;

                  Frame := (Data   => Buf,
                            Width  => V_Width,
                            Height => V_Height,
                            Valid  => True);
                  Got := True;
               end;

               FFmpeg.AVUtil.av_frame_unref (Raw_Frame);
               exit;

            elsif Ret = FFmpeg.AVCodec.AVERROR_EAGAIN then
               --  Need more packets
               null;
            else
               --  Decode error
               exit;
            end if;

         else
            FFmpeg.AVCodec.av_packet_unref (Packet);
         end if;
      end loop;

   end Next_Frame;

end Video_Decoder;
