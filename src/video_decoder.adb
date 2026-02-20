-- video_decoder.adb
-- BlackVideo Mini Player - FFmpeg decode + YUV→RGB conversion

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with System;

with FFmpeg.AVUtil;
with FFmpeg.AVCodec;
with FFmpeg.AVFormat;
with FFmpeg.SWScale;

package body Video_Decoder is

   use Ada.Text_IO;
   use Interfaces.C;          -- from video_decoder.ads (use Interfaces.C)
   use Interfaces.C.Strings;
   use FFmpeg.AVUtil;
   use FFmpeg.AVCodec;
   use FFmpeg.AVFormat;
   use FFmpeg.SWScale;

   -- ─── State ────────────────────────────────────────────────────────────
   Fmt_Ctx            : AVFormatContext_Ptr := null;
   Codec_Ctx          : AVCodecContext_Ptr  := null;
   Sws_Ctx            : SwsContext_Ptr      := null;
   Pkt                : AVPacket_Ptr        := null;
   Raw_Frame          : AVFrame_Ptr         := null;
   RGB_Frame_Internal : AVFrame_Ptr         := null;

   RGB_Buffer   : Byte_Array_Access := null;
   RGB_Data     : Plane_Data_Array;
   RGB_Linesize : Plane_Linesize_Array;

   Vid_Stream : Integer := -1;
   Aud_Stream : Integer := -1;

   V_Width  : Integer := 0;
   V_Height : Integer := 0;
   V_FPS    : Float   := 25.0;

   Paused_Flag : Boolean := False;
   EOF_Flag    : Boolean := False;

   procedure Free_Bytes is new Ada.Unchecked_Deallocation
     (Byte_Array, Byte_Array_Access);

   -- ─── Open ─────────────────────────────────────────────────────────────
   procedure Open
     (File_Path      : String;
      Width          : out Integer;
      Height         : out Integer;
      Frame_Delay_MS : out Integer)
   is
      C_Path : chars_ptr := New_String (File_Path);
      Ret    : int;
   begin
      av_log_set_level (AV_LOG_WARNING);

      Ret := avformat_open_input
               (Fmt_Ctx, C_Path, System.Null_Address, System.Null_Address);
      Free (C_Path);
      if Ret < 0 then
         raise Program_Error
           with "[Decoder] avformat_open_input failed: " & File_Path;
      end if;

      Ret := avformat_find_stream_info (Fmt_Ctx, System.Null_Address);
      if Ret < 0 then
         raise Program_Error with "[Decoder] avformat_find_stream_info failed";
      end if;

      --  Find video (and optionally audio) streams
      declare
         N : constant Natural := Natural (Fmt_Ctx.Nb_Streams);
      begin
         for I in 0 .. N - 1 loop
            declare
               Par : constant AVCodecParameters_Ptr
                   := Fmt_Ctx.Streams (I).Codecpar;
            begin
               if Par.Codec_Type = AVMEDIA_TYPE_VIDEO
                  and then Vid_Stream < 0
               then
                  Vid_Stream := I;
               elsif Par.Codec_Type = AVMEDIA_TYPE_AUDIO
                  and then Aud_Stream < 0
               then
                  Aud_Stream := I;
               end if;
            end;
         end loop;
      end;

      if Vid_Stream < 0 then
         raise Program_Error
           with "[Decoder] No video stream found in: " & File_Path;
      end if;
      Put_Line ("[Decoder] Video stream #" & Integer'Image (Vid_Stream));
      if Aud_Stream >= 0 then
         Put_Line ("[Decoder] Audio stream #" & Integer'Image (Aud_Stream));
      end if;

      --  Find + open decoder
      declare
         Par   : constant AVCodecParameters_Ptr
               := Fmt_Ctx.Streams (Vid_Stream).Codecpar;
         Codec : constant AVCodec_Ptr
               := avcodec_find_decoder (Par.Codec_Id);
      begin
         if Codec = null then
            raise Program_Error with "[Decoder] No decoder found";
         end if;

         Codec_Ctx := avcodec_alloc_context3 (Codec);
         if Codec_Ctx = null then
            raise Program_Error with "[Decoder] avcodec_alloc_context3 failed";
         end if;

         Ret := avcodec_parameters_to_context (Codec_Ctx, Par);
         if Ret < 0 then
            raise Program_Error
              with "[Decoder] avcodec_parameters_to_context failed";
         end if;

         Codec_Ctx.Thread_Count := 4;

         Ret := avcodec_open2 (Codec_Ctx, Codec, System.Null_Address);
         if Ret < 0 then
            raise Program_Error with "[Decoder] avcodec_open2 failed";
         end if;
      end;

      V_Width  := Integer (Codec_Ctx.Width);
      V_Height := Integer (Codec_Ctx.Height);

      --  FPS
      declare
         R : constant AVRational
           := Fmt_Ctx.Streams (Vid_Stream).Avg_Frame_Rate;
      begin
         if R.Num > 0 and then R.Den > 0 then
            V_FPS := Float (R.Num) / Float (R.Den);
         else
            V_FPS := 25.0;
         end if;
      end;

      Put_Line ("[Decoder] "
                & Integer'Image (V_Width) & "x" & Integer'Image (V_Height)
                & "  FPS=" & Float'Image (V_FPS));

      --  Allocate frames + packet
      Raw_Frame          := av_frame_alloc;
      RGB_Frame_Internal := av_frame_alloc;
      Pkt                := av_packet_alloc;

      --  Allocate RGB pixel buffer
      declare
         Buf_Size : constant int :=
           av_image_get_buffer_size (AV_PIX_FMT_RGB24,
                                     int (V_Width), int (V_Height), 1);
      begin
         RGB_Buffer := new Byte_Array (0 .. Natural (Buf_Size) - 1);

         Ret := av_image_fill_arrays
           (RGB_Data, RGB_Linesize,
            RGB_Buffer (0)'Address,
            AV_PIX_FMT_RGB24, int (V_Width), int (V_Height), 1);
         if Ret < 0 then
            raise Program_Error with "[Decoder] av_image_fill_arrays failed";
         end if;
      end;

      --  Build swscale context
      Sws_Ctx := sws_getContext
        (int (V_Width), int (V_Height), Codec_Ctx.Pix_Fmt,
         int (V_Width), int (V_Height), AV_PIX_FMT_RGB24,
         SWS_BILINEAR,
         System.Null_Address, System.Null_Address, System.Null_Address);

      if Sws_Ctx = null then
         raise Program_Error with "[Decoder] sws_getContext failed";
      end if;

      Width          := V_Width;
      Height         := V_Height;
      Frame_Delay_MS := Integer (1000.0 / V_FPS);
      Put_Line ("[Decoder] Ready. Delay="
                & Integer'Image (Frame_Delay_MS) & " ms");
   end Open;

   -- ─── Close ────────────────────────────────────────────────────────────
   procedure Close is
   begin
      if Sws_Ctx   /= null then sws_freeContext (Sws_Ctx);         Sws_Ctx   := null; end if;
      if Codec_Ctx /= null then avcodec_free_context (Codec_Ctx);  Codec_Ctx := null; end if;
      if Fmt_Ctx   /= null then avformat_close_input (Fmt_Ctx);    Fmt_Ctx   := null; end if;
      if Raw_Frame /= null then av_frame_free (Raw_Frame);         Raw_Frame := null; end if;
      if RGB_Frame_Internal /= null then
         av_frame_free (RGB_Frame_Internal);
         RGB_Frame_Internal := null;
      end if;
      if Pkt       /= null then av_packet_free (Pkt);              Pkt       := null; end if;
      if RGB_Buffer /= null then Free_Bytes (RGB_Buffer); end if;
      Put_Line ("[Decoder] Closed.");
   end Close;

   -- ─── Control ──────────────────────────────────────────────────────────
   procedure Start_Decoding is
   begin
      Paused_Flag := False;
      EOF_Flag    := False;
   end Start_Decoding;

   procedure Pause  is begin Paused_Flag := True;  end Pause;
   procedure Resume is begin Paused_Flag := False; end Resume;
   function  Is_EOF return Boolean is (EOF_Flag);

   -- ─── Seek ─────────────────────────────────────────────────────────────
   procedure Seek (Delta_Seconds : Float) is
      TB  : constant AVRational :=
              Fmt_Ctx.Streams (Vid_Stream).Time_Base;
      Cur : constant int64_t :=
              Fmt_Ctx.Streams (Vid_Stream).Start_Time;
      D   : constant int64_t :=
              int64_t (Delta_Seconds
                       * Float (TB.Den) / Float (TB.Num));
      Tgt : int64_t := Cur + D;
      Ret : int;
   begin
      if Tgt < 0 then Tgt := 0; end if;
      Ret := av_seek_frame
               (Fmt_Ctx, int (Vid_Stream), Tgt, AVSEEK_FLAG_BACKWARD);
      if Ret >= 0 then
         avcodec_flush_buffers (Codec_Ctx);
         EOF_Flag := False;
      else
         Put_Line ("[Decoder] Seek failed.");
      end if;
   end Seek;

   -- ─── Next_Frame ───────────────────────────────────────────────────────
   procedure Next_Frame (Frame : out RGB_Frame; Got : out Boolean) is
      Ret : int;
   begin
      Got   := False;
      Frame := (Data => null, Width => 0, Height => 0, Valid => False);

      if Paused_Flag or else EOF_Flag then
         return;
      end if;

      loop
         Ret := av_read_frame (Fmt_Ctx, Pkt);
         if Ret < 0 then
            EOF_Flag := True;
            return;
         end if;

         if Integer (Pkt.Stream_Index) = Vid_Stream then
            Ret := avcodec_send_packet (Codec_Ctx, Pkt);
            av_packet_unref (Pkt);

            if Ret = 0 then
               Ret := avcodec_receive_frame (Codec_Ctx, Raw_Frame);

               if Ret = 0 then
                  Ret := sws_scale
                    (Sws_Ctx,
                     Raw_Frame.Data, Raw_Frame.Linesize,
                     0, int (V_Height),
                     RGB_Data, RGB_Linesize);

                  av_frame_unref (Raw_Frame);

                  if Ret > 0 then
                     declare
                        Size : constant Natural := V_Width * V_Height * 3;
                        Buf  : constant Byte_Array_Access
                             := new Byte_Array (0 .. Size - 1);
                        Src  : Byte_Array (0 .. Size - 1)
                             with Import, Address => RGB_Data (0);
                     begin
                        Buf.all := Src;
                        Frame := (Data   => Buf,
                                  Width  => V_Width,
                                  Height => V_Height,
                                  Valid  => True);
                        Got := True;
                     end;
                     return;
                  end if;

               elsif Ret = AVERROR_EAGAIN then
                  null;
               else
                  return;
               end if;
            end if;

         else
            av_packet_unref (Pkt);
         end if;
      end loop;
   end Next_Frame;

end Video_Decoder;
