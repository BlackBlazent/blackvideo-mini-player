-- video_decoder.adb
-- BlackVideo Mini Player - FFmpeg video + audio decode pipeline

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with System;

with FFmpeg.AVUtil;
with FFmpeg.AVCodec;
with FFmpeg.AVFormat;
with FFmpeg.SWScale;
with FFmpeg.SWResample;
with Audio;

package body Video_Decoder is

   use Ada.Text_IO;
   use System;
   use Interfaces.C.Strings;
   use FFmpeg.AVUtil;
   use FFmpeg.AVCodec;
   use FFmpeg.AVFormat;
   use FFmpeg.SWScale;
   use FFmpeg.SWResample;

   -- ── Video state ───────────────────────────────────────────────────────
   Fmt_Ctx       : AVFormatContext_Ptr := Null_Format_Ctx;
   Vid_Codec_Ctx : AVCodecContext_Ptr  := Null_Codec_Ctx;
   Sws_Ctx       : SwsContext_Ptr      := null;
   Pkt           : AVPacket_Ptr        := null;
   Raw_Frame     : AVFrame_Ptr         := null;

   RGB_Buffer   : Byte_Array_Access := null;
   RGB_Data     : Plane_Data_Array;
   RGB_Linesize : Plane_Linesize_Array;

   Vid_Stream : int     := -1;
   V_Width    : Integer := 0;
   V_Height   : Integer := 0;
   V_FPS      : Float   := 25.0;
   V_Pix_Fmt  : int     := 0;

   -- ── Audio state ───────────────────────────────────────────────────────
   Aud_Stream    : int                := -1;
   Aud_Codec_Ctx : AVCodecContext_Ptr := Null_Codec_Ctx;
   Swr_Ctx       : SwrContext_Ptr     := null;
   Aud_Frame     : AVFrame_Ptr        := null;

   -- PCM output buffer: 8192 stereo S16 samples = 32 KiB
   Pcm_Max_Samples : constant := 8_192;
   type PCM_Buffer is
     array (0 .. Pcm_Max_Samples * 2 * 2 - 1) of Interfaces.C.unsigned_char;
   Pcm_Buf : PCM_Buffer;

   Paused_Flag : Boolean := False;
   EOF_Flag    : Boolean := False;

   procedure Free_Bytes is new Ada.Unchecked_Deallocation
     (Byte_Array, Byte_Array_Access);

   -- ── Open ─────────────────────────────────────────────────────────────
   procedure Open
     (File_Path      : String;
      Width          : out Integer;
      Height         : out Integer;
      Frame_Delay_MS : out Integer)
   is
      C_Path : chars_ptr := New_String (File_Path);
      Ret    : int;
   begin
      Width := 0; Height := 0; Frame_Delay_MS := 40;

      bv_suppress_logs;

      Ret := avformat_open_input
               (Fmt_Ctx, C_Path, System.Null_Address, System.Null_Address);
      Free (C_Path);
      if Ret < 0 then
         raise Program_Error with "[Decoder] Cannot open: " & File_Path;
      end if;

      Ret := avformat_find_stream_info (Fmt_Ctx, System.Null_Address);
      if Ret < 0 then
         raise Program_Error with "[Decoder] Cannot read stream info";
      end if;

      -- ── Find streams ──────────────────────────────────────────────────
      Vid_Stream := av_find_best_stream
        (Fmt_Ctx, AVMEDIA_TYPE_VIDEO, -1, -1, System.Null_Address, 0);
      if Vid_Stream < 0 then
         raise Program_Error with "[Decoder] No video stream in: " & File_Path;
      end if;

      Aud_Stream := av_find_best_stream
        (Fmt_Ctx, AVMEDIA_TYPE_AUDIO, -1, Vid_Stream, System.Null_Address, 0);

      -- ── Open video codec ──────────────────────────────────────────────
      declare
         Par   : constant AVCodecParameters_Ptr :=
                   Get_Stream_Codecpar (Fmt_Ctx, Vid_Stream);
         Codec : AVCodec_Ptr;
      begin
         if Par = Null_Codecpar then
            raise Program_Error with "[Decoder] No video codec parameters";
         end if;

         V_Width   := Integer (Get_Codecpar_Width   (Par));
         V_Height  := Integer (Get_Codecpar_Height  (Par));
         V_Pix_Fmt := Get_Codecpar_Pix_Fmt          (Par);

         if V_Width <= 0 or else V_Height <= 0 then
            raise Program_Error with "[Decoder] Invalid video dimensions";
         end if;

         Put_Line ("[Decoder] Video #" & int'Image (Vid_Stream)
                   & "  " & Integer'Image (V_Width)
                   & "x" & Integer'Image (V_Height)
                   & "  pix_fmt=" & int'Image (V_Pix_Fmt));

         Codec := avcodec_find_decoder (Get_Codecpar_Codec_Id (Par));
         if Codec = Null_Codec then
            raise Program_Error with "[Decoder] No video decoder found";
         end if;

         Vid_Codec_Ctx := avcodec_alloc_context3 (Codec);
         if Vid_Codec_Ctx = Null_Codec_Ctx then
            raise Program_Error with "[Decoder] Cannot alloc video codec context";
         end if;

         Ret := avcodec_parameters_to_context (Vid_Codec_Ctx, Par);
         if Ret < 0 then
            raise Program_Error with "[Decoder] Cannot copy video codec params";
         end if;

         Ret := avcodec_open2 (Vid_Codec_Ctx, Codec, System.Null_Address);
         if Ret < 0 then
            raise Program_Error with "[Decoder] Cannot open video codec";
         end if;
      end;

      -- ── Open audio codec + resampler ──────────────────────────────────
      if Aud_Stream >= 0 then
         declare
            Par       : constant AVCodecParameters_Ptr :=
                          Get_Stream_Codecpar (Fmt_Ctx, Aud_Stream);
            Codec     : AVCodec_Ptr;
            In_Rate   : int;
            In_Fmt    : int;
            In_Layout : unsigned_long;
         begin
            if Par /= Null_Codecpar then
               In_Rate   := Get_Codecpar_Sample_Rate    (Par);
               In_Fmt    := Get_Codecpar_Sample_Fmt     (Par);
               In_Layout := Get_Codecpar_Channel_Layout (Par);

               if In_Layout = 0 then
                  In_Layout := (if Get_Codecpar_Channels (Par) = 1
                                then AV_CH_LAYOUT_MONO
                                else AV_CH_LAYOUT_STEREO);
               end if;

               Codec := avcodec_find_decoder (Get_Codecpar_Codec_Id (Par));
               if Codec /= Null_Codec then
                  Aud_Codec_Ctx := avcodec_alloc_context3 (Codec);
                  if Aud_Codec_Ctx /= Null_Codec_Ctx then
                     Ret := avcodec_parameters_to_context (Aud_Codec_Ctx, Par);
                     if Ret >= 0 then
                        Ret := avcodec_open2 (Aud_Codec_Ctx, Codec,
                                              System.Null_Address);
                        if Ret >= 0 then
                           Swr_Ctx := bv_swr_setup
                             (AV_CH_LAYOUT_STEREO, AV_SAMPLE_FMT_S16, 44_100,
                              In_Layout, In_Fmt, In_Rate);
                           Aud_Frame := av_frame_alloc;
                           Put_Line ("[Decoder] Audio #" & int'Image (Aud_Stream)
                                     & "  rate=" & int'Image (In_Rate)
                                     & "  fmt=" & int'Image (In_Fmt));
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;

      if Aud_Codec_Ctx = Null_Codec_Ctx then
         Put_Line ("[Decoder] No audio decoder — silent playback.");
      end if;

      -- ── Get FPS ───────────────────────────────────────────────────────
      declare
         Num, Den : int;
      begin
         Get_Stream_Framerate (Fmt_Ctx, Vid_Stream, Num, Den);
         if Num > 0 and then Den > 0 then
            V_FPS := Float (Num) / Float (Den);
         else
            Get_Stream_Timebase (Fmt_Ctx, Vid_Stream, Num, Den);
            if Num > 0 and then Den > 0 then
               V_FPS := Float (Den) / Float (Num);
               if V_FPS > 120.0 or else V_FPS < 1.0 then
                  V_FPS := 25.0;
               end if;
            else
               V_FPS := 25.0;
            end if;
         end if;
         Put_Line ("[Decoder] FPS=" & Float'Image (V_FPS));
      end;

      -- ── Alloc video frame + packet ────────────────────────────────────
      Raw_Frame := av_frame_alloc;
      Pkt       := av_packet_alloc;
      if Raw_Frame = null or else Pkt = null then
         raise Program_Error with "[Decoder] Cannot alloc frame/packet";
      end if;

      -- ── Alloc RGB buffer (reused every frame — no per-frame allocation) ─
      declare
         Buf_Size : constant int :=
           av_image_get_buffer_size (AV_PIX_FMT_RGB24,
                                     int (V_Width), int (V_Height), 1);
      begin
         if Buf_Size <= 0 then
            raise Program_Error with "[Decoder] Cannot compute RGB buffer size";
         end if;
         RGB_Buffer := new Byte_Array (0 .. Natural (Buf_Size) - 1);
         Ret := av_image_fill_arrays
           (RGB_Data, RGB_Linesize,
            RGB_Buffer (0)'Address,
            AV_PIX_FMT_RGB24, int (V_Width), int (V_Height), 1);
         if Ret < 0 then
            raise Program_Error with "[Decoder] av_image_fill_arrays failed";
         end if;
      end;

      -- ── Build swscale context ─────────────────────────────────────────
      Sws_Ctx := sws_getContext
        (int (V_Width), int (V_Height), V_Pix_Fmt,
         int (V_Width), int (V_Height), AV_PIX_FMT_RGB24,
         SWS_BILINEAR,
         System.Null_Address, System.Null_Address, System.Null_Address);
      if Sws_Ctx = null then
         raise Program_Error with "[Decoder] sws_getContext failed";
      end if;

      Width          := V_Width;
      Height         := V_Height;
      Frame_Delay_MS := Integer (1000.0 / V_FPS);
      Put_Line ("[Decoder] Ready — delay " & Integer'Image (Frame_Delay_MS)
                & " ms");
   end Open;

   -- ── Close ────────────────────────────────────────────────────────────
   procedure Close is
   begin
      if Swr_Ctx       /= null           then swr_free (Swr_Ctx);                             end if;
      if Aud_Frame     /= null           then av_frame_free (Aud_Frame); Aud_Frame := null;   end if;
      if Aud_Codec_Ctx /= Null_Codec_Ctx then avcodec_free_context (Aud_Codec_Ctx);           end if;
      if Sws_Ctx       /= null           then sws_freeContext (Sws_Ctx); Sws_Ctx := null;     end if;
      if Vid_Codec_Ctx /= Null_Codec_Ctx then avcodec_free_context (Vid_Codec_Ctx);           end if;
      if Fmt_Ctx       /= Null_Format_Ctx then avformat_close_input (Fmt_Ctx);                end if;
      if Raw_Frame     /= null           then av_frame_free (Raw_Frame); Raw_Frame := null;   end if;
      if Pkt           /= null           then av_packet_free (Pkt);      Pkt := null;         end if;
      if RGB_Buffer    /= null           then Free_Bytes (RGB_Buffer);                        end if;
      Put_Line ("[Decoder] Closed.");
   end Close;

   procedure Start_Decoding is begin Paused_Flag := False; EOF_Flag := False; end;
   procedure Pause            is begin Paused_Flag := True;  end;
   procedure Resume           is begin Paused_Flag := False; end;
   function  Is_EOF return Boolean is (EOF_Flag);

   -- ── Seek ─────────────────────────────────────────────────────────────
   procedure Seek (Delta_Seconds : Float) is
      Num, Den : int;
      D, Tgt   : int64_t;
      Ret      : int;
   begin
      Get_Stream_Timebase (Fmt_Ctx, Vid_Stream, Num, Den);
      if Num <= 0 or else Den <= 0 then return; end if;
      D   := int64_t (Delta_Seconds * Float (Den) / Float (Num));
      Tgt := Get_Stream_Start_Time (Fmt_Ctx, Vid_Stream) + D;
      if Tgt < 0 then Tgt := 0; end if;
      Ret := av_seek_frame (Fmt_Ctx, Vid_Stream, Tgt, AVSEEK_FLAG_BACKWARD);
      if Ret >= 0 then
         avcodec_flush_buffers (Vid_Codec_Ctx);
         if Aud_Codec_Ctx /= Null_Codec_Ctx then
            avcodec_flush_buffers (Aud_Codec_Ctx);
         end if;
         EOF_Flag := False;
      end if;
   end Seek;

   -- ── Decode audio packet → push S16 PCM to ring buffer ────────────────
   -- Uses bv_swr_convert_frame (C helper) which correctly passes ALL
   -- audio plane pointers for planar formats like FLTP (fmt=8).
   -- The old approach (passing only data[0] from Ada) was broken for
   -- planar AAC/MP3 — only the left channel fed the resampler, producing
   -- the scraping/raspy sound every ~2 seconds.
   procedure Decode_Audio_Packet is
      Ret     : int;
      Out_Ptr : constant System.Address := Pcm_Buf (0)'Address;
   begin
      if Aud_Codec_Ctx = Null_Codec_Ctx or else Swr_Ctx = null
         or else Aud_Frame = null
      then return; end if;

      Ret := avcodec_send_packet (Aud_Codec_Ctx, Pkt);
      if Ret < 0 then return; end if;

      loop
         Ret := avcodec_receive_frame (Aud_Codec_Ctx, Aud_Frame);
         exit when Ret /= 0;

         declare
            Got : constant int :=
              bv_swr_convert_frame (Swr_Ctx, Out_Ptr, Pcm_Max_Samples,
                                    Aud_Frame);
         begin
            av_frame_unref (Aud_Frame);
            if Got > 0 then
               -- Got output samples * 2 channels * 2 bytes/sample (S16)
               Audio.Push_Audio (Out_Ptr, Integer (Got) * 2 * 2);
            end if;
         end;
      end loop;
   end Decode_Audio_Packet;

   -- ── Next_Frame ───────────────────────────────────────────────────────
   -- FIX: RGB_Buffer is reused — caller must NOT free Frame.Data.
   -- The frame data pointer is valid until the next call to Next_Frame.
   -- This eliminates the 6.5 MB-per-frame heap allocation that caused
   -- STORAGE_ERROR (heap exhausted) within seconds of playback.
   procedure Next_Frame (Frame : out RGB_Frame; Got : out Boolean) is
      Ret      : int;
      Pkt_Strm : int;
   begin
      Got   := False;
      Frame := (Data => null, Width => 0, Height => 0, Valid => False);
      if Paused_Flag or else EOF_Flag then return; end if;

      loop
         Ret := av_read_frame (Fmt_Ctx, Pkt);
         if Ret < 0 then EOF_Flag := True; return; end if;

         Pkt_Strm := Get_Packet_Stream_Index (Pkt);

         if Pkt_Strm = Vid_Stream then
            Ret := avcodec_send_packet (Vid_Codec_Ctx, Pkt);
            av_packet_unref (Pkt);

            if Ret = 0 then
               Ret := avcodec_receive_frame (Vid_Codec_Ctx, Raw_Frame);

               if Ret = 0 then
                  Ret := sws_scale
                    (Sws_Ctx,
                     Raw_Frame.Data, Raw_Frame.Linesize,
                     0, int (V_Height),
                     RGB_Data, RGB_Linesize);
                  av_frame_unref (Raw_Frame);

                  if Ret > 0 then
                     -- Point directly into the reused RGB_Buffer — no copy,
                     -- no allocation, no heap growth.
                     Frame := (Data   => RGB_Buffer,
                               Width  => V_Width,
                               Height => V_Height,
                               Valid  => True);
                     Got := True;
                     return;
                  end if;

               elsif Ret = AVERROR_EAGAIN then null;
               else EOF_Flag := True; return;
               end if;
            end if;

         elsif Pkt_Strm = Aud_Stream then
            Decode_Audio_Packet;
            av_packet_unref (Pkt);

         else
            av_packet_unref (Pkt);
         end if;
      end loop;
   end Next_Frame;

end Video_Decoder;
