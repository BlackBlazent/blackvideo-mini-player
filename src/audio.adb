-- audio.adb
-- BlackVideo Mini Player - SDL2 Audio Implementation
--
-- Uses SDL2 audio callback model with FFmpeg audio decode.
-- Resamples any audio format → SDL-requested format using swresample.

with Ada.Text_IO;
with Interfaces.C;
with System;

with SDL.Audio;
with FFmpeg.AVFormat;
with FFmpeg.AVCodec;
with FFmpeg.AVUtil;
with FFmpeg.SWResample;

package body Audio is

   use Ada.Text_IO;
   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  Internal State
   -- ─────────────────────────────────────────────
   Audio_Codec_Ctx  : FFmpeg.AVCodec.AVCodecContext_Ptr := null;
   Swr_Ctx          : FFmpeg.SWResample.SwrContext_Ptr  := null;
   Audio_Device_ID  : SDL.Audio.Audio_Device_ID := 0;

   Current_Volume   : Integer := 80;
   Muted            : Boolean := False;
   Pre_Mute_Volume  : Integer := 80;
   Running          : Boolean := False;

   --  Ring buffer for decoded PCM samples
   Ring_Cap    : constant := 65536;
   Ring_Buf    : array (0 .. Ring_Cap - 1) of Interfaces.C.unsigned_char;
   Ring_Read   : Integer := 0;
   Ring_Write  : Integer := 0;
   Ring_Avail  : Integer := 0;

   -- ─────────────────────────────────────────────
   --  SDL audio callback (called from SDL audio thread)
   -- ─────────────────────────────────────────────
   procedure Audio_Callback
     (User_Data : System.Address;
      Stream    : System.Address;
      Len       : Interfaces.C.int)
   is
      N       : Integer := Integer (Len);
      Written : Integer := 0;
      Dst     : array (0 .. N - 1) of Interfaces.C.unsigned_char
              with Import, Address => Stream;
   begin
      --  Fill silence first
      for I in 0 .. N - 1 loop
         Dst (I) := 0;
      end loop;

      --  Copy from ring buffer
      if Ring_Avail > 0 and not Muted then
         Written := Integer'Min (N, Ring_Avail);
         for I in 0 .. Written - 1 loop
            Dst (I) := Ring_Buf (Ring_Read);
            Ring_Read  := (Ring_Read + 1) mod Ring_Cap;
            Ring_Avail := Ring_Avail - 1;
         end loop;

         --  Apply volume via SDL MixAudio
         SDL.Audio.Mix_Audio_Stream (Stream, Stream, Len, Current_Volume);
      end if;
   end Audio_Callback;

   -- ─────────────────────────────────────────────
   --  Init
   -- ─────────────────────────────────────────────
   procedure Init (Volume : Integer) is
      Want : SDL.Audio.Audio_Spec;
      Got  : SDL.Audio.Audio_Spec;
   begin
      Current_Volume := Volume;

      Want.Freq     := 44100;
      Want.Format   := SDL.Audio.AUDIO_S16SYS;
      Want.Channels := 2;
      Want.Samples  := 4096;
      Want.Callback := Audio_Callback'Access;
      Want.User_Data := System.Null_Address;

      Audio_Device_ID := SDL.Audio.SDL_OpenAudioDevice
        (null, 0, Want, Got,
         SDL.Audio.SDL_AUDIO_ALLOW_FREQUENCY_CHANGE
         or SDL.Audio.SDL_AUDIO_ALLOW_CHANNELS_CHANGE);

      if Audio_Device_ID = 0 then
         Put_Line ("[Audio] WARNING: Could not open audio device. Continuing without audio.");
      else
         Put_Line ("[Audio] Audio device opened. Freq:" & Integer (Got.Freq)'Image);
      end if;
   end Init;

   -- ─────────────────────────────────────────────
   --  Start
   -- ─────────────────────────────────────────────
   procedure Start is
   begin
      if Audio_Device_ID /= 0 then
         SDL.Audio.SDL_PauseAudioDevice (Audio_Device_ID, 0);  -- 0 = play
         Running := True;
         Put_Line ("[Audio] Playback started.");
      end if;
   end Start;

   -- ─────────────────────────────────────────────
   --  Pause / Resume
   -- ─────────────────────────────────────────────
   procedure Pause is
   begin
      if Audio_Device_ID /= 0 then
         SDL.Audio.SDL_PauseAudioDevice (Audio_Device_ID, 1);
      end if;
   end Pause;

   procedure Resume is
   begin
      if Audio_Device_ID /= 0 then
         SDL.Audio.SDL_PauseAudioDevice (Audio_Device_ID, 0);
      end if;
   end Resume;

   -- ─────────────────────────────────────────────
   --  Stop
   -- ─────────────────────────────────────────────
   procedure Stop is
   begin
      Running := False;
      if Audio_Device_ID /= 0 then
         SDL.Audio.SDL_PauseAudioDevice (Audio_Device_ID, 1);
         SDL.Audio.SDL_CloseAudioDevice (Audio_Device_ID);
         Audio_Device_ID := 0;
      end if;
      Put_Line ("[Audio] Stopped.");
   end Stop;

   -- ─────────────────────────────────────────────
   --  Set_Volume
   -- ─────────────────────────────────────────────
   procedure Set_Volume (Volume : Integer) is
   begin
      Current_Volume := Integer'Min (Integer'Max (Volume, 0), 128);
      if not Muted then
         Pre_Mute_Volume := Current_Volume;
      end if;
   end Set_Volume;

   -- ─────────────────────────────────────────────
   --  Toggle_Mute
   -- ─────────────────────────────────────────────
   procedure Toggle_Mute is
   begin
      if Muted then
         Muted          := False;
         Current_Volume := Pre_Mute_Volume;
         Put_Line ("[Audio] Unmuted. Volume:" & Current_Volume'Image);
      else
         Pre_Mute_Volume := Current_Volume;
         Muted           := True;
         Current_Volume  := 0;
         Put_Line ("[Audio] Muted.");
      end if;
   end Toggle_Mute;

end Audio;
