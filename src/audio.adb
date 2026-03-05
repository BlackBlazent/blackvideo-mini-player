-- audio.adb
-- BlackVideo Mini Player - SDL2 Audio  v2.2
--
-- Fixes vs v2.0:
--   * SDL_LockAudioDevice / SDL_UnlockAudioDevice wrap all Push_Audio
--     ring-buffer writes so there is no race with the SDL audio thread.
--     Without this, torn reads of Ring_Avail at 50min+ caused the
--     "scattered / mixed" audio glitch.
--   * Ring buffer increased to 2 MB (~12 s at 44100 Hz stereo S16)
--     so decode bursts never overflow even during 1080p H.264.
--   * SDL_AUDIO_ALLOW_* flags set to 0 — we refuse any SDL changes to
--     sample rate, format, or channel count. If SDL silently resampled
--     to a different rate the audio pitch/sync would drift over time.
--   * On overflow we now drop NEWEST data (skip the incoming burst)
--     instead of dropping oldest (which causes audible skips in
--     already-buffered audio).

with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with SDL.Audio;

package body Audio is

   use Ada.Text_IO;
   use Interfaces.C;
   use SDL.Audio;

   Dev          : Audio_Device_ID := Invalid_Device;
   Vol          : Integer         := 80;
   Muted        : Boolean         := False;
   Pre_Mute_Vol : Integer         := 80;

   -- 2 MB ring: ~12 seconds at 44100 Hz stereo S16 (4 bytes/sample)
   -- Absorbs long decode bursts that happen mid-movie with H.264 B-frames.
   Ring_Size  : constant := 2_097_152;
   Ring       : array (0 .. Ring_Size - 1) of unsigned_char;
   Ring_Read  : Integer := 0;
   Ring_Write : Integer := 0;
   Ring_Avail : Integer := 0;

   -- ── Fill_Audio — SDL2 audio callback ─────────────────────────────────
   -- Called on the SDL audio thread. The lock is already held by SDL while
   -- the callback runs, so we do NOT call Lock/Unlock here — doing so
   -- would deadlock. We just read from the ring and apply volume.
   procedure Fill_Audio
     (User_Data : System.Address;
      Stream    : System.Address;
      Len       : int);
   pragma Convention (C, Fill_Audio);

   procedure Fill_Audio
     (User_Data : System.Address;
      Stream    : System.Address;
      Len       : int)
   is
      pragma Unreferenced (User_Data);
      N   : constant Integer := Integer (Len);
      Dst : array (0 .. N - 1) of unsigned_char
          with Import, Address => Stream;
   begin
      for I in 0 .. N - 1 loop
         Dst (I) := 0;
      end loop;

      if Ring_Avail <= 0 or else Muted then return; end if;

      declare
         To_Copy : constant Integer := Integer'Min (N, Ring_Avail);
         Tmp : array (0 .. To_Copy - 1) of unsigned_char;
      begin
         for I in 0 .. To_Copy - 1 loop
            Tmp (I)    := Ring (Ring_Read);
            Ring_Read  := (Ring_Read + 1) mod Ring_Size;
            Ring_Avail := Ring_Avail - 1;
         end loop;

         SDL_MixAudioFormat
           (Stream,
            Tmp (0)'Address,
            AUDIO_S16SYS,
            unsigned (To_Copy),
            int (Vol));
      end;
   end Fill_Audio;

   -- ── Push_Audio — called from decoder thread ────────────────────────────
   -- Lock the audio device before touching any shared ring state.
   -- On overflow: drop INCOMING data (skip the burst), NOT buffered data.
   -- Dropping already-buffered audio causes audible clicks; dropping new
   -- incoming data during a burst is inaudible.
   procedure Push_Audio (Data : System.Address; N_Bytes : Integer) is
      Src : array (0 .. N_Bytes - 1) of unsigned_char
          with Import, Address => Data;
      Free_Space : Integer;
      To_Write   : Integer;
   begin
      if N_Bytes <= 0 or else Dev = Invalid_Device then return; end if;

      SDL_LockAudioDevice (Dev);

      Free_Space := Ring_Size - Ring_Avail;

      if N_Bytes > Free_Space then
         -- Buffer nearly full: only write what fits, drop the rest.
         -- This silently truncates the burst rather than corrupting
         -- already-buffered audio that is about to be played.
         To_Write := Free_Space;
      else
         To_Write := N_Bytes;
      end if;

      for I in 0 .. To_Write - 1 loop
         Ring (Ring_Write) := Src (I);
         Ring_Write := (Ring_Write + 1) mod Ring_Size;
      end loop;
      Ring_Avail := Ring_Avail + To_Write;

      SDL_UnlockAudioDevice (Dev);
   end Push_Audio;

   -- ── Flush — discard all buffered audio (e.g. after seek) ─────────────
   procedure Flush is
   begin
      if Dev = Invalid_Device then return; end if;
      SDL_LockAudioDevice (Dev);
      Ring_Read  := 0;
      Ring_Write := 0;
      Ring_Avail := 0;
      SDL_UnlockAudioDevice (Dev);
   end Flush;

   -- ── Init ─────────────────────────────────────────────────────────────
   procedure Init (Volume : Integer) is
      Want : SDL_AudioSpec;
      Got  : SDL_AudioSpec;
   begin
      Vol := Volume;

      Want.Freq      := 44_100;
      Want.Format    := AUDIO_S16SYS;
      Want.Channels  := 2;
      Want.Samples   := 4_096;  -- larger SDL buffer reduces callback frequency
      Want.Callback  := Fill_Audio'Access;
      Want.User_Data := System.Null_Address;

      Dev := SDL_OpenAudioDevice
        (Device          => Interfaces.C.Strings.Null_Ptr,
         Is_Capture      => 0,
         Desired         => Want,
         Obtained        => Got,
         -- Refuse all SDL-initiated changes: we want exactly 44100 Hz
         -- stereo S16. If SDL resamples silently, audio drifts over time.
         Allowed_Changes => 0);

      if Dev = Invalid_Device then
         Put_Line ("[Audio] WARNING: No audio device. Silent mode.");
      else
         Put_Line ("[Audio] Device opened. Freq="
                   & Integer'Image (Integer (Got.Freq))
                   & " Ch=" & Integer'Image (Integer (Got.Channels))
                   & " Samples=" & Integer'Image (Integer (Got.Samples)));
      end if;
   end Init;

   procedure Start is
   begin
      if Dev /= Invalid_Device then
         SDL_PauseAudioDevice (Dev, 0);
         Put_Line ("[Audio] Started.");
      end if;
   end Start;

   procedure Pause is
   begin
      if Dev /= Invalid_Device then SDL_PauseAudioDevice (Dev, 1); end if;
   end Pause;

   procedure Resume is
   begin
      if Dev /= Invalid_Device then SDL_PauseAudioDevice (Dev, 0); end if;
   end Resume;

   procedure Stop is
   begin
      if Dev /= Invalid_Device then
         SDL_PauseAudioDevice (Dev, 1);
         SDL_CloseAudioDevice (Dev);
         Dev := Invalid_Device;
         Put_Line ("[Audio] Stopped.");
      end if;
   end Stop;

   procedure Set_Volume (Volume : Integer) is
   begin
      Vol := Integer'Min (Integer'Max (Volume, 0), SDL_MIX_MAXVOLUME);
      if not Muted then Pre_Mute_Vol := Vol; end if;
   end Set_Volume;

   procedure Toggle_Mute is
   begin
      if Muted then
         Muted := False; Vol := Pre_Mute_Vol;
         Put_Line ("[Audio] Unmuted. Vol=" & Integer'Image (Vol));
      else
         Pre_Mute_Vol := Vol; Vol := 0; Muted := True;
         Put_Line ("[Audio] Muted.");
      end if;
   end Toggle_Mute;

end Audio;
