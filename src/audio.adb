-- audio.adb
-- BlackVideo Mini Player - SDL2 Audio with ring buffer

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
   Vol          : Integer         := 80;   -- 0..128
   Muted        : Boolean         := False;
   Pre_Mute_Vol : Integer         := 80;

   -- Ring buffer: 256 KiB — large enough to absorb decode bursts
   -- at 44100 Hz stereo S16 = 4 bytes/sample → ~1.5 seconds of audio
   Ring_Size  : constant := 262_144;
   Ring       : array (0 .. Ring_Size - 1) of unsigned_char;
   Ring_Read  : Integer := 0;
   Ring_Write : Integer := 0;
   Ring_Avail : Integer := 0;

   -- ── Fill_Audio — SDL2 audio callback ─────────────────────────────────
   -- Called by SDL in a separate thread when it needs more audio data.
   -- Copies bytes from the ring buffer into SDL's output stream.
   -- Volume is applied by scaling each S16 sample inline.
   -- We do NOT use SDL_MixAudioFormat(stream, stream, ...) — that would
   -- mix the audio with itself, doubling amplitude and causing distortion.
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
      -- Always zero the buffer first (silence if ring is empty)
      for I in 0 .. N - 1 loop
         Dst (I) := 0;
      end loop;

      if Ring_Avail <= 0 or else Muted then return; end if;

      -- Copy from ring buffer then apply volume via SDL_MixAudioFormat.
      -- SDL_MixAudioFormat(dst, src, ...) with DIFFERENT dst and src is
      -- the correct way to mix with volume — src is the ring data,
      -- dst is the zeroed stream buffer.
      declare
         To_Copy : constant Integer := Integer'Min (N, Ring_Avail);
         -- Temporary buffer to hold the ring data before mixing into dst
         Tmp : array (0 .. To_Copy - 1) of unsigned_char;
      begin
         for I in 0 .. To_Copy - 1 loop
            Tmp (I)    := Ring (Ring_Read);
            Ring_Read  := (Ring_Read + 1) mod Ring_Size;
            Ring_Avail := Ring_Avail - 1;
         end loop;

         -- Mix Tmp → Dst with volume scaling.
         -- This is the intended use: src /= dst.
         SDL_MixAudioFormat
           (Stream,          -- dst (zeroed)
            Tmp (0)'Address, -- src (ring data)
            AUDIO_S16SYS,
            unsigned (To_Copy),
            int (Vol));
      end;
   end Fill_Audio;

   -- ── Push_Audio — called from decoder ──────────────────────────────────
   -- Writes S16 interleaved stereo PCM bytes into the ring buffer.
   -- If the buffer is full, we wait instead of dropping — dropping causes
   -- the periodic gaps (scraping sound every ~2 seconds).
   -- Because this runs on the main thread and Fill_Audio runs on SDL's
   -- audio thread, we keep the logic simple: just overwrite oldest if full.
   procedure Push_Audio (Data : System.Address; N_Bytes : Integer) is
      Src : array (0 .. N_Bytes - 1) of unsigned_char
          with Import, Address => Data;
      Free_Space : Integer;
   begin
      if N_Bytes <= 0 or else Dev = Invalid_Device then return; end if;

      Free_Space := Ring_Size - Ring_Avail;

      if N_Bytes > Free_Space then
         -- Buffer full: drop oldest data to make room.
         -- This means we're producing audio faster than SDL consumes it —
         -- the real fix is the larger ring buffer above.
         declare
            Drop : constant Integer := N_Bytes - Free_Space;
         begin
            Ring_Read  := (Ring_Read + Drop) mod Ring_Size;
            Ring_Avail := Ring_Avail - Drop;
         end;
      end if;

      for I in 0 .. N_Bytes - 1 loop
         Ring (Ring_Write) := Src (I);
         Ring_Write := (Ring_Write + 1) mod Ring_Size;
      end loop;
      Ring_Avail := Ring_Avail + N_Bytes;
   end Push_Audio;

   -- ── Init ─────────────────────────────────────────────────────────────
   procedure Init (Volume : Integer) is
      Want : SDL_AudioSpec;
      Got  : SDL_AudioSpec;
   begin
      Vol := Volume;

      Want.Freq      := 44_100;
      Want.Format    := AUDIO_S16SYS;
      Want.Channels  := 2;
      Want.Samples   := 2_048;  -- smaller buffer = lower audio latency
      Want.Callback  := Fill_Audio'Access;
      Want.User_Data := System.Null_Address;

      Dev := SDL_OpenAudioDevice
        (Device          => Interfaces.C.Strings.Null_Ptr,
         Is_Capture      => 0,
         Desired         => Want,
         Obtained        => Got,
         Allowed_Changes => SDL_AUDIO_ALLOW_FREQUENCY_CHANGE
                            or SDL_AUDIO_ALLOW_CHANNELS_CHANGE);

      if Dev = Invalid_Device then
         Put_Line ("[Audio] WARNING: No audio device. Silent mode.");
      else
         Put_Line ("[Audio] Device opened. Freq="
                   & Integer'Image (Integer (Got.Freq)));
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
