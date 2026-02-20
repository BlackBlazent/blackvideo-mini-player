-- audio.adb
-- BlackVideo Mini Player - SDL2 Audio
--
-- Ada rules applied:
--   1. Fill_Audio needs "pragma Convention (C, Fill_Audio)" so Ada allows
--      taking its 'Access for an Audio_Callback (Convention => C) type.
--   2. Got.Freq is Interfaces.C.int — convert to Integer before Integer'Image.
--   3. SDL_MIX_MAXVOLUME is Integer in the binding — Vol is also Integer, OK.
--   4. Allow-change flags are unsigned in the binding — 'or' works fine.

with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with SDL.Audio;

package body Audio is

   use Ada.Text_IO;
   use Interfaces.C;
   use SDL.Audio;

   -- ─────────────────────────────────────────────
   --  State
   -- ─────────────────────────────────────────────
   Dev          : Audio_Device_ID := Invalid_Device;
   Vol          : Integer         := 80;
   Muted        : Boolean         := False;
   Pre_Mute_Vol : Integer         := 80;

   --  Simple ring buffer (64 KiB)
   Ring_Size  : constant := 65_536;
   Ring       : array (0 .. Ring_Size - 1) of unsigned_char;
   Ring_Read  : Integer := 0;
   Ring_Write : Integer := 0;
   Ring_Avail : Integer := 0;

   -- ─────────────────────────────────────────────
   --  Fill_Audio — SDL2 audio callback
   --
   --  MUST have Convention => C so that Fill_Audio'Access is compatible
   --  with Audio_Callback (which is also Convention => C).
   -- ─────────────────────────────────────────────
   procedure Fill_Audio
     (User_Data : System.Address;
      Stream    : System.Address;
      Len       : int);
   pragma Convention (C, Fill_Audio);  -- ← required for 'Access to work

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
      --  Clear to silence first
      for I in 0 .. N - 1 loop
         Dst (I) := 0;
      end loop;

      if Ring_Avail > 0 and then not Muted then
         declare
            To_Copy : constant Integer := Integer'Min (N, Ring_Avail);
         begin
            for I in 0 .. To_Copy - 1 loop
               Dst (I)    := Ring (Ring_Read);
               Ring_Read  := (Ring_Read + 1) mod Ring_Size;
               Ring_Avail := Ring_Avail - 1;
            end loop;
            --  Apply software volume
            SDL_MixAudioFormat
              (Stream, Stream,
               AUDIO_S16SYS,
               unsigned (To_Copy),
               Vol);     -- Vol is Integer, SDL_MixAudioFormat takes int — OK
         end;
      end if;
   end Fill_Audio;

   -- ─────────────────────────────────────────────
   --  Init
   -- ─────────────────────────────────────────────
   procedure Init (Volume : Integer) is
      Want : SDL_AudioSpec;
      Got  : SDL_AudioSpec;
   begin
      Vol := Volume;

      Want.Freq      := 44_100;
      Want.Format    := AUDIO_S16SYS;
      Want.Channels  := 2;
      Want.Samples   := 4_096;
      Want.Callback  := Fill_Audio'Access;   -- works because Convention => C
      Want.User_Data := System.Null_Address;

      Dev := SDL_OpenAudioDevice
        (Device          => Interfaces.C.Strings.Null_Ptr,
         Is_Capture      => 0,
         Desired         => Want,
         Obtained        => Got,
         Allowed_Changes => SDL_AUDIO_ALLOW_FREQUENCY_CHANGE
                            or SDL_AUDIO_ALLOW_CHANNELS_CHANGE);
         --  'or' works because both flags are 'unsigned' (modular type)

      if Dev = Invalid_Device then
         Put_Line ("[Audio] WARNING: No audio device. Silent mode.");
      else
         --  Got.Freq is Interfaces.C.int — must convert to Integer first
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
      if Dev /= Invalid_Device then
         SDL_PauseAudioDevice (Dev, 1);
      end if;
   end Pause;

   procedure Resume is
   begin
      if Dev /= Invalid_Device then
         SDL_PauseAudioDevice (Dev, 0);
      end if;
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
      --  SDL_MIX_MAXVOLUME is Integer (128) — both sides are Integer, OK
      Vol := Integer'Min (Integer'Max (Volume, 0), SDL_MIX_MAXVOLUME);
      if not Muted then
         Pre_Mute_Vol := Vol;
      end if;
   end Set_Volume;

   procedure Toggle_Mute is
   begin
      if Muted then
         Muted := False;
         Vol   := Pre_Mute_Vol;
         Put_Line ("[Audio] Unmuted. Vol=" & Integer'Image (Vol));
      else
         Pre_Mute_Vol := Vol;
         Vol          := 0;
         Muted        := True;
         Put_Line ("[Audio] Muted.");
      end if;
   end Toggle_Mute;

end Audio;
