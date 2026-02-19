-- bindings/sdl2/sdl-audio.ads
-- Ada bindings for SDL2 audio subsystem

with Interfaces.C;
with System;

package SDL.Audio is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  SDL Audio Formats
   -- ─────────────────────────────────────────────
   AUDIO_U8     : constant unsigned_short := 16#0008#;
   AUDIO_S16LSB : constant unsigned_short := 16#8010#;
   AUDIO_S16MSB : constant unsigned_short := 16#9010#;
   AUDIO_S16SYS : constant unsigned_short := 16#8010#;  -- platform native
   AUDIO_F32SYS : constant unsigned_short := 16#8120#;

   --  Allow flags for SDL_OpenAudioDevice
   SDL_AUDIO_ALLOW_FREQUENCY_CHANGE : constant int := 1;
   SDL_AUDIO_ALLOW_FORMAT_CHANGE    : constant int := 2;
   SDL_AUDIO_ALLOW_CHANNELS_CHANGE  : constant int := 4;
   SDL_AUDIO_ALLOW_ANY_CHANGE       : constant int := 15;

   -- ─────────────────────────────────────────────
   --  SDL_AudioSpec
   -- ─────────────────────────────────────────────
   type Audio_Callback_Ptr is access procedure
     (User_Data : System.Address;
      Stream    : System.Address;
      Len       : int)
   with Convention => C;

   type Audio_Spec is record
      Freq     : int;
      Format   : unsigned_short;
      Channels : unsigned_char;
      Silence  : unsigned_char;
      Samples  : unsigned_short;
      Padding  : unsigned_short;
      Size     : unsigned;
      Callback : Audio_Callback_Ptr;
      User_Data : System.Address;
   end record with Convention => C;

   -- ─────────────────────────────────────────────
   --  Audio Device ID
   -- ─────────────────────────────────────────────
   type Audio_Device_ID is new unsigned;

   -- ─────────────────────────────────────────────
   --  API Functions
   -- ─────────────────────────────────────────────

   function SDL_OpenAudioDevice
     (Device          : Interfaces.C.Strings.chars_ptr;
      Is_Capture      : int;
      Desired         : Audio_Spec;
      Obtained        : out Audio_Spec;
      Allowed_Changes : int) return Audio_Device_ID
   with Import, Convention => C, External_Name => "SDL_OpenAudioDevice";

   procedure SDL_CloseAudioDevice (Dev : Audio_Device_ID)
   with Import, Convention => C, External_Name => "SDL_CloseAudioDevice";

   procedure SDL_PauseAudioDevice (Dev : Audio_Device_ID; Pause_On : int)
   with Import, Convention => C, External_Name => "SDL_PauseAudioDevice";

   --  SDL_MixAudioFormat for volume control
   procedure Mix_Audio_Stream
     (Dst    : System.Address;
      Src    : System.Address;
      Len    : int;
      Volume : int)
   with Import, Convention => C, External_Name => "SDL_MixAudioFormat";

end SDL.Audio;
