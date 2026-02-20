-- sdl-audio.ads
-- Package SDL.Audio — audio device + callback
-- Key Ada rules applied:
--   * Bit-flags use 'unsigned' (modular) so the 'or' operator works.
--   * Callback access type must have Convention => C.
--   * SDL_MIX_MAXVOLUME kept as Integer for easy use in Ada code.

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package SDL.Audio is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  Audio formats (unsigned_short)
   -- ─────────────────────────────────────────────
   AUDIO_U8     : constant unsigned_short := 16#0008#;
   AUDIO_S16SYS : constant unsigned_short := 16#8010#;
   AUDIO_F32SYS : constant unsigned_short := 16#8120#;

   --  Allow-change flags — MUST be unsigned (modular) for 'or' to work
   SDL_AUDIO_ALLOW_FREQUENCY_CHANGE : constant unsigned := 1;
   SDL_AUDIO_ALLOW_FORMAT_CHANGE    : constant unsigned := 2;
   SDL_AUDIO_ALLOW_CHANNELS_CHANGE  : constant unsigned := 4;
   SDL_AUDIO_ALLOW_ANY_CHANGE       : constant unsigned := 15;

   --  Max volume for SDL_MixAudioFormat
   SDL_MIX_MAXVOLUME : constant Integer := 128;

   -- ─────────────────────────────────────────────
   --  Callback — Convention => C is mandatory so Ada passes
   --  a raw C function pointer, not an Ada closure.
   -- ─────────────────────────────────────────────
   type Audio_Callback is access procedure
     (User_Data : System.Address;
      Stream    : System.Address;
      Len       : int)
   with Convention => C;

   -- ─────────────────────────────────────────────
   --  SDL_AudioSpec
   -- ─────────────────────────────────────────────
   type SDL_AudioSpec is record
      Freq      : int;
      Format    : unsigned_short;
      Channels  : unsigned_char;
      Silence   : unsigned_char;
      Samples   : unsigned_short;
      Padding   : unsigned_short;
      Size      : unsigned;
      Callback  : Audio_Callback;
      User_Data : System.Address;
   end record with Convention => C;

   -- ─────────────────────────────────────────────
   --  Audio_Device_ID
   -- ─────────────────────────────────────────────
   type Audio_Device_ID is new unsigned;
   Invalid_Device : constant Audio_Device_ID := 0;

   -- ─────────────────────────────────────────────
   --  C imports
   -- ─────────────────────────────────────────────

   function SDL_OpenAudioDevice
     (Device          : Interfaces.C.Strings.chars_ptr;
      Is_Capture      : int;
      Desired         : SDL_AudioSpec;
      Obtained        : out SDL_AudioSpec;
      Allowed_Changes : unsigned) return Audio_Device_ID   -- unsigned, not int
   with Import, Convention => C, External_Name => "SDL_OpenAudioDevice";

   procedure SDL_CloseAudioDevice (Dev : Audio_Device_ID)
   with Import, Convention => C, External_Name => "SDL_CloseAudioDevice";

   procedure SDL_PauseAudioDevice (Dev : Audio_Device_ID; Pause_On : int)
   with Import, Convention => C, External_Name => "SDL_PauseAudioDevice";

   procedure SDL_MixAudioFormat
     (Dst    : System.Address;
      Src    : System.Address;
      Format : unsigned_short;
      Len    : unsigned;
      Volume : int)
   with Import, Convention => C, External_Name => "SDL_MixAudioFormat";

end SDL.Audio;
