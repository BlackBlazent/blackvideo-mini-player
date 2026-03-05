-- sdl-audio.ads
-- Package SDL.Audio — audio device + callback

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package SDL.Audio is

   use Interfaces.C;

   AUDIO_U8     : constant unsigned_short := 16#0008#;
   AUDIO_S16SYS : constant unsigned_short := 16#8010#;
   AUDIO_F32SYS : constant unsigned_short := 16#8120#;

   SDL_AUDIO_ALLOW_FREQUENCY_CHANGE : constant unsigned := 1;
   SDL_AUDIO_ALLOW_FORMAT_CHANGE    : constant unsigned := 2;
   SDL_AUDIO_ALLOW_CHANNELS_CHANGE  : constant unsigned := 4;
   SDL_AUDIO_ALLOW_ANY_CHANGE       : constant unsigned := 15;

   SDL_MIX_MAXVOLUME : constant Integer := 128;

   type Audio_Callback is access procedure
     (User_Data : System.Address;
      Stream    : System.Address;
      Len       : int)
   with Convention => C;

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

   type Audio_Device_ID is new unsigned;
   Invalid_Device : constant Audio_Device_ID := 0;

   function SDL_OpenAudioDevice
     (Device          : Interfaces.C.Strings.chars_ptr;
      Is_Capture      : int;
      Desired         : SDL_AudioSpec;
      Obtained        : out SDL_AudioSpec;
      Allowed_Changes : unsigned) return Audio_Device_ID
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

   -- Lock/Unlock must wrap all non-callback access to shared ring buffer state
   procedure SDL_LockAudioDevice   (Dev : Audio_Device_ID)
   with Import, Convention => C, External_Name => "SDL_LockAudioDevice";

   procedure SDL_UnlockAudioDevice (Dev : Audio_Device_ID)
   with Import, Convention => C, External_Name => "SDL_UnlockAudioDevice";

end SDL.Audio;
