-- audio.ads
-- BlackVideo Mini Player - Audio Package Specification

package Audio is

   --  Initialize SDL audio, set initial volume (0..128)
   procedure Init (Volume : Integer);

   --  Start audio decode/playback thread
   procedure Start;

   --  Pause / Resume audio
   procedure Pause;
   procedure Resume;

   --  Stop and clean up
   procedure Stop;

   --  Adjust volume (0..128)
   procedure Set_Volume (Volume : Integer);

   --  Mute / Unmute toggle
   procedure Toggle_Mute;

end Audio;
