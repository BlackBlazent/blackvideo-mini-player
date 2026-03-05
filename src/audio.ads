-- audio.ads
with System;

package Audio is
   procedure Init        (Volume : Integer);
   procedure Start;
   procedure Pause;
   procedure Resume;
   procedure Stop;
   procedure Set_Volume  (Volume : Integer);
   procedure Toggle_Mute;

   -- Push decoded S16 stereo PCM into the playback ring buffer.
   procedure Push_Audio (Data : System.Address; N_Bytes : Integer);

   -- Flush ring buffer — call immediately after any seek to prevent
   -- stale pre-seek audio from playing after the seek point.
   procedure Flush;
end Audio;
