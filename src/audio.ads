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
   -- Called from video_decoder after swr_convert produces S16 output.
   procedure Push_Audio (Data : System.Address; N_Bytes : Integer);
end Audio;
