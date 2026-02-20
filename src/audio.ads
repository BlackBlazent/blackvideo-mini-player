-- audio.ads
package Audio is
   procedure Init    (Volume : Integer);
   procedure Start;
   procedure Pause;
   procedure Resume;
   procedure Stop;
   procedure Set_Volume   (Volume : Integer);
   procedure Toggle_Mute;
end Audio;
