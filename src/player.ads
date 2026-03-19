-- player.ads
-- BlackVideo Mini Player  v2.4
package Player is
   procedure Run (Video_File : String);

   --  No_Video : welcome screen (launched with no file argument)
   --  Stopped  : file opened but not yet playing
   --  Playing  : active playback
   --  Paused   : playback suspended
   type Player_State is (No_Video, Stopped, Playing, Paused);
   function Current_State  return Player_State;
   function Current_Volume return Integer;
   function Is_Fullscreen  return Boolean;
end Player;
