-- player.ads
package Player is
   procedure Run (Video_File : String);

   type Player_State is (Stopped, Playing, Paused);
   function Current_State  return Player_State;
   function Current_Volume return Integer;
   function Is_Fullscreen  return Boolean;
end Player;
