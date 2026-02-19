-- player.ads
-- BlackVideo Mini Player - Player Package Specification
-- Orchestrates: video decode, audio, rendering, events

package Player is

   --  Main entry: open window, decode, render, handle events
   procedure Run (Video_File : String);

   --  Player state
   type Player_State is (Stopped, Playing, Paused);

   --  Global state (read-only from outside)
   function Current_State return Player_State;
   function Current_Volume return Integer;   -- 0..128 (SDL range)
   function Is_Fullscreen  return Boolean;

end Player;
