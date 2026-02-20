-- main.adb
-- BlackVideo Mini Player - Entry point

with Ada.Command_Line;
with Ada.Text_IO;
with Player;

procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
begin
   Put_Line ("BlackVideo Mini Player v1.0  (Ada + SDL2 + FFmpeg)");
   New_Line;

   if Argument_Count = 0 then
      Put_Line ("Usage:  blackvideo-player <video_file>");
      Put_Line ("        blackvideo-player --help");
      Set_Exit_Status (Failure);
      return;
   end if;

   if Argument (1) = "--help" or else Argument (1) = "-h" then
      Put_Line ("Keyboard controls:");
      Put_Line ("  SPACE        Play / Pause");
      Put_Line ("  LEFT/RIGHT   Seek -5 / +5 seconds");
      Put_Line ("  UP/DOWN      Volume +/- 10");
      Put_Line ("  M            Mute toggle");
      Put_Line ("  F            Fullscreen toggle");
      Put_Line ("  ESC / Q      Quit");
      return;
   end if;

   Player.Run (Argument (1));
end Main;
