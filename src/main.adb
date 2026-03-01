-- main.adb
-- BlackVideo Mini Player - Entry point
-- Concatenates all CLI arguments with spaces to handle paths with spaces.

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Player;

procedure Main is
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   function Full_Path return String is
      Result : Unbounded_String;
   begin
      for I in 1 .. Argument_Count loop
         if I > 1 then
            Append (Result, " ");
         end if;
         Append (Result, Argument (I));
      end loop;
      return To_String (Result);
   end Full_Path;

begin
   Put_Line ("BlackVideo Mini Player v1.0  (Ada + SDL2 + FFmpeg)");
   New_Line;

   if Argument_Count = 0 then
      Put_Line ("Usage:  blackvideo-player <video_file>");
      Put_Line ("        blackvideo-player --help");
      Set_Exit_Status (Failure);
      return;
   end if;

   if Argument (1) = "--help" or else Argument (1) = "-help"
      or else Argument (1) = "-h"
   then
      Put_Line ("Keyboard controls:");
      Put_Line ("  SPACE        Play / Pause");
      Put_Line ("  LEFT/RIGHT   Seek -5 / +5 seconds");
      Put_Line ("  UP/DOWN      Volume +/- 10");
      Put_Line ("  M            Mute toggle");
      Put_Line ("  F            Fullscreen toggle");
      Put_Line ("  ESC / Q      Quit");
      return;
   end if;

   Player.Run (Full_Path);
end Main;
