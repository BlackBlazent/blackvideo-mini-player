-- main.adb
-- BlackVideo Mini Player - Entry Point
-- Cross-platform: Windows / Linux / macOS
--
-- Usage:
--   blackvideo-player <video_file>
--   blackvideo-player --help

with Ada.Command_Line;
with Ada.Text_IO;
with Player;

procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
begin
   --  Print banner
   Put_Line ("╔══════════════════════════════════════╗");
   Put_Line ("║    BlackVideo Mini Player v1.0        ║");
   Put_Line ("║    Ada + SDL2 + FFmpeg                ║");
   Put_Line ("╚══════════════════════════════════════╝");
   New_Line;

   --  Check arguments
   if Argument_Count = 0 then
      Put_Line ("Usage: blackvideo-player <video_file>");
      Put_Line ("       blackvideo-player --help");
      Set_Exit_Status (Failure);
      return;
   end if;

   if Argument (1) = "--help" or Argument (1) = "-h" then
      Put_Line ("BlackVideo Mini Player");
      Put_Line ("  Supported formats: MP4, MKV, AVI, MOV, WebM, and more");
      New_Line;
      Put_Line ("Controls:");
      Put_Line ("  SPACE         - Play / Pause");
      Put_Line ("  LEFT / RIGHT  - Seek -5 / +5 seconds");
      Put_Line ("  UP / DOWN     - Volume +10% / -10%");
      Put_Line ("  M             - Mute / Unmute");
      Put_Line ("  F             - Fullscreen toggle");
      Put_Line ("  ESC / Q       - Quit");
      return;
   end if;

   --  Run player with given file
   declare
      Video_Path : constant String := Argument (1);
   begin
      Player.Run (Video_Path);
   end;

exception
   when others =>
      Put_Line ("Error: Unexpected failure in main.");
      Set_Exit_Status (Failure);
end Main;
