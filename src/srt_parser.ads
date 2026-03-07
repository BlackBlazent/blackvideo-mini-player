-- srt_parser.ads
-- BlackVideo Mini Player — SRT Subtitle Parser  v2.3
--
-- Loads a .srt file and provides O(log n) lookup of the cue text
-- for any playback position (seconds).  All times stored as Float seconds.
-- Handles Windows CRLF, UTF-8 BOM, and multi-line cues.

package SRT_Parser is

   Max_Cues : constant := 4_096;

   type Cue is record
      Start_S : Float  := 0.0;
      End_S   : Float  := 0.0;
      Text    : String (1 .. 256) := (others => ' ');
      Len     : Natural := 0;
   end record;

   type Cue_Array is array (1 .. Max_Cues) of Cue;

   type SRT_File is record
      Cues  : Cue_Array;
      Count : Natural := 0;
   end record;

   -- Load and parse an SRT file. Returns True on success.
   procedure Load (Path : String; S : out SRT_File; OK : out Boolean);

   -- Return the cue text visible at position Pos_S (seconds).
   -- Returns "" when nothing should be displayed.
   function Current_Text (S : SRT_File; Pos_S : Float) return String;

end SRT_Parser;
