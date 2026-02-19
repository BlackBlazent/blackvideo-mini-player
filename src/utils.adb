-- utils.adb
-- BlackVideo Mini Player - Utility Functions Implementation

package body Utils is

   -- ─────────────────────────────────────────────
   --  Base_Name
   --  Works on both / (Unix) and \ (Windows) paths
   -- ─────────────────────────────────────────────
   function Base_Name (Path : String) return String is
      Last_Sep : Integer := 0;
   begin
      for I in Path'Range loop
         if Path (I) = '/' or Path (I) = '\' then
            Last_Sep := I;
         end if;
      end loop;

      if Last_Sep = 0 then
         return Path;
      else
         return Path (Last_Sep + 1 .. Path'Last);
      end if;
   end Base_Name;

   -- ─────────────────────────────────────────────
   --  Format_Time  (e.g. 125 → "02:05")
   -- ─────────────────────────────────────────────
   function Format_Time (Seconds : Integer) return String is
      M    : Integer := Seconds / 60;
      S    : Integer := Seconds mod 60;

      function Pad2 (N : Integer) return String is
         Raw : constant String := Integer'Image (N);
         Trimmed : constant String := Raw (Raw'First + 1 .. Raw'Last);
      begin
         if N < 10 then
            return "0" & Trimmed;
         else
            return Trimmed;
         end if;
      end Pad2;

   begin
      return Pad2 (M) & ":" & Pad2 (S);
   end Format_Time;

end Utils;
