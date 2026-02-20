-- utils.adb
package body Utils is

   function Base_Name (Path : String) return String is
      Last_Sep : Integer := 0;
   begin
      for I in Path'Range loop
         if Path (I) = '/' or else Path (I) = '\' then
            Last_Sep := I;
         end if;
      end loop;
      if Last_Sep = 0 then
         return Path;
      else
         return Path (Last_Sep + 1 .. Path'Last);
      end if;
   end Base_Name;

   function Format_Time (Seconds : Integer) return String is
      M : constant Integer := Seconds / 60;
      S : constant Integer := Seconds mod 60;

      function Pad (N : Integer) return String is
         Raw : constant String := Integer'Image (N);
         Num : constant String := Raw (Raw'First + 1 .. Raw'Last);
      begin
         if N < 10 then return "0" & Num; else return Num; end if;
      end Pad;
   begin
      return Pad (M) & ":" & Pad (S);
   end Format_Time;

end Utils;
