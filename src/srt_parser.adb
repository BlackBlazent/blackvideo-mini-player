-- srt_parser.adb
-- BlackVideo Mini Player — SRT Subtitle Parser  v2.3

with Ada.Text_IO;
with Ada.Strings.Fixed;

package body SRT_Parser is

   use Ada.Text_IO;
   use Ada.Strings.Fixed;

   -- Parse "HH:MM:SS,mmm" → seconds as Float
   function Parse_Time (S : String) return Float is
      -- S expected format: "HH:MM:SS,mmm" (13 chars) or "H:MM:SS,mmm"
      -- We parse manually to avoid locale issues
      function Parse_Int (Str : String; From, To : Integer) return Integer is
         V : Integer := 0;
      begin
         for I in From .. To loop
            if I <= Str'Last and then Str (I) in '0' .. '9' then
               V := V * 10 + (Character'Pos (Str (I)) - Character'Pos ('0'));
            end if;
         end loop;
         return V;
      end Parse_Int;

      -- Find colon and comma positions
      C1 : Integer := 0;  -- first colon
      C2 : Integer := 0;  -- second colon
      CM : Integer := 0;  -- comma
   begin
      for I in S'Range loop
         if S (I) = ':' then
            if C1 = 0 then C1 := I;
            elsif C2 = 0 then C2 := I;
            end if;
         elsif S (I) = ',' then
            CM := I;
         end if;
      end loop;
      if C1 = 0 or else C2 = 0 or else CM = 0 then return 0.0; end if;

      declare
         H  : constant Float := Float (Parse_Int (S, S'First,  C1 - 1));
         M  : constant Float := Float (Parse_Int (S, C1 + 1,   C2 - 1));
         Sc : constant Float := Float (Parse_Int (S, C2 + 1,   CM - 1));
         Ms : constant Float := Float (Parse_Int (S, CM + 1,   S'Last));
      begin
         return H * 3600.0 + M * 60.0 + Sc + Ms / 1000.0;
      end;
   end Parse_Time;

   -- Strip trailing CR/LF/space
   function Strip (S : String) return String is
      Last : Integer := S'Last;
   begin
      while Last >= S'First and then
            (S (Last) = ASCII.CR or else S (Last) = ASCII.LF
             or else S (Last) = ' ')
      loop
         Last := Last - 1;
      end loop;
      if Last < S'First then return ""; end if;
      return S (S'First .. Last);
   end Strip;

   -- Remove UTF-8 BOM (EF BB BF) from first line
   function Strip_BOM (S : String) return String is
   begin
      if S'Length >= 3
         and then Character'Pos (S (S'First))     = 16#EF#
         and then Character'Pos (S (S'First + 1)) = 16#BB#
         and then Character'Pos (S (S'First + 2)) = 16#BF#
      then
         return S (S'First + 3 .. S'Last);
      end if;
      return S;
   end Strip_BOM;

   procedure Load (Path : String; S : out SRT_File; OK : out Boolean) is
      File   : File_Type;
      Buffer : String (1 .. 1024);
      Last   : Natural;
      Count  : Natural := 0;

      type Parse_State is (Expect_Number, Expect_Arrow, Expect_Text, Blank);
      State  : Parse_State := Expect_Number;

      Cur_Start, Cur_End : Float := 0.0;
      Cur_Text           : String (1 .. 256) := (others => ' ');
      Cur_Len            : Natural := 0;
      First_Line         : Boolean := True;
   begin
      S  := (Cues => (others => (others => <>)), Count => 0);
      OK := False;

      begin
         Open (File, In_File, Path);
      exception
         when others =>
            Ada.Text_IO.Put_Line ("[SRT] Cannot open: " & Path);
            return;
      end;

      while not End_Of_File (File) loop
         Get_Line (File, Buffer, Last);
         declare
            Line : constant String :=
              (if First_Line
               then Strip_BOM (Strip (Buffer (1 .. Last)))
               else Strip (Buffer (1 .. Last)));
         begin
            First_Line := False;

            case State is
               when Expect_Number =>
                  -- Skip the cue number line; move to expecting arrow
                  if Line'Length > 0 then
                     State := Expect_Arrow;
                  end if;

               when Expect_Arrow =>
                  -- "00:01:23,456 --> 00:01:27,890"
                  declare
                     Arrow : constant String := "-->";
                     Pos   : constant Natural :=
                       Index (Line, Arrow);
                  begin
                     if Pos > 0 then
                        Cur_Start := Parse_Time
                          (Strip (Line (Line'First .. Pos - 1)));
                        Cur_End   := Parse_Time
                          (Strip (Line (Pos + 3 .. Line'Last)));
                        Cur_Text  := (others => ' ');
                        Cur_Len   := 0;
                        State     := Expect_Text;
                     end if;
                  end;

               when Expect_Text =>
                  if Line'Length = 0 then
                     -- Blank line: cue complete
                     if Count < Max_Cues and then Cur_Len > 0 then
                        Count := Count + 1;
                        S.Cues (Count) :=
                          (Start_S => Cur_Start,
                           End_S   => Cur_End,
                           Text    => Cur_Text,
                           Len     => Cur_Len);
                     end if;
                     State := Expect_Number;
                  else
                     -- Append line to cue text (handle multi-line cues)
                     if Cur_Len > 0 and then Cur_Len < 255 then
                        Cur_Text (Cur_Len + 1) := ' ';
                        Cur_Len := Cur_Len + 1;
                     end if;
                     declare
                        Available : constant Natural :=
                          Integer'Min (Line'Length, 256 - Cur_Len);
                     begin
                        if Available > 0 then
                           Cur_Text (Cur_Len + 1 .. Cur_Len + Available) :=
                             Line (Line'First .. Line'First + Available - 1);
                           Cur_Len := Cur_Len + Available;
                        end if;
                     end;
                  end if;

               when Blank => null;
            end case;
         end;
      end loop;

      -- Flush last cue (no trailing blank line in some files)
      if State = Expect_Text and then Count < Max_Cues and then Cur_Len > 0 then
         Count := Count + 1;
         S.Cues (Count) :=
           (Start_S => Cur_Start,
            End_S   => Cur_End,
            Text    => Cur_Text,
            Len     => Cur_Len);
      end if;

      Close (File);
      S.Count := Count;
      OK := True;
      Ada.Text_IO.Put_Line ("[SRT] Loaded " & Natural'Image (Count) &
                            " cues from " & Path);
   end Load;

   function Current_Text (S : SRT_File; Pos_S : Float) return String is
   begin
      -- Linear scan — fast enough for ≤4096 cues at 60fps
      for I in 1 .. S.Count loop
         if Pos_S >= S.Cues (I).Start_S and then Pos_S < S.Cues (I).End_S then
            return S.Cues (I).Text (1 .. S.Cues (I).Len);
         end if;
         -- Early exit once we've passed the current position
         exit when S.Cues (I).Start_S > Pos_S + 1.0;
      end loop;
      return "";
   end Current_Text;

end SRT_Parser;
