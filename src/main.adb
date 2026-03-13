-- main.adb
-- BlackVideo Mini Player  v2.4
-- Entry point. Loads .env (dev only), then launches the player.
-- v2.4: no argument guard removed — passes "" so player shows welcome screen.

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Directories;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Storage_Elements;
with Player;

procedure Main is
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Interfaces.C;
   use System;
   use System.Storage_Elements;

   -- ── Load .env file ─────────────────────────────────────────────────────
   procedure Load_Env (Path : String) is
      function SetEnvironmentVariableA
        (Name, Value : Interfaces.C.Strings.chars_ptr) return int
      with Import, Convention => C,
           External_Name => "SetEnvironmentVariableA";
      File   : File_Type;
      Buffer : String (1 .. 2048);
      Last   : Natural;
   begin
      if not Ada.Directories.Exists (Path) then return; end if;
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Get_Line (File, Buffer, Last);
         declare
            Line : constant String := Buffer (1 .. Last);
            Eq   : Natural := 0;
         begin
            if Line'Length > 0 and then Line (Line'First) /= '#' then
               for I in Line'Range loop
                  if Line (I) = '=' then Eq := I; exit; end if;
               end loop;
               if Eq > Line'First and then Eq < Line'Last then
                  declare
                     Key : constant String := Line (Line'First .. Eq - 1);
                     Val : constant String := Line (Eq + 1 .. Line'Last);
                     CK  : Interfaces.C.Strings.chars_ptr :=
                       Interfaces.C.Strings.New_String (Key);
                     CV  : Interfaces.C.Strings.chars_ptr :=
                       Interfaces.C.Strings.New_String (Val);
                     Ret : int;
                  begin
                     Ret := SetEnvironmentVariableA (CK, CV);
                     Interfaces.C.Strings.Free (CK);
                     Interfaces.C.Strings.Free (CV);
                     if Ret /= 0 then
                        Put_Line ("[Env] " & Key & " = " & Val);
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;
      Close (File);
   exception
      when others => null;
   end Load_Env;

   -- ── Resolve exe directory ──────────────────────────────────────────────
   function Exe_Dir return String is
      function GetModuleFileNameA
        (HModule : System.Address;
         Buf     : System.Address;
         Size    : unsigned) return unsigned
      with Import, Convention => C, External_Name => "GetModuleFileNameA";
      Buf : String (1 .. 1024) := (others => ASCII.NUL);
      Len : unsigned;
   begin
      Len := GetModuleFileNameA (System.Null_Address, Buf (1)'Address, 1023);
      if Len = 0 then return ""; end if;
      declare
         Full : constant String := Buf (1 .. Integer (Len));
         Last : Natural := Full'Last;
      begin
         while Last > Full'First and then
               Full (Last) /= '\' and then Full (Last) /= '/'
         loop Last := Last - 1; end loop;
         return Full (Full'First .. Last);
      end;
   exception
      when others => return "";
   end Exe_Dir;

   -- ── Build full path from CLI args ──────────────────────────────────────
   function Full_Path return String is
      Result : Unbounded_String;
   begin
      for I in 1 .. Argument_Count loop
         if I > 1 then Append (Result, " "); end if;
         Append (Result, Argument (I));
      end loop;
      return To_String (Result);
   end Full_Path;

begin
   Put_Line ("BlackVideo Mini Player v2.4  (Ada + SDL2 + FFmpeg + Whisper + LLM)");
   New_Line;

   -- Load .env (dev only; silently skipped in production)
   declare
      Dir      : constant String := Exe_Dir;
      Env_Path : constant String :=
        (if Dir'Length > 0 then Dir & ".env" else ".env");
   begin
      Load_Env (Env_Path);
   end;

   -- v2.4: --help still works; otherwise pass whatever path we have
   -- (empty string "" → player enters No_Video / welcome screen state)
   if Argument_Count > 0 and then
      (Argument (1) = "--help" or else Argument (1) = "-help"
       or else Argument (1) = "-h")
   then
      Put_Line ("Usage:  blackvideo-player [video_file]");
      Put_Line ("        (no argument → welcome screen / drag-and-drop)");
      New_Line;
      Put_Line ("Keyboard controls:");
      Put_Line ("  SPACE        Play / Pause");
      Put_Line ("  LEFT/RIGHT   Seek -5 / +5 seconds");
      Put_Line ("  UP/DOWN      Volume +/- 10");
      Put_Line ("  M            Mute toggle");
      Put_Line ("  L            Loop toggle");
      Put_Line ("  F            Fullscreen toggle");
      Put_Line ("  ESC / Q      Quit");
      New_Line;
      Put_Line ("Whisper (offline captions):");
      Put_Line ("  Right-click → Generate Captions  (auto-detect language)");
      Put_Line ("  Right-click → Translate to English");
      Put_Line ("  Required: whisper-cli.exe + ggml-*.dll + model in build\");
      New_Line;
      Put_Line ("LLM cloud captions:");
      Put_Line ("  Right-click → Generate Captions (LLM) → choose provider");
      Put_Line ("  Providers: Claude, OpenAI, Gemini, DeepSeek, Grok");
      Put_Line ("  API keys stored in %APPDATA%\BlackVideo\keys.cfg");
      return;
   end if;

   Player.Run (Full_Path);
end Main;
