-- whisper_bridge.adb
-- BlackVideo Mini Player — Whisper.cpp Offline Caption Bridge  v2.3
--
-- Runs whisper-cli.exe as a subprocess. No whisper.h compilation needed —
-- we shell out just like a command-line tool, which means:
--   * Zero build-time dependency on whisper headers or libraries.
--   * The user just drops whisper-cli.exe + ggml-*.dll + model into build\.
--   * Models are swappable at any time by changing BLACKVIDEO_WHISPER_MODEL.

with Ada.Text_IO;
with Ada.Directories;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Storage_Elements;

package body Whisper_Bridge is

   use Ada.Text_IO;
   use Interfaces.C;
   use Interfaces.C.Strings;
   use System;
   use System.Storage_Elements;

   -- ── Portable C helpers ─────────────────────────────────────────────────

   function getenv (Name : chars_ptr) return chars_ptr
   with Import, Convention => C, External_Name => "getenv";

   function popen  (Cmd, Mode : chars_ptr) return System.Address
   with Import, Convention => C, External_Name => "popen";

   function pclose (F : System.Address) return int
   with Import, Convention => C, External_Name => "pclose";

   function fgets  (Buf : System.Address; N : int; F : System.Address)
     return System.Address
   with Import, Convention => C, External_Name => "fgets";

   function C_System (Cmd : chars_ptr) return int
   with Import, Convention => C, External_Name => "system";

   -- ── Helpers ────────────────────────────────────────────────────────────

   -- Get environment variable; return "" if unset.
   function Env (Name : String) return String is
      Cp  : chars_ptr       := New_String (Name);
      Res : constant chars_ptr := getenv (Cp);
   begin
      Free (Cp);
      if Res = Null_Ptr then return ""; end if;
      return Value (Res);
   end Env;

   -- Return True if file exists and is non-empty.
   function File_Exists (Path : String) return Boolean is
   begin
      return Ada.Directories.Exists (Path);
   exception
      when others => return False;
   end File_Exists;

   -- Run a shell command; return exit code.
   function Run_Cmd (Cmd : String) return Integer is
      Cp  : chars_ptr := New_String (Cmd);
      Ret : constant int := C_System (Cp);
   begin
      Free (Cp);
      return Integer (Ret);
   end Run_Cmd;

   -- Read first line from a command's stdout; strip CR/LF.
   function Read_Cmd_Line (Cmd : String) return String is
      Cc  : chars_ptr := New_String (Cmd);
      Cm  : chars_ptr := New_String ("r");
      Fp  : System.Address;
      Buf : String (1 .. 1024) := (others => ' ');
      Dummy_A : System.Address;
      Dummy_I : int;
   begin
      Fp := popen (Cc, Cm);
      Free (Cc); Free (Cm);
      if Fp = System.Null_Address then return ""; end if;
      Dummy_A := fgets (Buf (1)'Address, 1023, Fp);
      Dummy_I := pclose (Fp);
      pragma Unreferenced (Dummy_A, Dummy_I);
      for I in Buf'Range loop
         if Buf (I) = ASCII.LF or else Buf (I) = ASCII.CR
            or else Character'Pos (Buf (I)) = 0
         then
            if I = 1 then return ""; end if;
            return Buf (1 .. I - 1);
         end if;
      end loop;
      return Buf;
   end Read_Cmd_Line;

   -- Resolve the player executable directory (Windows: uses GetModuleFileName).
   function Exe_Dir return String is
      function GetModuleFileNameA
        (HModule : System.Address;
         Buf     : System.Address;
         Size    : unsigned) return unsigned
      with Import, Convention => C, External_Name => "GetModuleFileNameA";

      Buf  : String (1 .. 1024) := (others => ASCII.NUL);
      Len  : unsigned;
   begin
      Len := GetModuleFileNameA (System.Null_Address, Buf (1)'Address, 1023);
      if Len = 0 then return ""; end if;
      -- Strip filename, keep directory
      declare
         Full : constant String := Buf (1 .. Integer (Len));
         Last : Natural := Full'Last;
      begin
         while Last > Full'First and then
               Full (Last) /= '\' and then Full (Last) /= '/'
         loop
            Last := Last - 1;
         end loop;
         if Last = 0 then return ""; end if;
         return Full (Full'First .. Last);  -- includes trailing slash
      end;
   exception
      when others => return "";
   end Exe_Dir;

   -- ── Public: Find_Whisper ───────────────────────────────────────────────

   function Find_Whisper return String is
      -- 1. Explicit env override (dev / .env file)
      Env_Path : constant String := Env ("BLACKVIDEO_WHISPER_PATH");
   begin
      if Env_Path'Length > 0 and then File_Exists (Env_Path) then
         return Env_Path;
      end if;

      -- 2. Beside the player exe
      declare
         Dir  : constant String := Exe_Dir;
         Cand : constant String := Dir & "whisper-cli.exe";
      begin
         if Cand'Length > 0 and then File_Exists (Cand) then
            return Cand;
         end if;
      end;

      -- 3. On PATH
      declare
         Which : constant String := Read_Cmd_Line ("where whisper-cli.exe 2>nul");
      begin
         if Which'Length > 0 and then File_Exists (Which) then
            return Which;
         end if;
      end;

      return "";
   end Find_Whisper;

   -- ── Public: Find_Model ────────────────────────────────────────────────

   function Find_Model return String is
      -- 1. Explicit env override
      Env_Model : constant String := Env ("BLACKVIDEO_WHISPER_MODEL");
   begin
      if Env_Model'Length > 0 and then File_Exists (Env_Model) then
         return Env_Model;
      end if;

      -- 2. models\ggml-base.bin beside exe
      declare
         Dir   : constant String := Exe_Dir;
         Cand1 : constant String := Dir & "models\ggml-base.bin";
         Cand2 : constant String := Dir & "ggml-base.bin";
      begin
         if File_Exists (Cand1) then return Cand1; end if;
         if File_Exists (Cand2) then return Cand2; end if;
      end;

      return "";
   end Find_Model;

   -- ── Public: Generate ──────────────────────────────────────────────────

   function Generate
     (Video_Path : String;
      Translate  : Boolean        := False;
      Cb         : Status_Callback := null)
     return String
   is
      Whisper : constant String := Find_Whisper;
      Model   : constant String := Find_Model;

      procedure Status (Msg : String) is
      begin
         Put_Line ("[Whisper] " & Msg);
         if Cb /= null then Cb (Msg); end if;
      end Status;

   begin
      -- ── Validate prerequisites ────────────────────────────────────────
      if Whisper = "" then
         Status ("ERROR: whisper-cli.exe not found. " &
                 "Place it in build\ or set BLACKVIDEO_WHISPER_PATH.");
         return "";
      end if;

      if Model = "" then
         Status ("ERROR: model not found. " &
                 "Place ggml-base.bin in build\models\ " &
                 "or set BLACKVIDEO_WHISPER_MODEL.");
         return "";
      end if;

      if not File_Exists (Video_Path) then
         Status ("ERROR: video file not found: " & Video_Path);
         return "";
      end if;

      -- ── Step 1: Extract audio to 16-kHz mono WAV ─────────────────────
      -- whisper-cli needs 16kHz mono PCM WAV (its required input format).
      -- We use the ffmpeg from PATH (or the player's own ffmpeg DLLs via
      -- ffmpeg.exe if present beside the player).
      declare
         Tmp_WAV : constant String := Video_Path & ".whisper_tmp.wav";
         Ffmpeg  : constant String :=
           (if File_Exists (Exe_Dir & "ffmpeg.exe")
            then Exe_Dir & "ffmpeg.exe"
            else "ffmpeg");

         -- Build ffmpeg command
         Ffcmd : constant String :=
           """" & Ffmpeg & """ -y -i """ & Video_Path & """ " &
           "-ar 16000 -ac 1 -f wav """ & Tmp_WAV & """ 2>nul";

         Ret : Integer;
      begin
         Status ("Extracting audio...");
         Ret := Run_Cmd (Ffcmd);
         if Ret /= 0 or else not File_Exists (Tmp_WAV) then
            Status ("ERROR: audio extraction failed (ffmpeg returned " &
                    Integer'Image (Ret) & ").");
            return "";
         end if;

         -- ── Step 2: Run whisper-cli ────────────────────────────────────
         -- Build the base name for output: whisper-cli writes <base>.srt
         declare
            -- Output goes beside the video, same name + .srt
            Out_Base : constant String := Video_Path;
            -- whisper-cli -m model -f wav --output-srt [--translate] -o base
            Trans_Flag : constant String :=
              (if Translate then " --translate" else "");
            Wcmd : constant String :=
              """" & Whisper & """ " &
              "-m """ & Model & """ " &
              "-f """ & Tmp_WAV & """ " &
              "--output-srt" & Trans_Flag & " " &
              "-of """ & Out_Base & """ " &
              "2>&1";

            Out_SRT : constant String := Out_Base & ".srt";
            Ret2    : Integer;
         begin
            if Translate then
               Status ("Running Whisper (transcribe + translate to English)...");
            else
               Status ("Running Whisper (transcribing)...");
            end if;
            Status ("Model: " & Model);
            Status ("This may take a few minutes...");

            Ret2 := Run_Cmd (Wcmd);

            -- Clean up temp WAV regardless
            declare
               Del_Cmd : constant String :=
                 "del /q """ & Tmp_WAV & """ 2>nul";
               Dummy : Integer := Run_Cmd (Del_Cmd);
            begin
               pragma Unreferenced (Dummy);
            end;

            if Ret2 /= 0 then
               Status ("ERROR: whisper-cli failed (exit " &
                       Integer'Image (Ret2) & ").");
               return "";
            end if;

            if not File_Exists (Out_SRT) then
               Status ("ERROR: whisper did not write " & Out_SRT);
               return "";
            end if;

            Status ("Done. SRT saved: " & Out_SRT);
            return Out_SRT;
         end;
      end;
   end Generate;

end Whisper_Bridge;
