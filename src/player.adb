-- player.adb
-- BlackVideo Mini Player — Core Player  v2.4
--
-- v2.4 changes:
--   NEW: No_Video / welcome screen — launched with no argument, shows a
--     welcome screen with Open File button and drag-and-drop support.
--     SDL_DROPFILE events accepted at any time (welcome or playback).
--
--   NEW: Sprite thumbnail preview — hover seek bar shows a JPEG frame
--     at that position via Thumb_Cache + ui_overlay.c bv_tp_draw.
--
--   NEW: LLM cloud caption generation — five providers (Claude, OpenAI,
--     Gemini, DeepSeek, Grok) via LLM_Bridge; two-step transcript→SRT.
--     API key input overlay via UI_Overlay.Show_Key_Input.
--
--   NEW: Whisper progress % — stderr pipe via bv_create_process_with_pipe;
--     Check_Whisper_Progress reads stderr, parses "progress = NN%".
--
--   NEW: Whisper uses CreateProcess (via bv_create_process_with_pipe)
--     instead of WinExec.  Sentinel .done file polling.
--     Path-length guard writes long commands to a temp .bat.
--     30-minute stall timeout with console warning.
--
--   NEW: Auto-updater — Updater.Start_Check on init; update badge on
--     menu button when newer version available; overlay on request.
--
--   BUG FIX (from v2.3): Ctx_Close_Tick guard (250ms) still in effect.

with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Interfaces.C;
with Interfaces.C.Strings;
with System;

with SDL;
with SDL.Video;
with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL.Events;
with SDL.Events.Keyboards;

with Video_Decoder;
with Renderer;
with Audio;
with Utils;
with UI_Overlay;
with Whisper_Bridge;
with SRT_Parser;
with Thumb_Cache;
with LLM_Bridge;
with Updater;

package body Player is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Interfaces.C;
   use System;
   use SDL.Events;
   use SDL.Events.Keyboards;

   -- ── Player state ────────────────────────────────────────────────────────
   State      : Player_State     := No_Video;
   Volume     : Integer          := 80;
   Fullscreen : Boolean          := False;
   Looping    : Boolean          := False;
   Muted      : Boolean          := False;
   Speed_Idx  : UI_Overlay.Speed_Index := 1;

   -- ── Subtitle state (up to 3 tracks) ──────────────────────────────────
   Sub_Path   : array (0 .. 2) of Unbounded_String :=
     (others => To_Unbounded_String (""));
   Sub_Count  : Integer := 0;
   Sub_Active : Integer := -1;

   Active_SRT    : SRT_Parser.SRT_File;
   Active_SRT_OK : Boolean := False;

   -- ── Context menu guard ────────────────────────────────────────────────
   Ctx_Close_Tick : unsigned := 0;
   CTX_GUARD_MS   : constant unsigned := 250;

   -- ── Whisper background job ────────────────────────────────────────────
   Whisper_Running    : Boolean         := False;
   Whisper_Out_SRT    : Unbounded_String := To_Unbounded_String ("");
   Whisper_Video_Path : Unbounded_String := To_Unbounded_String ("");
   Whisper_H_Process  : System.Address  := System.Null_Address;
   Whisper_H_Pipe     : System.Address  := System.Null_Address;
   Whisper_Start_Tick : unsigned         := 0;
   WHISPER_TIMEOUT_MS : constant unsigned := 30 * 60 * 1000;   -- 30 minutes

   -- ── LLM background state ──────────────────────────────────────────────
   LLM_Pending_Provider : LLM_Bridge.Provider := LLM_Bridge.Claude;
   LLM_Waiting_Key      : Boolean := False;

   -- ── Auto-hide bar ─────────────────────────────────────────────────────
   Bar_Until   : unsigned := 0;
   Bar_Visible : Boolean  := True;
   Hide_MS     : constant unsigned := 3_000;

   -- ── Accessors ────────────────────────────────────────────────────────
   function Current_State  return Player_State is (State);
   function Current_Volume return Integer      is (Volume);
   function Is_Fullscreen  return Boolean      is (Fullscreen);

   -- ── C process helpers (from csrc/process_helpers.c) ──────────────────
   function BV_Create_Process_With_Pipe
     (Cmd       : Interfaces.C.Strings.chars_ptr;
      H_Process : System.Address;
      H_Pipe    : System.Address) return int
   with Import, Convention => C,
        External_Name => "bv_create_process_with_pipe";

   function BV_Process_Running (H : System.Address) return int
   with Import, Convention => C,
        External_Name => "bv_process_still_running";

   function BV_Peek_Output
     (H       : System.Address;
      Buf     : System.Address;
      Buf_Len : int) return int
   with Import, Convention => C,
        External_Name => "bv_peek_process_output";

   procedure BV_Close_Handle (H : System.Address)
   with Import, Convention => C, External_Name => "bv_close_handle";

   -- SDL drag-and-drop
   SDL_DROPFILE  : constant unsigned := 16#1000#;
   -- SDL text input
   SDL_TEXTINPUT : constant unsigned := 16#303#;
   -- Ctrl modifier check
   KMOD_CTRL     : constant unsigned := 16#00C0#;

   -- ── Forward declarations ─────────────────────────────────────────────
   procedure Handle_Key (Ev : SDL_Event; Win : SDL.Video.Window_Handle;
                         Quit : out Boolean);
   procedure Handle_Motion (Ev : SDL_Event);
   procedure Handle_Button_Down (Ev   : SDL_Event;
                                 Win  : SDL.Video.Window_Handle;
                                 Tex  : in out SDL.Video.Texture_Handle;
                                 Rend : SDL.Video.Renderer_Handle;
                                 Vid_W : in out Integer;
                                 Vid_H : in out Integer;
                                 FD_MS : in out Integer;
                                 Quit : out Boolean);
   procedure Handle_Button_Up   (Ev : SDL_Event);
   procedure Handle_Wheel       (Ev : SDL_Event);
   procedure Handle_Drop_File   (Ev   : SDL_Event;
                                 Win  : SDL.Video.Window_Handle;
                                 Tex  : in out SDL.Video.Texture_Handle;
                                 Rend : SDL.Video.Renderer_Handle;
                                 Vid_W : in out Integer;
                                 Vid_H : in out Integer;
                                 FD_MS : in out Integer;
                                 Quit : out Boolean);
   procedure Show_Bar;
   procedure Sync_Subs;
   procedure Toggle_Play_Pause;
   procedure Load_Active_SRT;
   procedure Check_Whisper_Done;
   procedure Check_Whisper_Progress;
   procedure Launch_Whisper (Video_Path : String; Translate : Boolean);
   procedure Handle_LLM_Request (P : LLM_Bridge.Provider);
   procedure Check_Update_Result;

   -- ── Show_Bar ──────────────────────────────────────────────────────────
   procedure Show_Bar is
   begin
      Bar_Until   := SDL.Get_Ticks + Hide_MS;
      Bar_Visible := True;
   end Show_Bar;

   -- ── Sync_Subs ─────────────────────────────────────────────────────────
   procedure Sync_Subs is
   begin
      UI_Overlay.Set_Subtitles
        (To_String (Sub_Path (0)),
         To_String (Sub_Path (1)),
         To_String (Sub_Path (2)),
         Sub_Count, Sub_Active);
   end Sync_Subs;

   -- ── Load_Active_SRT ───────────────────────────────────────────────────
   procedure Load_Active_SRT is
   begin
      Active_SRT_OK := False;
      Active_SRT    := (Cues => (others => (others => <>)), Count => 0);
      UI_Overlay.Set_Sub_Text ("");
      if Sub_Active < 0 or else Sub_Active >= Sub_Count then return; end if;
      declare
         Path : constant String := To_String (Sub_Path (Sub_Active));
      begin
         if Path'Length > 0 then
            SRT_Parser.Load (Path, Active_SRT, Active_SRT_OK);
            if not Active_SRT_OK then
               Put_Line ("[Player] WARNING: could not parse SRT: " & Path);
            end if;
         end if;
      end;
   end Load_Active_SRT;

   -- ── Check_Whisper_Progress ────────────────────────────────────────────
   --  Reads available stderr bytes from the pipe, scans for
   --  "progress = NN%" and updates the status banner.
   procedure Check_Whisper_Progress is
      Buf  : String (1 .. 512) := (others => ASCII.NUL);
      Len  : int;
      Key  : constant String := "progress = ";
   begin
      if Whisper_H_Pipe = System.Null_Address then return; end if;
      Len := BV_Peek_Output (Whisper_H_Pipe, Buf (1)'Address, Buf'Length - 1);
      if Len <= 0 then return; end if;

      declare
         S : constant String := Buf (1 .. Integer (Len));
      begin
         for I in S'First .. S'Last - Key'Length loop
            if S (I .. I + Key'Length - 1) = Key then
               declare
                  Start : constant Natural := I + Key'Length;
                  Stop  : Natural := Start;
               begin
                  while Stop <= S'Last and then S (Stop) in '0' .. '9' loop
                     Stop := Stop + 1;
                  end loop;
                  if Stop > Start then
                     declare
                        Pct : constant String := S (Start .. Stop - 1);
                     begin
                        UI_Overlay.Set_Whisper_Status
                          ("Whisper: generating...  " & Pct & "%");
                     end;
                  end if;
               end;
               return;
            end if;
         end loop;
      end;
   end Check_Whisper_Progress;

   -- ── Check_Whisper_Done ────────────────────────────────────────────────
   --  Polls the SRT file + process exit code.  No .done sentinel needed.
   procedure Check_Whisper_Done is
      SRT_Path : constant String := To_String (Whisper_Out_SRT);

      procedure Finish_OK is
      begin
         Whisper_Running := False;
         UI_Overlay.Set_Whisper_Status ("");
         if Whisper_H_Pipe /= System.Null_Address then
            BV_Close_Handle (Whisper_H_Pipe);
            Whisper_H_Pipe := System.Null_Address;
         end if;
         if Whisper_H_Process /= System.Null_Address then
            BV_Close_Handle (Whisper_H_Process);
            Whisper_H_Process := System.Null_Address;
         end if;
         Sub_Path  (0) := To_Unbounded_String (SRT_Path);
         Sub_Count     := Integer'Max (Sub_Count, 1);
         Sub_Active    := 0;
         Sync_Subs;
         Load_Active_SRT;
         Put_Line ("[Whisper] Auto-loaded: " & SRT_Path);
      end Finish_OK;

      procedure Finish_Fail (Reason : String) is
      begin
         Whisper_Running := False;
         UI_Overlay.Set_Whisper_Status (Reason);
         if Whisper_H_Pipe /= System.Null_Address then
            BV_Close_Handle (Whisper_H_Pipe);
            Whisper_H_Pipe := System.Null_Address;
         end if;
         if Whisper_H_Process /= System.Null_Address then
            BV_Close_Handle (Whisper_H_Process);
            Whisper_H_Process := System.Null_Address;
         end if;
      end Finish_Fail;

   begin
      if not Whisper_Running then return; end if;

      --  Drain progress output
      Check_Whisper_Progress;

      --  Check if process has exited
      if Whisper_H_Process /= System.Null_Address and then
         BV_Process_Running (Whisper_H_Process) = 0
      then
         --  Process finished — check for SRT output
         if Ada.Directories.Exists (SRT_Path) then
            Finish_OK;
         else
            Put_Line ("[Whisper] Process exited but no SRT found: " & SRT_Path);
            Finish_Fail ("Whisper: no output — check console");
         end if;
         return;
      end if;

      --  Timeout: 30 minutes
      declare
         Elapsed_S : constant unsigned :=
           (SDL.Get_Ticks - Whisper_Start_Tick) / 1000;
         SRT_Path  : constant String := To_String (Whisper_Out_SRT);
         Tmp_WAV   : constant String :=
           To_String (Whisper_Video_Path) & ".tmp.wav";
      begin
         --  Show elapsed time every ~10s so user knows it's still running
         if Elapsed_S > 0 and then (Elapsed_S mod 10) = 0 then
            UI_Overlay.Set_Whisper_Status
              ("Whisper: running... " & unsigned'Image (Elapsed_S) & "s");
         end if;
         if SDL.Get_Ticks - Whisper_Start_Tick > WHISPER_TIMEOUT_MS then
            --  Diagnose: did ffmpeg at least create the WAV?
            if Ada.Directories.Exists (Tmp_WAV) then
               Put_Line ("[Whisper] TIMEOUT — ffmpeg OK but whisper-cli stalled.");
               Put_Line ("[Whisper] Defender may be blocking whisper-cli.exe.");
               Put_Line ("[Whisper] Add build\ to Defender exclusions and retry.");
               Finish_Fail ("Whisper timeout — add build\ to Defender exclusions");
            elsif Ada.Directories.Exists (SRT_Path) then
               --  SRT appeared but process hasn't exited — load it anyway
               Put_Line ("[Whisper] SRT found at timeout — loading.");
               Finish_OK;
            else
               Put_Line ("[Whisper] TIMEOUT — ffmpeg may have been blocked.");
               Put_Line ("[Whisper] Check: is ffmpeg.exe in build\ or on PATH?");
               Put_Line ("[Whisper] Check: add build\ to Defender exclusions.");
               Finish_Fail ("Whisper timeout — check ffmpeg + Defender exclusions");
            end if;
            return;
         end if;
      end;

      --  Still running — also check if SRT appeared (whisper can write it
      --  before the process fully exits)
      if Ada.Directories.Exists (SRT_Path) then
         --  Give it 2 more seconds to write cleanly, then load
         if SDL.Get_Ticks - Whisper_Start_Tick > 2000 then
            Finish_OK;
         end if;
      end if;

   exception
      when others => null;
   end Check_Whisper_Done;

   -- ── Launch_Whisper ────────────────────────────────────────────────────
   --  Always writes a .bat file to avoid cmd.exe length limit and Defender
   --  issues with inline compound commands.  Polls the .srt file directly
   --  instead of a sentinel .done file so we never miss a successful run.
   procedure Launch_Whisper (Video_Path : String; Translate : Boolean) is
      Whisper_Exe : constant String := Whisper_Bridge.Find_Whisper;
      Model_Path  : constant String := Whisper_Bridge.Find_Model;
   begin
      if Whisper_Exe = "" then
         Put_Line ("[Whisper] ERROR: whisper-cli.exe not found.");
         UI_Overlay.Set_Whisper_Status ("Whisper not found — see console");
         return;
      end if;
      if Model_Path = "" then
         Put_Line ("[Whisper] ERROR: model not found.");
         UI_Overlay.Set_Whisper_Status ("Model not found — see console");
         return;
      end if;
      if Whisper_Running then
         Put_Line ("[Whisper] Already running, please wait.");
         return;
      end if;

      declare
         use Ada.Text_IO;
         Out_SRT    : constant String := Video_Path & ".srt";
         Tmp_WAV    : constant String := Video_Path & ".tmp.wav";
         Trans_Flag : constant String :=
           (if Translate then " --translate" else "");

         --  Determine exe dir for ffmpeg / bat placement
         Exe : constant String := Whisper_Bridge.Exe_Dir;
         Ffmpeg : constant String :=
           (if Ada.Directories.Exists (Exe & "ffmpeg.exe")
            then Exe & "ffmpeg.exe" else "ffmpeg");

         --  Write bat to exe dir
         Bat_Path : constant String := Exe & "bv_whisper_job.bat";
         Bat_File : File_Type;

         H_Proc : aliased System.Address := System.Null_Address;
         H_Pipe : aliased System.Address := System.Null_Address;
         CP     : Interfaces.C.Strings.chars_ptr;
         Ret    : int;
      begin
         --  Write bat — each line is a separate Put_Line, no quoting hell
         begin
            Create (Bat_File, Out_File, Bat_Path);
            Put_Line (Bat_File, "@echo off");
            Put_Line (Bat_File, ":: BlackVideo Whisper job — if this hangs,");
            Put_Line (Bat_File, ":: add the build\ folder to Windows Defender exclusions.");
            --  Step 1: extract audio
            Put_Line (Bat_File,
              """" & Ffmpeg & """ -y -i """ & Video_Path & """ " &
              "-ar 16000 -ac 1 -f wav """ & Tmp_WAV & """ >nul 2>&1");
            Put_Line (Bat_File, "if errorlevel 1 (");
            Put_Line (Bat_File, "  echo [Whisper] ERROR: ffmpeg audio extraction failed >&2");
            Put_Line (Bat_File, "  exit /b 1");
            Put_Line (Bat_File, ")");
            --  Step 2: run whisper
            Put_Line (Bat_File,
              """" & Whisper_Exe & """ " &
              "-m """ & Model_Path & """ " &
              "-f """ & Tmp_WAV & """ " &
              "--output-srt" & Trans_Flag & " " &
              "-of """ & Video_Path & """ 2>&1");
            Put_Line (Bat_File, "if errorlevel 1 (");
            Put_Line (Bat_File, "  echo [Whisper] ERROR: whisper-cli failed >&2");
            Put_Line (Bat_File, "  exit /b 1");
            Put_Line (Bat_File, ")");
            Put_Line (Bat_File,
              "del /q """ & Tmp_WAV & """ 2>nul");
            Close (Bat_File);
         exception
            when others =>
               Put_Line ("[Whisper] ERROR: could not write bat: " & Bat_Path);
               UI_Overlay.Set_Whisper_Status ("Whisper bat write failed");
               return;
         end;

         CP  := Interfaces.C.Strings.New_String
                  ("cmd.exe /c """ & Bat_Path & """");
         Ret := BV_Create_Process_With_Pipe
           (CP, H_Proc'Address, H_Pipe'Address);
         Interfaces.C.Strings.Free (CP);

         if Ret = 0 then
            Put_Line ("[Whisper] ERROR: CreateProcess failed.");
            UI_Overlay.Set_Whisper_Status ("Whisper launch failed");
            return;
         end if;

         Whisper_H_Process  := H_Proc;
         Whisper_H_Pipe     := H_Pipe;
         Whisper_Running    := True;
         Whisper_Out_SRT    := To_Unbounded_String (Out_SRT);
         Whisper_Video_Path := To_Unbounded_String (Video_Path);
         Whisper_Start_Tick := SDL.Get_Ticks;

         if Translate then
            UI_Overlay.Set_Whisper_Status ("Whisper: translating...  0%");
            Put_Line ("[Whisper] Launched translation job.");
         else
            UI_Overlay.Set_Whisper_Status ("Whisper: generating...  0%");
            Put_Line ("[Whisper] Launched transcription job.");
         end if;
         Put_Line ("[Whisper] Output: " & Out_SRT);
      end;
   end Launch_Whisper;

   -- ── Handle_LLM_Request ────────────────────────────────────────────────
   procedure Handle_LLM_Request (P : LLM_Bridge.Provider) is
      Key : constant String := LLM_Bridge.Get_API_Key (P);
   begin
      if Key = "" then
         --  Ask user for API key via SDL2 text input overlay
         LLM_Pending_Provider := P;
         LLM_Waiting_Key := True;
         UI_Overlay.Show_Key_Input (LLM_Bridge.Provider_Name (P));
         declare
            procedure SDL_StartTextInput
            with Import, Convention => C,
                 External_Name => "SDL_StartTextInput";
         begin SDL_StartTextInput; end;
         return;
      end if;

      --  Build transcript from existing Whisper .srt if available,
      --  otherwise use video filename as minimal context.
      UI_Overlay.Set_Whisper_Status
        (LLM_Bridge.Provider_Name (P) & ": preparing...");

      declare
         Video_Path_S : constant String := To_String (Whisper_Video_Path);
         Out_SRT      : constant String := Video_Path_S & ".llm.srt";
         Whisper_SRT  : constant String := Video_Path_S & ".srt";
         Duration     : constant Float  := Video_Decoder.Get_Duration;
         Success      : Boolean;

         --  Extract plain text from an SRT file (strip timestamps + indices)
         function SRT_To_Transcript (SRT_Path : String) return String is
            use Ada.Text_IO;
            use Ada.Strings.Unbounded;
            F      : File_Type;
            Result : Unbounded_String;
            Buf    : String (1 .. 2048);
            Last   : Natural;
         begin
            if not Ada.Directories.Exists (SRT_Path) then
               return "";
            end if;
            Open (F, In_File, SRT_Path);
            while not End_Of_File (F) loop
               Get_Line (F, Buf, Last);
               declare
                  Line : constant String := Buf (1 .. Last);
                  Is_Index : Boolean := True;
                  Is_Time  : Boolean := Last > 8 and then
                    Line (Line'First + 2) = ':';
               begin
                  --  Skip pure-number lines (subtitle index)
                  for C of Line loop
                     if C not in '0' .. '9' then
                        Is_Index := False; exit;
                     end if;
                  end loop;
                  if not Is_Index and then not Is_Time
                     and then Last > 0
                  then
                     if Length (Result) > 0 then
                        Append (Result, " ");
                     end if;
                     Append (Result, Line);
                  end if;
               end;
            end loop;
            Close (F);
            return To_String (Result);
         exception
            when others => return "";
         end SRT_To_Transcript;

         Transcript : constant String :=
           (if Ada.Directories.Exists (Whisper_SRT)
            then SRT_To_Transcript (Whisper_SRT)
            else "(No transcript available. Generate captions with " &
                 "Whisper first for accurate results. " &
                 "Video: " & Utils.Base_Name (Video_Path_S) & ")");
      begin
         Put_Line ("[LLM] Transcript length:" & Integer'Image (Transcript'Length));
         Success := LLM_Bridge.Generate
           (P            => P,
            Video_Path   => Video_Path_S,
            Transcript   => Transcript,
            Duration_Sec => Duration,
            Out_SRT      => Out_SRT,
            Cb           => null);

         if Success then
            --  Load into Track 2
            Sub_Path  (1) := To_Unbounded_String (Out_SRT);
            Sub_Count     := Integer'Max (Sub_Count, 2);
            Sub_Active    := 1;
            Sync_Subs;
            Load_Active_SRT;
            UI_Overlay.Set_Whisper_Status ("");
            Put_Line ("[LLM] Loaded: " & Out_SRT);
         else
            UI_Overlay.Set_Whisper_Status
              (LLM_Bridge.Provider_Name (P) & ": failed — see console");
         end if;
      end;
   end Handle_LLM_Request;

   -- ── Check_Update_Result ───────────────────────────────────────────────
   procedure Check_Update_Result is
   begin
      if Updater.Check_Done and then Updater.Update_Available then
         UI_Overlay.Set_Update_Available (True, Updater.Remote_Version);
      end if;
   end Check_Update_Result;

   -- ── Toggle_Play_Pause ────────────────────────────────────────────────
   procedure Toggle_Play_Pause is
   begin
      if State = No_Video then return; end if;
      if State = Playing then
         State := Paused;
         Video_Decoder.Pause; Audio.Pause;
         Put_Line ("[Player] Paused");
      else
         State := Playing;
         Video_Decoder.Resume; Audio.Resume;
         Put_Line ("[Player] Resumed");
      end if;
   end Toggle_Play_Pause;

   -- ── Open file dialog (Windows — PowerShell) ───────────────────────────
   function Open_Dialog (Title : String; Filter_Ext : String) return String is
      pragma Unreferenced (Title);
      Cmd : constant String :=
        "powershell -NoProfile -WindowStyle Hidden -Command " &
        """" &
        "[void][System.Reflection.Assembly]::LoadWithPartialName(" &
        "'System.Windows.Forms');" &
        "$f=New-Object System.Windows.Forms.OpenFileDialog;" &
        "$f.Filter='" & Filter_Ext & "';" &
        "if($f.ShowDialog()-eq'OK'){Write-Host $f.FileName}" &
        """";

      function popen  (C, M : Interfaces.C.Strings.chars_ptr)
        return System.Address
      with Import, Convention => C, External_Name => "popen";
      function pclose (F : System.Address) return int
      with Import, Convention => C, External_Name => "pclose";
      function fgets  (B : System.Address; N : int; F : System.Address)
        return System.Address
      with Import, Convention => C, External_Name => "fgets";

      use Interfaces.C.Strings;
      Cc : chars_ptr     := New_String (Cmd);
      Cm : chars_ptr     := New_String ("r");
      Fp : System.Address;
      Buf : String (1 .. 1024) := (others => ' ');
      Dummy_Addr : System.Address;
      Dummy_Int  : int;
   begin
      Fp := popen (Cc, Cm);
      Free (Cc); Free (Cm);
      if Fp = System.Null_Address then return ""; end if;
      Dummy_Addr := fgets (Buf (1)'Address, 1023, Fp);
      Dummy_Int  := pclose (Fp);
      pragma Unreferenced (Dummy_Addr, Dummy_Int);
      for I in Buf'Range loop
         if Buf (I) = ASCII.LF or else Buf (I) = ASCII.CR
            or else Character'Pos (Buf (I)) = 0
         then
            if I = 1 then return ""; end if;
            return Buf (1 .. I - 1);
         end if;
      end loop;
      return Buf;
   end Open_Dialog;

   -- ── Find a system font ─────────────────────────────────────────────────
   function Find_Font return String is
      type String_Ptr is access constant String;
      type Font_List  is array (Positive range <>) of String_Ptr;
      Fonts : constant Font_List :=
        (new String'("C:\Windows\Fonts\segoeui.ttf"),
         new String'("C:\Windows\Fonts\arial.ttf"),
         new String'("C:\Windows\Fonts\tahoma.ttf"),
         new String'("C:\Windows\Fonts\verdana.ttf"),
         new String'("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"),
         new String'("/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"),
         new String'("/usr/share/fonts/opentype/noto/NotoSans-Regular.ttf"));

      function fopen  (P, M : Interfaces.C.Strings.chars_ptr)
        return System.Address
      with Import, Convention => C, External_Name => "fopen";
      function fclose (F : System.Address) return int
      with Import, Convention => C, External_Name => "fclose";

      use Interfaces.C.Strings;
   begin
      for F of Fonts loop
         declare
            Cp : chars_ptr      := New_String (F.all);
            Cm : chars_ptr      := New_String ("r");
            Fh : constant System.Address := fopen (Cp, Cm);
            D  : int;
         begin
            Free (Cp); Free (Cm);
            if Fh /= System.Null_Address then
               D := fclose (Fh);
               pragma Unreferenced (D);
               return F.all;
            end if;
         end;
      end loop;
      return "";
   end Find_Font;

   -- ── Open video (hot-load during running session) ─────────────────────
   --  Used from welcome screen Open button and SDL_DROPFILE.
   procedure Open_Video
     (Video_File : String;
      Win        : SDL.Video.Window_Handle;
      Vid_W      : out Integer;
      Vid_H      : out Integer;
      FD_MS      : out Integer;
      Tex        : in out SDL.Video.Texture_Handle;
      Rend       : SDL.Video.Renderer_Handle)
   is
      procedure SDL_SetWindowSize
        (W : SDL.Video.Window_Handle; W2, H2 : int)
      with Import, Convention => C, External_Name => "SDL_SetWindowSize";
   begin
      Video_Decoder.Close;
      Audio.Stop;                          --  closes old device
      Video_Decoder.Open (Video_File, Vid_W, Vid_H, FD_MS);
      Whisper_Video_Path := To_Unbounded_String (Video_File);
      declare
         procedure SDL_SetWindowTitle
           (W : SDL.Video.Window_Handle; T : Interfaces.C.Strings.chars_ptr)
         with Import, Convention => C,
              External_Name => "SDL_SetWindowTitle";
         CT : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String
             ("BlackVideo  " & Utils.Base_Name (Video_File));
      begin
         SDL_SetWindowTitle (Win, CT);
         Interfaces.C.Strings.Free (CT);
      end;
      --  Resize SDL window to match the new video dimensions
      SDL_SetWindowSize (Win, int (Vid_W), int (Vid_H));
      Renderer.On_Resize (Vid_W, Vid_H);
      Renderer.Init_Texture (Rend, Tex, Vid_W, Vid_H);
      UI_Overlay.Set_Window_Size (Vid_W, Vid_H);
      UI_Overlay.Set_No_Video_Mode (False);
      Thumb_Cache.Generate (Video_File);
      Video_Decoder.Start_Decoding;
      Audio.Init (Volume);                 --  re-open audio device
      Audio.Start;
      Audio.Set_Volume (Volume);
      State := Playing;
      Put_Line ("[Player] Opened: " & Video_File);
   end Open_Video;

   function SDL_GetMouseState (X, Y : System.Address) return unsigned
   with Import, Convention => C, External_Name => "SDL_GetMouseState";

   -- ── SDL_DROPFILE event helpers ────────────────────────────────────────
   --  SDL stores a heap-allocated UTF-8 path in SDL_DropEvent.file.
   --  We read it via the raw event bytes at offset 12 (32-bit pointer on
   --  some builds; 64-bit on others).  For safety we use SDL_GetDroppedFile
   --  which is not in our minimal binding, so we read via the raw bytes.
   --  Safe C helper — reads SDL_DropEvent.drop.file correctly on both
   --  32-bit and 64-bit builds (avoids fragile manual byte-offset arithmetic).
   function BV_SDL_Drop_File (Ev_Ptr : System.Address)
     return Interfaces.C.Strings.chars_ptr
   with Import, Convention => C, External_Name => "bv_sdl_drop_file";

   procedure SDL_Free (Ptr : System.Address)
   with Import, Convention => C, External_Name => "SDL_free";

   -- ══════════════════════════════════════════════════════════════════════
   --  Run
   -- ══════════════════════════════════════════════════════════════════════
   procedure Run (Video_File : String) is
      Win  : SDL.Video.Window_Handle;
      Rend : SDL.Video.Renderer_Handle;
      Tex  : SDL.Video.Texture_Handle;
      Ev   : SDL_Event;
      Quit : Boolean  := False;

      Has_Video      : Boolean := Video_File'Length > 0;
      Vid_W          : Integer := 1280;
      Vid_H          : Integer := 720;
      Frame_Delay_MS : Integer := 33;
      Ret            : int;
      Frame_Start, Elapsed, Sleep_MS : unsigned;
   begin
      Put_Line ("[Player] Init SDL2 ...");
      Ret := SDL.Initialize (SDL.Flags.Enable_Video or SDL.Flags.Enable_Audio);
      if Ret /= 0 then raise Program_Error with "[Player] SDL_Init failed"; end if;

      --  Enable drag-and-drop
      declare
         procedure SDL_EventState (T : unsigned; State : int)
         with Import, Convention => C, External_Name => "SDL_EventState";
      begin
         SDL_EventState (SDL_DROPFILE, 1);  -- SDL_ENABLE = 1
      end;

      if Has_Video then
         Video_Decoder.Open (Video_File, Vid_W, Vid_H, Frame_Delay_MS);
         Whisper_Video_Path := To_Unbounded_String (Video_File);
      end if;

      SDL.Video.Windows.Create
        (Win,
         Title  => (if Has_Video
                    then "BlackVideo  " & Utils.Base_Name (Video_File)
                    else "BlackVideo Mini Player"),
         X      => SDL.Video.Windows.SDL_WINDOWPOS_CENTERED,
         Y      => SDL.Video.Windows.SDL_WINDOWPOS_CENTERED,
         Width  => int (Vid_W),
         Height => int (Vid_H),
         Flags  => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Create
        (Rend, Win,
         Flags => SDL.Video.Renderers.SDL_RENDERER_ACCELERATED
                  or SDL.Video.Renderers.SDL_RENDERER_PRESENTVSYNC);

      if Has_Video then
         Renderer.Init_Texture (Rend, Tex, Vid_W, Vid_H);
      end if;

      Audio.Init (Volume);
      UI_Overlay.Init (Find_Font);
      UI_Overlay.Set_Window_Size (Vid_W, Vid_H);

      if Has_Video then
         Sync_Subs;
         Show_Bar;
         Video_Decoder.Start_Decoding;
         Audio.Start;
         State := Playing;
         Thumb_Cache.Generate (Video_File);
         Put_Line ("[Player] Playing: " & Video_File);
      else
         --  Welcome screen
         State := No_Video;
         UI_Overlay.Set_No_Video_Mode (True);
         Put_Line ("[Player] Welcome screen — open a video or drop a file.");
      end if;

      --  Start background update check
      Updater.Start_Check;

      Put_Line ("[Player]   Keys: SPACE=pause  LEFT/RIGHT=seek  UP/DOWN=vol");
      Put_Line ("[Player]         M=mute  L=loop  F=fullscreen  Q/ESC=quit");
      Put_Line ("[Player]   Mouse: click=pause/play  drag-bar=seek  right-click=menu");

      -- ── Main loop ──────────────────────────────────────────────────────
      while not Quit loop

         Frame_Start := SDL.Get_Ticks;

         if SDL.Get_Ticks > Bar_Until and then not UI_Overlay.Is_Seeking then
            Bar_Visible := False;
         end if;

         --  Background job polling
         Check_Whisper_Done;
         Check_Update_Result;

         --  LLM key input overlay: submitted or cancelled
         if LLM_Waiting_Key then
            if UI_Overlay.Key_Input_Submitted then
               declare
                  procedure SDL_StopTextInput
                  with Import, Convention => C,
                       External_Name => "SDL_StopTextInput";
                  K : constant String := UI_Overlay.Key_Input_Value;
               begin
                  LLM_Bridge.Set_API_Key (LLM_Pending_Provider, K);
                  UI_Overlay.Hide_Key_Input;
                  LLM_Waiting_Key := False;
                  SDL_StopTextInput;
                  Handle_LLM_Request (LLM_Pending_Provider);
               end;
            elsif UI_Overlay.Key_Input_Cancelled then
               declare
                  procedure SDL_StopTextInput
                  with Import, Convention => C,
                       External_Name => "SDL_StopTextInput";
               begin
                  LLM_Waiting_Key := False;
                  SDL_StopTextInput;
               end;
            end if;
         end if;

         --  Update overlay button handling
         if UI_Overlay.Update_Overlay_Download_Clicked then
            Updater.Open_Browser (Updater.Download_URL);
            UI_Overlay.Hide_Update_Overlay;
         elsif UI_Overlay.Update_Overlay_Later_Clicked then
            UI_Overlay.Hide_Update_Overlay;
         end if;

         -- ── Event pump ────────────────────────────────────────────────
         while SDL.Events.Poll (Ev) loop
            declare
               T : constant unsigned := SDL.Events.Event_Type (Ev);
            begin
               if    T = SDL_QUIT            then Quit := True;
               elsif T = SDL_KEYDOWN         then Handle_Key         (Ev, Win, Quit);
               elsif T = SDL_MOUSEMOTION     then Handle_Motion       (Ev);
               elsif T = SDL_MOUSEBUTTONDOWN then Handle_Button_Down  (Ev, Win, Tex, Rend, Vid_W, Vid_H, Frame_Delay_MS, Quit);
               elsif T = SDL_MOUSEBUTTONUP   then Handle_Button_Up    (Ev);
               elsif T = SDL_MOUSEWHEEL      then Handle_Wheel        (Ev);
               elsif T = SDL_TEXTINPUT       then
                  if UI_Overlay.Key_Input_Visible then
                     UI_Overlay.Handle_Text_Input
                       (SDL.Events.Text_Chars (Ev));
                  end if;
               elsif T = SDL_DROPFILE        then
                  declare
                     Drop_Quit : Boolean := False;
                  begin
                     Handle_Drop_File (Ev, Win, Tex, Rend, Vid_W, Vid_H, Frame_Delay_MS, Drop_Quit);
                     Has_Video := True;
                     if Drop_Quit then Quit := True; end if;
                  end;
               elsif T = SDL_WINDOWEVENT then
                  if SDL.Events.Window_Sub_Event (Ev) = SDL_WINDOWEVENT_SIZE_CHANGED
                  then
                     declare
                        NW : constant Integer := Integer (SDL.Events.Window_Data1 (Ev));
                        NH : constant Integer := Integer (SDL.Events.Window_Data2 (Ev));
                     begin
                        Renderer.On_Resize (NW, NH);
                        UI_Overlay.Set_Window_Size (NW, NH);
                     end;
                  end if;
               end if;
            end;
         end loop;

         --  Scrubber drag
         if UI_Overlay.Is_Seeking and then State /= No_Video then
            declare
               Mx, My : aliased int := 0;
               Dummy  : unsigned;
               Dur    : constant Float := Video_Decoder.Get_Duration;
            begin
               Dummy := SDL_GetMouseState (Mx'Address, My'Address);
               pragma Unreferenced (Dummy);
               Video_Decoder.Seek_To (UI_Overlay.Seek_Frac (Integer (Mx)) * Dur);
               Audio.Flush;
            end;
         end if;

         --  Thumbnail hover preview
         if State /= No_Video and then Bar_Visible then
            declare
               Mx, My : aliased int := 0;
               Dummy  : unsigned;
            begin
               Dummy := SDL_GetMouseState (Mx'Address, My'Address);
               pragma Unreferenced (Dummy);
               if UI_Overlay.Hit_Seek (Integer (Mx), Integer (My)) then
                  declare
                     Frac  : constant Float := UI_Overlay.Seek_Frac (Integer (Mx));
                     Pos   : constant Float := Frac * Video_Decoder.Get_Duration;
                     TPath : constant String :=
                       Thumb_Cache.Frame_Path
                         (To_String (Whisper_Video_Path), Pos);
                  begin
                     UI_Overlay.Set_Thumb_Preview (TPath, Pos);
                  end;
               else
                  UI_Overlay.Set_Thumb_Preview ("", 0.0);
               end if;
            end;
         end if;

         -- ── Decode + display ──────────────────────────────────────────
         if State = Playing then
            declare
               Frame : Video_Decoder.RGB_Frame;
               Got   : Boolean;
            begin
               Video_Decoder.Next_Frame (Frame, Got);
               if Got then
                  Renderer.Upload_Frame (Tex, Frame, Vid_W, Vid_H);
               elsif Video_Decoder.Is_EOF then
                  if Looping then
                     Video_Decoder.Seek_To (0.0);
                     Audio.Flush;
                     Put_Line ("[Player] Loop.");
                  else
                     Put_Line ("[Player] End of file.");
                     Quit := True;
                  end if;
               end if;
            end;
         end if;

         --  Subtitle cue
         if Active_SRT_OK and then Sub_Active >= 0 and then
            State /= No_Video
         then
            declare
               Pos : constant Float := Video_Decoder.Get_Position;
               Cue : constant String :=
                 SRT_Parser.Current_Text (Active_SRT, Pos);
            begin
               UI_Overlay.Set_Sub_Text (Cue);
            end;
         end if;

         --  Render
         if State /= No_Video then
            Renderer.Draw (Rend, Tex, Vid_W, Vid_H);
         end if;

         UI_Overlay.Draw
           (Renderer   => Renderer.To_Address (Rend),
            Position   => (if State /= No_Video
                           then Video_Decoder.Get_Position else 0.0),
            Duration   => (if State /= No_Video
                           then Video_Decoder.Get_Duration else 0.0),
            Playing    => (State = Playing),
            Looping    => Looping,
            Muted      => Muted,
            Volume     => Volume,
            Fullscreen => Fullscreen,
            Speed_Idx  => Speed_Idx,
            Visible    => Bar_Visible);

         Renderer.Present (Rend);

         Elapsed := SDL.Get_Ticks - Frame_Start;
         if Elapsed < unsigned (Frame_Delay_MS) then
            Sleep_MS := unsigned (Frame_Delay_MS) - Elapsed;
            SDL.Delay_MS (Sleep_MS);
         end if;

      end loop;

      Put_Line ("[Player] Shutting down ...");
      UI_Overlay.Quit;
      Audio.Stop;
      if State /= No_Video then
         Video_Decoder.Close;
         Renderer.Destroy_Texture (Tex);
      end if;
      SDL.Video.Renderers.Destroy (Rend);
      SDL.Video.Windows.Destroy (Win);
      SDL.Quit;
      State := Stopped;

   exception
      when E : others =>
         Put_Line ("[Player] FATAL: " & Ada.Exceptions.Exception_Message (E));
         SDL.Quit;
         raise;
   end Run;

   -- ══════════════════════════════════════════════════════════════════════
   --  Event handlers
   -- ══════════════════════════════════════════════════════════════════════

   procedure Handle_Motion (Ev : SDL_Event) is
      X : constant Integer := Integer (SDL.Events.Mouse_X (Ev));
      Y : constant Integer := Integer (SDL.Events.Mouse_Y (Ev));
   begin
      Show_Bar;
      UI_Overlay.Set_Hover_Btn (UI_Overlay.Hit_Button (X, Y));
      if UI_Overlay.Ctx_Open then
         declare
            Ign : constant int := UI_Overlay.Hit_Ctx (X, Y);
         begin pragma Unreferenced (Ign); end;
      end if;
   end Handle_Motion;

   procedure Handle_Button_Up (Ev : SDL_Event) is
      pragma Unreferenced (Ev);
   begin
      UI_Overlay.Set_Seeking (False);
   end Handle_Button_Up;

   procedure Handle_Wheel (Ev : SDL_Event) is
      Wy : constant int := SDL.Events.Mouse_Wheel_Y (Ev);
   begin
      Show_Bar;
      Volume := Integer'Min (128, Integer'Max (0, Volume + Integer (Wy) * 5));
      Audio.Set_Volume (Volume);
      Put_Line ("[Player] Volume =" & Integer'Image (Volume));
   end Handle_Wheel;

   -- ── Handle_Drop_File ─────────────────────────────────────────────────
   procedure Handle_Drop_File
     (Ev    : SDL_Event;
      Win   : SDL.Video.Window_Handle;
      Tex   : in out SDL.Video.Texture_Handle;
      Rend  : SDL.Video.Renderer_Handle;
      Vid_W : in out Integer;
      Vid_H : in out Integer;
      FD_MS : in out Integer;
      Quit  : out Boolean)
   is
      use Interfaces.C.Strings;
      --  Use C helper to safely read the drop path on 32/64-bit alike.
      CP   : constant chars_ptr :=
               BV_SDL_Drop_File (Ev'Address);
      Path : constant String :=
               (if CP /= Null_Ptr then Value (CP) else "");
   begin
      Quit := False;
      --  SDL_free is called by SDL internally for drop events; do NOT
      --  call SDL_Free here — doing so causes a double-free crash.
      if Path'Length > 0 then
         Put_Line ("[Player] Drop: " & Path);
         Open_Video (Path, Win, Vid_W, Vid_H, FD_MS, Tex, Rend);
         Sync_Subs;
         Show_Bar;
      end if;
   end Handle_Drop_File;

   -- ── Handle_Button_Down ───────────────────────────────────────────────
   procedure Handle_Button_Down
     (Ev    : SDL_Event;
      Win   : SDL.Video.Window_Handle;
      Tex   : in out SDL.Video.Texture_Handle;
      Rend  : SDL.Video.Renderer_Handle;
      Vid_W : in out Integer;
      Vid_H : in out Integer;
      FD_MS : in out Integer;
      Quit  : out Boolean)
   is
      X   : constant Integer       := Integer (SDL.Events.Mouse_Btn_X (Ev));
      Y   : constant Integer       := Integer (SDL.Events.Mouse_Btn_Y (Ev));
      Btn : constant unsigned_char := SDL.Events.Mouse_Button (Ev);
   begin
      Quit := False;
      Show_Bar;

      --  Forward every left-click to the overlay hit-tester first.
      --  This handles the API-key OK button and update-overlay buttons
      --  which are drawn on top of everything else.
      if Btn = SDL_BUTTON_LEFT then
         UI_Overlay.Handle_Click (X, Y);
         --  If key-input overlay is showing, swallow all other click logic
         if UI_Overlay.Key_Input_Visible then return; end if;
      end if;

      if Btn = SDL_BUTTON_RIGHT then
         if UI_Overlay.Ctx_Open then
            UI_Overlay.Close_Ctx;
            Ctx_Close_Tick := SDL.Get_Ticks;
         else
            UI_Overlay.Open_Ctx (X, Y);
         end if;
         return;
      end if;

      if Btn /= SDL_BUTTON_LEFT then return; end if;

      if SDL.Get_Ticks - Ctx_Close_Tick < CTX_GUARD_MS then
         return;
      end if;

      if UI_Overlay.Ctx_Open then
         declare
            Item : constant int := UI_Overlay.Hit_Ctx (X, Y);
         begin
            UI_Overlay.Close_Ctx;
            Ctx_Close_Tick := SDL.Get_Ticks;

            -- ── Context menu dispatch ───────────────────────────────────
            if Item = UI_Overlay.CTX_OPEN_FILE then
               declare
                  Path : constant String :=
                    Open_Dialog ("Open Video",
                      "Video files|*.mp4;*.mkv;*.avi;*.mov;*.wmv;*.flv;*.webm|All files|*.*");
               begin
                  if Path'Length > 0 then
                     Put_Line ("[Player] Opening: " & Path);
                     Open_Video (Path, Win, Vid_W, Vid_H, FD_MS, Tex, Rend);
                     Sync_Subs;
                     Show_Bar;
                  end if;
               end;
               Ctx_Close_Tick := SDL.Get_Ticks;

            elsif Item = UI_Overlay.CTX_SUB_NONE then
               Sub_Active    := -1;
               Active_SRT_OK := False;
               UI_Overlay.Set_Sub_Text ("");
               Sync_Subs;

            elsif Item >= UI_Overlay.CTX_SUB_1
              and then Item <= UI_Overlay.CTX_SUB_3
            then
               declare
                  Idx : constant Integer :=
                    Integer (Item) - Integer (UI_Overlay.CTX_SUB_1);
               begin
                  if Idx < Sub_Count then
                     Sub_Active := Idx;
                     Sync_Subs;
                     Load_Active_SRT;
                  else
                     declare
                        Path : constant String :=
                          Open_Dialog ("Load Subtitle",
                            "Subtitle files|*.srt;*.ass;*.ssa;*.vtt|All files|*.*");
                     begin
                        if Path'Length > 0 then
                           Sub_Path (Idx) := To_Unbounded_String (Path);
                           if Idx >= Sub_Count then Sub_Count := Idx + 1; end if;
                           Sub_Active := Idx;
                           Sync_Subs;
                           Load_Active_SRT;
                        end if;
                     end;
                     Ctx_Close_Tick := SDL.Get_Ticks;
                  end if;
               end;

            elsif Item = UI_Overlay.CTX_WHISPER_GEN then
               Launch_Whisper (To_String (Whisper_Video_Path), False);

            elsif Item = UI_Overlay.CTX_WHISPER_TRANS then
               Launch_Whisper (To_String (Whisper_Video_Path), True);

            elsif Item = UI_Overlay.CTX_LLM_CLAUDE  then
               Handle_LLM_Request (LLM_Bridge.Claude);
            elsif Item = UI_Overlay.CTX_LLM_OPENAI  then
               Handle_LLM_Request (LLM_Bridge.OpenAI);
            elsif Item = UI_Overlay.CTX_LLM_GEMINI  then
               Handle_LLM_Request (LLM_Bridge.Gemini);
            elsif Item = UI_Overlay.CTX_LLM_DEEPSEEK then
               Handle_LLM_Request (LLM_Bridge.DeepSeek);
            elsif Item = UI_Overlay.CTX_LLM_GROK    then
               Handle_LLM_Request (LLM_Bridge.Grok);

            elsif Item = UI_Overlay.CTX_CLEAR_THUMB_CACHE then
               Thumb_Cache.Clear (To_String (Whisper_Video_Path));

            elsif Item = UI_Overlay.CTX_CHECK_UPDATES then
               if Updater.Check_Done then
                  if Updater.Update_Available then
                     UI_Overlay.Show_Update_Overlay
                       (Updater.Remote_Version,
                        Updater.Release_Notes,
                        Updater.Download_URL);
                  else
                     UI_Overlay.Set_Whisper_Status
                       ("Up to date  (v" & Updater.Current_Version & ")");
                  end if;
               else
                  UI_Overlay.Set_Whisper_Status ("Checking for updates...");
               end if;

            end if;
         end;
         return;
      end if;

      if State = No_Video then
         --  Welcome screen: clicking opens a file dialog
         declare
            Path : constant String :=
              Open_Dialog ("Open Video",
                "Video files|*.mp4;*.mkv;*.avi;*.mov;*.wmv;*.flv;*.webm|All files|*.*");
         begin
            if Path'Length > 0 then
               Put_Line ("[Player] Opening: " & Path);
               Open_Video (Path, Win, Vid_W, Vid_H, FD_MS, Tex, Rend);
               Sync_Subs;
               Show_Bar;
            end if;
         end;
         Ctx_Close_Tick := SDL.Get_Ticks;
         return;
      end if;

      if UI_Overlay.In_Bar (X, Y) and then UI_Overlay.Hit_Seek (X, Y) then
         UI_Overlay.Set_Seeking (True);
         Video_Decoder.Seek_To
           (UI_Overlay.Seek_Frac (X) * Video_Decoder.Get_Duration);
         Audio.Flush;
         return;
      end if;

      if UI_Overlay.In_Bar (X, Y) then
         declare
            B : constant int := UI_Overlay.Hit_Button (X, Y);
         begin
            if B = UI_Overlay.BTN_PLAY_PAUSE then
               Toggle_Play_Pause;
            elsif B = UI_Overlay.BTN_PREV then
               Video_Decoder.Seek_To (0.0); Audio.Flush;
            elsif B = UI_Overlay.BTN_NEXT then
               Video_Decoder.Seek_To
                 (Float'Max (0.0, Video_Decoder.Get_Duration - 3.0));
               Audio.Flush;
            elsif B = UI_Overlay.BTN_LOOP then
               Looping := not Looping;
            elsif B = UI_Overlay.BTN_VOLUME then
               Muted := not Muted; Audio.Toggle_Mute;
            elsif B = UI_Overlay.BTN_SPEED then
               Speed_Idx := UI_Overlay.Speed_Index
                              ((int (Speed_Idx) + 1) mod 4);
            elsif B = UI_Overlay.BTN_FULLSCREEN then
               Fullscreen := not Fullscreen;
               SDL.Video.Windows.Set_Fullscreen (Win, Fullscreen);
            elsif B = UI_Overlay.BTN_MENU then
               UI_Overlay.Open_Ctx (X, Y - 220);
            end if;
         end;
         return;
      end if;

      Toggle_Play_Pause;
   end Handle_Button_Down;

   -- ── Handle_Key ───────────────────────────────────────────────────────
   procedure Handle_Key
     (Ev   : SDL_Event;
      Win  : SDL.Video.Window_Handle;
      Quit : out Boolean)
   is
      K : constant int := SDL.Events.Key_Sym (Ev);
   begin
      Quit := False;
      Show_Bar;
      --  If key-input overlay is open, route ALL keys there
      if UI_Overlay.Key_Input_Visible then
         declare
            Ctrl_Held : constant Boolean :=
              (SDL.Events.Key_Mod (Ev) and KMOD_CTRL) /= 0;
         begin
            if Ctrl_Held and then K = SDLK_v then
               --  Ctrl+V: paste entire clipboard (replaces current value)
               UI_Overlay.Paste_Clipboard;
            elsif Ctrl_Held and then K = SDLK_a then
               --  Ctrl+A: clear field
               UI_Overlay.Clear_Key_Input;
            elsif K = SDLK_BACKSPACE then
               UI_Overlay.Handle_Key_Backspace;
            elsif K = SDLK_RETURN then
               --  Enter submits
               if UI_Overlay.Key_Input_Value'Length > 0 then
                  declare
                     procedure SDL_StopTextInput
                     with Import, Convention => C,
                          External_Name => "SDL_StopTextInput";
                     Key_Val : constant String :=
                       UI_Overlay.Key_Input_Value;
                  begin
                     LLM_Bridge.Set_API_Key (LLM_Pending_Provider, Key_Val);
                     UI_Overlay.Hide_Key_Input;
                     LLM_Waiting_Key := False;
                     SDL_StopTextInput;
                     Handle_LLM_Request (LLM_Pending_Provider);
                  end;
               end if;
            elsif K = SDLK_ESCAPE then
               declare
                  procedure SDL_StopTextInput
                  with Import, Convention => C,
                       External_Name => "SDL_StopTextInput";
               begin
                  UI_Overlay.Hide_Key_Input;
                  LLM_Waiting_Key := False;
                  SDL_StopTextInput;
               end;
            end if;
         end;
         return;
      end if;
      if    K = SDLK_ESCAPE or else K = SDLK_q then Quit := True;
      elsif State = No_Video then return;   -- no playback keys in welcome mode
      elsif K = SDLK_SPACE  then Toggle_Play_Pause;
      elsif K = SDLK_LEFT   then Video_Decoder.Seek (-5.0); Audio.Flush;
      elsif K = SDLK_RIGHT  then Video_Decoder.Seek (+5.0); Audio.Flush;
      elsif K = SDLK_UP     then
         Volume := Integer'Min (Volume + 10, 128);
         Audio.Set_Volume (Volume);
      elsif K = SDLK_DOWN   then
         Volume := Integer'Max (Volume - 10, 0);
         Audio.Set_Volume (Volume);
      elsif K = SDLK_m      then Muted := not Muted; Audio.Toggle_Mute;
      elsif K = SDLK_l      then Looping := not Looping;
      elsif K = SDLK_f      then
         Fullscreen := not Fullscreen;
         SDL.Video.Windows.Set_Fullscreen (Win, Fullscreen);
      end if;
   end Handle_Key;

end Player;
