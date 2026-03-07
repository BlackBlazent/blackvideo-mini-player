-- player.adb
-- BlackVideo Mini Player — Core Player  v2.3
--
-- v2.3 changes:
--   BUG FIX: After an Open_Dialog call (file picker), SDL queues pending
--     MOUSEBUTTONDOWN events that fire after the dialog closes.  This caused
--     the context menu to re-open and immediately fire CTX_SUB_NONE,
--     resetting Sub_Active to -1 right after the user selected a track.
--     Fix: record the SDL tick at which the ctx menu was last closed; ignore
--     any MOUSEBUTTONDOWN that arrives within 250 ms of that moment.
--
--   NEW: Subtitle text rendering — SRT_Parser loads the .srt file and
--     current cue text is pushed to the UI overlay every frame.
--
--   NEW: Whisper.cpp offline caption generation and translation, run as
--     a background Windows job (player stays responsive while Whisper runs).
--     Results auto-loaded into Track 1 when complete.

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Storage_Elements;

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

package body Player is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Interfaces.C;
   use System;
   use System.Storage_Elements;
   use SDL.Events;
   use SDL.Events.Keyboards;

   -- ── Player state ────────────────────────────────────────────────────────
   State      : Player_State     := Stopped;
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

   -- Loaded SRT data for the active track
   Active_SRT    : SRT_Parser.SRT_File;
   Active_SRT_OK : Boolean := False;

   -- ── Context menu guard: ignore clicks for 250 ms after close ─────────
   Ctx_Close_Tick : unsigned := 0;
   CTX_GUARD_MS   : constant unsigned := 250;

   -- ── Whisper background job ─────────────────────────────────────────────
   -- We launch whisper in a detached cmd.exe /c process and poll for the
   -- output SRT file.  When it appears, we auto-load it.
   Whisper_Running    : Boolean         := False;
   Whisper_Out_SRT    : Unbounded_String := To_Unbounded_String ("");
   Whisper_Video_Path : Unbounded_String := To_Unbounded_String ("");

   -- ── Auto-hide bar ─────────────────────────────────────────────────────
   Bar_Until   : unsigned := 0;
   Bar_Visible : Boolean  := True;
   Hide_MS     : constant unsigned := 3_000;

   -- ── Accessors ────────────────────────────────────────────────────────
   function Current_State  return Player_State is (State);
   function Current_Volume return Integer      is (Volume);
   function Is_Fullscreen  return Boolean      is (Fullscreen);

   -- ── Forward declarations ─────────────────────────────────────────────
   procedure Handle_Key (Ev : SDL_Event; Win : SDL.Video.Window_Handle;
                         Quit : out Boolean);
   procedure Handle_Motion (Ev : SDL_Event);
   procedure Handle_Button_Down (Ev : SDL_Event; Win : SDL.Video.Window_Handle;
                                 Quit : out Boolean);
   procedure Handle_Button_Up   (Ev : SDL_Event);
   procedure Handle_Wheel       (Ev : SDL_Event);
   procedure Show_Bar;
   procedure Sync_Subs;
   procedure Toggle_Play_Pause;
   procedure Load_Active_SRT;
   procedure Check_Whisper_Done;
   procedure Launch_Whisper (Video_Path : String; Translate : Boolean);

   -- ── Show_Bar ──────────────────────────────────────────────────────────
   procedure Show_Bar is
   begin
      Bar_Until   := SDL.Get_Ticks + Hide_MS;
      Bar_Visible := True;
   end Show_Bar;

   -- ── Sync_Subs — push subtitle table to C overlay ─────────────────────
   procedure Sync_Subs is
   begin
      UI_Overlay.Set_Subtitles
        (To_String (Sub_Path (0)),
         To_String (Sub_Path (1)),
         To_String (Sub_Path (2)),
         Sub_Count, Sub_Active);
   end Sync_Subs;

   -- ── Load_Active_SRT — parse the SRT for the active track ─────────────
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

   -- ── Check_Whisper_Done — poll for completed background job ───────────
   procedure Check_Whisper_Done is
   begin
      if not Whisper_Running then return; end if;
      declare
         SRT_Path : constant String := To_String (Whisper_Out_SRT);
      begin
         if SRT_Path'Length > 0 and then
            Ada.Directories.Exists (SRT_Path)
         then
            -- SRT appeared — load it into Track 1
            Whisper_Running := False;
            UI_Overlay.Set_Whisper_Status ("");
            Sub_Path  (0) := To_Unbounded_String (SRT_Path);
            Sub_Count     := Integer'Max (Sub_Count, 1);
            Sub_Active    := 0;
            Sync_Subs;
            Load_Active_SRT;
            Put_Line ("[Whisper] Auto-loaded: " & SRT_Path);
         end if;
      end;
   end Check_Whisper_Done;

   -- ── Launch_Whisper — start background transcription ──────────────────
   procedure Launch_Whisper (Video_Path : String; Translate : Boolean) is
      Whisper_Exe : constant String := Whisper_Bridge.Find_Whisper;
      Model_Path  : constant String := Whisper_Bridge.Find_Model;
   begin
      if Whisper_Exe = "" then
         Put_Line ("[Whisper] ERROR: whisper-cli.exe not found.");
         Put_Line ("[Whisper]   Place whisper-cli.exe in build\ or");
         Put_Line ("[Whisper]   set BLACKVIDEO_WHISPER_PATH env var.");
         UI_Overlay.Set_Whisper_Status ("Whisper not found — see console");
         return;
      end if;

      if Model_Path = "" then
         Put_Line ("[Whisper] ERROR: model not found.");
         Put_Line ("[Whisper]   Place ggml-base.bin in build\models\ or");
         Put_Line ("[Whisper]   set BLACKVIDEO_WHISPER_MODEL env var.");
         UI_Overlay.Set_Whisper_Status ("Model not found — see console");
         return;
      end if;

      if Whisper_Running then
         Put_Line ("[Whisper] Already running, please wait.");
         return;
      end if;

      -- Derive output SRT path (beside the video)
      declare
         Out_SRT : constant String := Video_Path & ".srt";

         -- Build the FFmpeg + whisper command as a batch that writes a
         -- sentinel file when done. We run it in a hidden cmd.exe window.
         Tmp_WAV     : constant String := Video_Path & ".tmp.wav";
         Trans_Flag  : constant String :=
           (if Translate then " --translate" else "");

         -- The full command sequence:
         --   1. ffmpeg extracts 16kHz mono WAV
         --   2. whisper-cli transcribes → .srt  (whisper names it <base>.srt)
         --   3. del temp wav
         Batch_Cmd : constant String :=
           "cmd.exe /c """ &
           "ffmpeg -y -i """ & Video_Path & """ " &
           "-ar 16000 -ac 1 -f wav """ & Tmp_WAV & """ 2>nul" &
           " && " &
           """" & Whisper_Exe & """ " &
           "-m """ & Model_Path & """ " &
           "-f """ & Tmp_WAV & """ " &
           "--output-srt" & Trans_Flag & " " &
           "-of """ & Video_Path & """ " &
           "2>nul" &
           " && del /q """ & Tmp_WAV & """ 2>nul" &
           """";

         function WinExec (Cmd : Interfaces.C.Strings.chars_ptr;
                           Show : unsigned) return unsigned
         with Import, Convention => C, External_Name => "WinExec";

         CP  : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String (Batch_Cmd);
         Ret : unsigned;
      begin
         Ret := WinExec (CP, 0);   -- 0 = SW_HIDE
         Interfaces.C.Strings.Free (CP);

         if Ret < 32 then
            Put_Line ("[Whisper] ERROR: WinExec failed (code" &
                      unsigned'Image (Ret) & ").");
            return;
         end if;

         Whisper_Running    := True;
         Whisper_Out_SRT    := To_Unbounded_String (Out_SRT);
         Whisper_Video_Path := To_Unbounded_String (Video_Path);

         if Translate then
            UI_Overlay.Set_Whisper_Status ("Whisper: translating...");
            Put_Line ("[Whisper] Launched translation job.");
         else
            UI_Overlay.Set_Whisper_Status ("Whisper: generating captions...");
            Put_Line ("[Whisper] Launched transcription job.");
         end if;
         Put_Line ("[Whisper] Output: " & Out_SRT);
         Put_Line ("[Whisper] Player stays responsive while Whisper runs.");
      end;
   end Launch_Whisper;

   -- ── Toggle_Play_Pause ────────────────────────────────────────────────
   procedure Toggle_Play_Pause is
   begin
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

   function SDL_GetMouseState (X, Y : System.Address) return unsigned
   with Import, Convention => C, External_Name => "SDL_GetMouseState";

   -- ══════════════════════════════════════════════════════════════════════
   --  Run
   -- ══════════════════════════════════════════════════════════════════════
   procedure Run (Video_File : String) is
      Win  : SDL.Video.Window_Handle;
      Rend : SDL.Video.Renderer_Handle;
      Tex  : SDL.Video.Texture_Handle;
      Ev   : SDL_Event;
      Quit : Boolean  := False;

      Vid_W, Vid_H, Frame_Delay_MS : Integer;
      Ret : int;

      Frame_Start, Elapsed, Sleep_MS : unsigned;
   begin
      Put_Line ("[Player] Init SDL2 ...");
      Ret := SDL.Initialize (SDL.Flags.Enable_Video or SDL.Flags.Enable_Audio);
      if Ret /= 0 then raise Program_Error with "[Player] SDL_Init failed"; end if;

      Put_Line ("[Player] Opening: " & Video_File);
      Video_Decoder.Open (Video_File, Vid_W, Vid_H, Frame_Delay_MS);
      -- Store path so Whisper knows which file to transcribe
      Whisper_Video_Path := To_Unbounded_String (Video_File);

      SDL.Video.Windows.Create
        (Win,
         Title  => "BlackVideo  " & Utils.Base_Name (Video_File),
         X      => SDL.Video.Windows.SDL_WINDOWPOS_CENTERED,
         Y      => SDL.Video.Windows.SDL_WINDOWPOS_CENTERED,
         Width  => int (Vid_W),
         Height => int (Vid_H),
         Flags  => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Create
        (Rend, Win,
         Flags => SDL.Video.Renderers.SDL_RENDERER_ACCELERATED
                  or SDL.Video.Renderers.SDL_RENDERER_PRESENTVSYNC);

      Renderer.Init_Texture (Rend, Tex, Vid_W, Vid_H);
      Audio.Init (Volume);

      UI_Overlay.Init (Find_Font);
      UI_Overlay.Set_Window_Size (Vid_W, Vid_H);
      Sync_Subs;
      Show_Bar;

      Video_Decoder.Start_Decoding;
      Audio.Start;

      State := Playing;
      Put_Line ("[Player] Playing.");
      Put_Line ("[Player]   Keys: SPACE=pause  LEFT/RIGHT=seek  UP/DOWN=vol");
      Put_Line ("[Player]         M=mute  L=loop  F=fullscreen  Q/ESC=quit");
      Put_Line ("[Player]   Mouse: click=pause/play  drag-bar=seek  right-click=menu");
      Put_Line ("[Player]   Whisper: right-click → Generate Captions or Translate");

      -- ── Main loop ──────────────────────────────────────────────────────
      while not Quit loop

         Frame_Start := SDL.Get_Ticks;

         if SDL.Get_Ticks > Bar_Until and then not UI_Overlay.Is_Seeking then
            Bar_Visible := False;
         end if;

         -- Poll for completed Whisper background job
         Check_Whisper_Done;

         -- ── Event pump ────────────────────────────────────────────────
         while SDL.Events.Poll (Ev) loop
            declare
               T : constant unsigned := SDL.Events.Event_Type (Ev);
            begin
               if    T = SDL_QUIT            then Quit := True;
               elsif T = SDL_KEYDOWN         then Handle_Key         (Ev, Win, Quit);
               elsif T = SDL_MOUSEMOTION     then Handle_Motion       (Ev);
               elsif T = SDL_MOUSEBUTTONDOWN then Handle_Button_Down  (Ev, Win, Quit);
               elsif T = SDL_MOUSEBUTTONUP   then Handle_Button_Up    (Ev);
               elsif T = SDL_MOUSEWHEEL      then Handle_Wheel        (Ev);
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

         -- Scrubber drag
         if UI_Overlay.Is_Seeking then
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

         -- ── Update subtitle cue text ──────────────────────────────────
         if Active_SRT_OK and then Sub_Active >= 0 then
            declare
               Pos : constant Float := Video_Decoder.Get_Position;
               Cue : constant String :=
                 SRT_Parser.Current_Text (Active_SRT, Pos);
            begin
               UI_Overlay.Set_Sub_Text (Cue);
            end;
         end if;

         -- Draw video
         Renderer.Draw (Rend, Tex, Vid_W, Vid_H);

         -- Draw UI overlay
         UI_Overlay.Draw
           (Renderer   => Renderer.To_Address (Rend),
            Position   => Video_Decoder.Get_Position,
            Duration   => Video_Decoder.Get_Duration,
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
      Video_Decoder.Close;
      Renderer.Destroy_Texture (Tex);
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
         begin
            pragma Unreferenced (Ign);
         end;
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

   -- ── Handle_Button_Down ───────────────────────────────────────────────
   procedure Handle_Button_Down
     (Ev   : SDL_Event;
      Win  : SDL.Video.Window_Handle;
      Quit : out Boolean)
   is
      X   : constant Integer       := Integer (SDL.Events.Mouse_Btn_X (Ev));
      Y   : constant Integer       := Integer (SDL.Events.Mouse_Btn_Y (Ev));
      Btn : constant unsigned_char := SDL.Events.Mouse_Button (Ev);
   begin
      Quit := False;
      Show_Bar;

      -- ── Right-click: open/close context menu ────────────────────────
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

      -- ── GUARD: ignore clicks arriving within 250ms of ctx menu close ─
      -- This absorbs queued MOUSEBUTTONDOWN events that SDL buffers while
      -- the file-dialog modal is open (Open_Dialog blocks via popen).
      if SDL.Get_Ticks - Ctx_Close_Tick < CTX_GUARD_MS then
         return;
      end if;

      -- ── Context menu left-click ───────────────────────────────────────
      if UI_Overlay.Ctx_Open then
         declare
            Item : constant int := UI_Overlay.Hit_Ctx (X, Y);
         begin
            UI_Overlay.Close_Ctx;
            Ctx_Close_Tick := SDL.Get_Ticks;

            if Item = UI_Overlay.CTX_OPEN_FILE then
               declare
                  Path : constant String :=
                    Open_Dialog ("Open Video",
                      "Video files|*.mp4;*.mkv;*.avi;*.mov;*.wmv;*.flv;*.webm|All files|*.*");
               begin
                  if Path'Length > 0 then
                     Put_Line ("[Player] Open: " & Path);
                     Put_Line ("[Player] Quit and run:");
                     Put_Line ("[Player]   blackvideo-player.exe """ & Path & """");
                  end if;
               end;
               Ctx_Close_Tick := SDL.Get_Ticks;

            elsif Item = UI_Overlay.CTX_SUB_NONE then
               Sub_Active    := -1;
               Active_SRT_OK := False;
               UI_Overlay.Set_Sub_Text ("");
               Sync_Subs;
               Put_Line ("[Player] Subtitle: off");

            elsif Item >= UI_Overlay.CTX_SUB_1
              and then Item <= UI_Overlay.CTX_SUB_3
            then
               declare
                  Idx : constant Integer :=
                    Integer (Item) - Integer (UI_Overlay.CTX_SUB_1);
               begin
                  if Idx < Sub_Count then
                     -- Track already loaded — just activate it
                     Sub_Active := Idx;
                     Sync_Subs;
                     Load_Active_SRT;
                     Put_Line ("[Player] Subtitle track" & Integer'Image (Idx + 1));
                  else
                     -- Slot empty — ask user to pick an SRT
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
                           Put_Line ("[Player] Subtitle loaded: " & Path);
                        end if;
                     end;
                     -- Reset guard after file dialog
                     Ctx_Close_Tick := SDL.Get_Ticks;
                  end if;
               end;

            elsif Item = UI_Overlay.CTX_WHISPER_GEN then
               Launch_Whisper (To_String (Whisper_Video_Path),
                               Translate => False);

            elsif Item = UI_Overlay.CTX_WHISPER_TRANS then
               Launch_Whisper (To_String (Whisper_Video_Path),
                               Translate => True);

            end if;
         end;
         return;
      end if;

      -- ── Seek bar ─────────────────────────────────────────────────────
      if UI_Overlay.In_Bar (X, Y) and then UI_Overlay.Hit_Seek (X, Y) then
         UI_Overlay.Set_Seeking (True);
         Video_Decoder.Seek_To
           (UI_Overlay.Seek_Frac (X) * Video_Decoder.Get_Duration);
         Audio.Flush;
         return;
      end if;

      -- ── Control buttons ──────────────────────────────────────────────
      if UI_Overlay.In_Bar (X, Y) then
         declare
            B : constant int := UI_Overlay.Hit_Button (X, Y);
         begin
            if B = UI_Overlay.BTN_PLAY_PAUSE then
               Toggle_Play_Pause;

            elsif B = UI_Overlay.BTN_PREV then
               Video_Decoder.Seek_To (0.0);
               Audio.Flush;
               Put_Line ("[Player] Seek to start");

            elsif B = UI_Overlay.BTN_NEXT then
               Video_Decoder.Seek_To
                 (Float'Max (0.0, Video_Decoder.Get_Duration - 3.0));
               Audio.Flush;
               Put_Line ("[Player] Seek to end");

            elsif B = UI_Overlay.BTN_LOOP then
               Looping := not Looping;
               Put_Line ("[Player] Loop=" & Boolean'Image (Looping));

            elsif B = UI_Overlay.BTN_VOLUME then
               Muted := not Muted;
               Audio.Toggle_Mute;
               Put_Line ("[Player] Mute=" & Boolean'Image (Muted));

            elsif B = UI_Overlay.BTN_SPEED then
               Speed_Idx := UI_Overlay.Speed_Index
                              ((int (Speed_Idx) + 1) mod 4);
               Put_Line ("[Player] Speed="
                         & Float'Image (UI_Overlay.Speed_Values (Speed_Idx))
                         & "x");

            elsif B = UI_Overlay.BTN_FULLSCREEN then
               Fullscreen := not Fullscreen;
               SDL.Video.Windows.Set_Fullscreen (Win, Fullscreen);
               Put_Line ("[Player] Fullscreen=" & Boolean'Image (Fullscreen));

            elsif B = UI_Overlay.BTN_MENU then
               UI_Overlay.Open_Ctx (X, Y - 160);
            end if;
         end;
         return;
      end if;

      -- ── Click on video: toggle play/pause ────────────────────────────
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

      if    K = SDLK_ESCAPE or else K = SDLK_q then Quit := True;
      elsif K = SDLK_SPACE  then Toggle_Play_Pause;
      elsif K = SDLK_LEFT   then Video_Decoder.Seek (-5.0); Audio.Flush;
                                  Put_Line ("[Player] Seek -5 s");
      elsif K = SDLK_RIGHT  then Video_Decoder.Seek (+5.0); Audio.Flush;
                                  Put_Line ("[Player] Seek +5 s");
      elsif K = SDLK_UP     then
         Volume := Integer'Min (Volume + 10, 128);
         Audio.Set_Volume (Volume);
         Put_Line ("[Player] Volume =" & Integer'Image (Volume));
      elsif K = SDLK_DOWN   then
         Volume := Integer'Max (Volume - 10, 0);
         Audio.Set_Volume (Volume);
         Put_Line ("[Player] Volume =" & Integer'Image (Volume));
      elsif K = SDLK_m      then
         Muted := not Muted;
         Audio.Toggle_Mute;
      elsif K = SDLK_l      then
         Looping := not Looping;
         Put_Line ("[Player] Loop=" & Boolean'Image (Looping));
      elsif K = SDLK_f      then
         Fullscreen := not Fullscreen;
         SDL.Video.Windows.Set_Fullscreen (Win, Fullscreen);
         Put_Line ("[Player] Fullscreen=" & Boolean'Image (Fullscreen));
      end if;
   end Handle_Key;

end Player;
