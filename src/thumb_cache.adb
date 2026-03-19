-- thumb_cache.adb
-- BlackVideo Mini Player — Video Thumbnail Cache  v2.4

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Storage_Elements;

package body Thumb_Cache is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Interfaces.C;
   use Interfaces.C.Strings;
   use System;
   use System.Storage_Elements;

   -- ── Process launch import ────────────────────────────────────────────────
   -- Reuses bv_create_process from csrc/process_helpers.c
   function BV_Create_Process (Cmd : chars_ptr;
                                H   : System.Address) return int
   with Import, Convention => C, External_Name => "bv_create_process";

   function BV_Process_Running (H : System.Address) return int
   with Import, Convention => C, External_Name => "bv_process_still_running";

   procedure BV_Close_Handle (H : System.Address)
   with Import, Convention => C, External_Name => "bv_close_handle";

   -- ── State ────────────────────────────────────────────────────────────────
   Generating     : Boolean         := False;
   Gen_H_Process  : System.Address  := System.Null_Address;
   Current_Video  : Unbounded_String := To_Unbounded_String ("");
   Cache_Dir_Cur  : Unbounded_String := To_Unbounded_String ("");

   -- ── Hash ─────────────────────────────────────────────────────────────────
   --  Simple 32-bit FNV-1a hash of the video path, formatted as 8 hex chars.
   function Path_Hash (S : String) return String is
      H : Interfaces.C.unsigned := 2_166_136_261;
   begin
      for C of S loop
         H := (H xor Interfaces.C.unsigned (Character'Pos (C))) * 16_777_619;
      end loop;
      declare
         Hex  : constant String := "0123456789abcdef";
         Res  : String (1 .. 8);
         Val  : Interfaces.C.unsigned := H;
      begin
         for I in reverse Res'Range loop
            Res (I) := Hex (Integer (Val and 16#F#) + 1);
            Val := Val / 16;
         end loop;
         return Res;
      end;
   end Path_Hash;

   -- ── Exe dir helper ────────────────────────────────────────────────────────
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
      if Len = 0 then return ".\"; end if;
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
      when others => return ".\";
   end Exe_Dir;

   -- ── Cache_Dir_For ─────────────────────────────────────────────────────────
   function Cache_Dir_For (Video_Path : String) return String is
   begin
      return Exe_Dir & "cache\" & Path_Hash (Video_Path) & "\";
   end Cache_Dir_For;

   -- ── Generate ─────────────────────────────────────────────────────────────
   --  Writes a small .bat to %TEMP% then launches it asynchronously.
   --  Using a .bat sidesteps all cmd.exe quoting issues for paths with spaces.
   procedure Generate (Video_Path : String) is
      Cache    : constant String := Cache_Dir_For (Video_Path);
      --  Use GetTempPath for bat to avoid permission issues in any CWD
      Bat_Path : constant String := Exe_Dir & "bv_thumb_job.bat";
      Bat_File : File_Type;
      CP       : chars_ptr;
      H        : aliased System.Address := System.Null_Address;
      Ret      : int;
   begin
      --  Close any previous handle
      if Gen_H_Process /= System.Null_Address then
         BV_Close_Handle (Gen_H_Process);
         Gen_H_Process := System.Null_Address;
      end if;

      --  Write the batch file — no shell quoting needed inside Ada strings
      begin
         Create (Bat_File, Out_File, Bat_Path);
         Put_Line (Bat_File, "@echo off");
         Put_Line (Bat_File, "if not exist """ & Cache & """ mkdir """ & Cache & """");
         Put_Line (Bat_File, "set FFMPEG=ffmpeg");
         Put_Line (Bat_File, "if exist """ & Exe_Dir & "ffmpeg.exe"" set FFMPEG=""" & Exe_Dir & "ffmpeg.exe""");
         Put_Line (Bat_File, "%FFMPEG% -y -i """ & Video_Path & """ " &
                             "-vf ""fps=1/10,scale=160:90"" " &
                             "-q:v 5 """ & Cache & "frame_%%04d.jpg"" 2>nul");
         Close (Bat_File);
      exception
         when others =>
            Put_Line ("[ThumbCache] WARNING: could not write bat: " & Bat_Path);
            return;
      end;

      --  Launch the bat asynchronously
      CP  := New_String ("cmd.exe /c """ & Bat_Path & """");
      Ret := BV_Create_Process (CP, H'Address);
      Free (CP);

      if Ret /= 0 then
         Gen_H_Process := H;
         Generating    := True;
         Current_Video := To_Unbounded_String (Video_Path);
         Cache_Dir_Cur := To_Unbounded_String (Cache);
         Put_Line ("[ThumbCache] Generating thumbnails -> " & Cache);
      else
         Put_Line ("[ThumbCache] WARNING: failed to launch ffmpeg thumbnail job");
      end if;
   end Generate;

   -- ── Is_Generating ────────────────────────────────────────────────────────
   function Is_Generating return Boolean is
   begin
      if not Generating then return False; end if;
      if Gen_H_Process /= System.Null_Address then
         if BV_Process_Running (Gen_H_Process) = 0 then
            --  Job finished
            BV_Close_Handle (Gen_H_Process);
            Gen_H_Process := System.Null_Address;
            Generating    := False;
            Put_Line ("[ThumbCache] Generation complete.");
         end if;
      end if;
      return Generating;
   end Is_Generating;

   -- ── Frame_Path ───────────────────────────────────────────────────────────
   function Frame_Path (Video_Path : String; Pos_Seconds : Float)
     return String
   is
      Cache : constant String  := Cache_Dir_For (Video_Path);
      Idx   : constant Integer :=
        Integer (Float'Floor (Pos_Seconds / Thumb_Interval)) + 1;

      --  Build frame_%04d.jpg filename
      function Pad4 (N : Integer) return String is
         S : String (1 .. 4) := "0000";
         T : constant String := Integer'Image (N);
         --  Integer'Image includes a leading space for non-negatives
         Digits_Start : constant Natural :=
           (if T (T'First) = ' ' then T'First + 1 else T'First);
         Digit_Str : constant String := T (Digits_Start .. T'Last);
         Fill   : constant Natural := 4 - Digit_Str'Length;
      begin
         if Fill < 0 then return Digit_Str (Digit_Str'Last - 3 .. Digit_Str'Last); end if;
         S (Fill + 1 .. 4) := Digit_Str;
         return S;
      end Pad4;

      Path : constant String := Cache & "frame_" & Pad4 (Idx) & ".jpg";
   begin
      if Ada.Directories.Exists (Path) then return Path; end if;
      return "";
   end Frame_Path;

   -- ── Clear ────────────────────────────────────────────────────────────────
   procedure Clear (Video_Path : String) is
      Cache : constant String := Cache_Dir_For (Video_Path);
   begin
      if Ada.Directories.Exists (Cache) then
         Ada.Directories.Delete_Tree (Cache);
         Put_Line ("[ThumbCache] Cleared: " & Cache);
      end if;
   exception
      when others => null;
   end Clear;

   -- ── Clear_All ────────────────────────────────────────────────────────────
   procedure Clear_All is
      All_Cache : constant String := Exe_Dir & "cache\";
   begin
      if Ada.Directories.Exists (All_Cache) then
         Ada.Directories.Delete_Tree (All_Cache);
         Put_Line ("[ThumbCache] All caches cleared.");
      end if;
   exception
      when others => null;
   end Clear_All;

end Thumb_Cache;
