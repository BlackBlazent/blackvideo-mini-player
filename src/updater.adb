-- updater.adb
-- BlackVideo Mini Player — GitHub Release Auto-Updater  v2.4

with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Storage_Elements;

package body Updater is

   use Ada.Text_IO;
   use Interfaces.C;
   use Interfaces.C.Strings;
   use System;
   use System.Storage_Elements;

   -- ── C imports (from csrc/llm_client.c) ──────────────────────────────────
   function BV_HTTP_Get
     (URL     : chars_ptr;
      Buf     : System.Address;
      Buf_Len : int) return int
   with Import, Convention => C, External_Name => "bv_http_get";

   procedure ShellExecuteA
     (HWnd      : System.Address;
      Operation : chars_ptr;
      File_Arg  : chars_ptr;
      Params    : System.Address;
      Dir       : System.Address;
      Show      : int)
   with Import, Convention => C, External_Name => "ShellExecuteA";

   -- Background thread via CreateThread
   function CreateThread
     (Attrs     : System.Address;
      Stack     : unsigned;
      Start_Fn  : System.Address;
      Param     : System.Address;
      Flags     : unsigned;
      Thread_ID : System.Address) return System.Address
   with Import, Convention => C, External_Name => "CreateThread";

   -- ── State (written by thread, read by main loop) ─────────────────────────
   Done              : Boolean := False;
   Has_Update        : Boolean := False;
   Rem_Version_Buf   : String (1 .. 32)   := (others => ' ');
   Rem_Version_Len   : Natural             := 0;
   Notes_Buf         : String (1 .. 256)  := (others => ' ');
   Notes_Len         : Natural             := 0;
   Download_Buf      : String (1 .. 512)  := (others => ' ');
   Download_Len      : Natural             := 0;

   -- ── JSON field extractor ─────────────────────────────────────────────────
   --  Pulls the string value of a JSON key from a flat JSON buffer.
   --  Works for simple single-level JSON without nested objects.
   function Extract_Field (JSON, Key : String) return String is
      Search : constant String := """" & Key & """";
      Pos    : Natural := 0;
   begin
      --  Find key
      for I in JSON'First .. JSON'Last - Search'Length loop
         if JSON (I .. I + Search'Length - 1) = Search then
            Pos := I + Search'Length;
            exit;
         end if;
      end loop;
      if Pos = 0 then return ""; end if;

      --  Skip whitespace, colon, whitespace
      while Pos <= JSON'Last and then
            (JSON (Pos) = ' ' or else JSON (Pos) = ':' or else
             JSON (Pos) = ASCII.HT or else JSON (Pos) = ASCII.LF or else
             JSON (Pos) = ASCII.CR)
      loop Pos := Pos + 1; end loop;

      if Pos > JSON'Last or else JSON (Pos) /= '"' then return ""; end if;
      Pos := Pos + 1;  -- skip opening quote

      declare
         Start : constant Natural := Pos;
      begin
         while Pos <= JSON'Last and then JSON (Pos) /= '"' loop
            Pos := Pos + 1;
         end loop;
         if Pos > JSON'Last then return ""; end if;
         return JSON (Start .. Pos - 1);
      end;
   end Extract_Field;

   -- ── Background check (called from a Windows thread) ─────────────────────
   procedure Do_Check is
      Buf : String (1 .. 4096) := (others => ASCII.NUL);
      URL_C : chars_ptr := New_String (Manifest_URL);
      Len   : int;
   begin
      Len := BV_HTTP_Get (URL_C, Buf (1)'Address, Buf'Length - 1);
      Free (URL_C);

      if Len <= 0 then
         Put_Line ("[Updater] Could not reach manifest URL.");
         Done := True;
         return;
      end if;

      declare
         JSON    : constant String := Buf (1 .. Integer (Len));
         Ver     : constant String := Extract_Field (JSON, "version");
         Notes   : constant String := Extract_Field (JSON, "notes");
         Dl_URL  : constant String := Extract_Field (JSON, "download_url");
      begin
         if Ver'Length > 0 then
            Rem_Version_Len := Integer'Min (Ver'Length, Rem_Version_Buf'Length);
            Rem_Version_Buf (1 .. Rem_Version_Len) :=
              Ver (Ver'First .. Ver'First + Rem_Version_Len - 1);

            Notes_Len := Integer'Min (Notes'Length, Notes_Buf'Length);
            Notes_Buf (1 .. Notes_Len) :=
              Notes (Notes'First .. Notes'First + Notes_Len - 1);

            Download_Len := Integer'Min (Dl_URL'Length, Download_Buf'Length);
            Download_Buf (1 .. Download_Len) :=
              Dl_URL (Dl_URL'First .. Dl_URL'First + Download_Len - 1);

            Has_Update := Is_Newer (Ver, Current_Version);
            if Has_Update then
               Put_Line ("[Updater] Update available: v" & Ver);
            else
               Put_Line ("[Updater] Up to date (remote: v" & Ver & ")");
            end if;
         end if;
      end;

      Done := True;
   exception
      when others => Done := True;
   end Do_Check;

   -- ── Thread entry point trampoline ────────────────────────────────────────
   --  CreateThread requires a C-callable stdcall function; we use a simple
   --  convention => Stdcall wrapper.
   function Thread_Proc (Param : System.Address) return unsigned
   with Convention => Stdcall;

   function Thread_Proc (Param : System.Address) return unsigned is
      pragma Unreferenced (Param);
   begin
      Do_Check;
      return 0;
   end Thread_Proc;

   -- ── Public API ───────────────────────────────────────────────────────────
   procedure Start_Check is
      H : System.Address;
   begin
      Done       := False;
      Has_Update := False;
      H := CreateThread
        (System.Null_Address, 0,
         Thread_Proc'Address,
         System.Null_Address, 0,
         System.Null_Address);
      pragma Unreferenced (H);
   end Start_Check;

   function Check_Done       return Boolean is (Done);
   function Update_Available return Boolean is (Has_Update);

   function Remote_Version return String is
   begin
      if Rem_Version_Len = 0 then return ""; end if;
      return Rem_Version_Buf (1 .. Rem_Version_Len);
   end Remote_Version;

   function Release_Notes return String is
   begin
      if Notes_Len = 0 then return ""; end if;
      return Notes_Buf (1 .. Notes_Len);
   end Release_Notes;

   function Download_URL return String is
   begin
      if Download_Len = 0 then return ""; end if;
      return Download_Buf (1 .. Download_Len);
   end Download_URL;

   -- ── Is_Newer ─────────────────────────────────────────────────────────────
   function Is_Newer (Remote, Local : String) return Boolean is

      function Parse_Part (S : String; Start : out Natural) return Natural is
         I   : Natural := Start;
         Num : Natural := 0;
      begin
         while I <= S'Last and then S (I) /= '.' loop
            if S (I) in '0' .. '9' then
               Num := Num * 10 + Character'Pos (S (I)) - Character'Pos ('0');
            end if;
            I := I + 1;
         end loop;
         if I <= S'Last and then S (I) = '.' then I := I + 1; end if;
         Start := I;
         return Num;
      end Parse_Part;

      RP, LP : Natural;
      RM, LM, Rn, Ln, Rp2, Lp2 : Natural;
   begin
      RP := Remote'First;
      LP := Local'First;
      RM := Parse_Part (Remote, RP);
      LM := Parse_Part (Local,  LP);
      if RM /= LM then return RM > LM; end if;
      Rn := Parse_Part (Remote, RP);
      Ln := Parse_Part (Local,  LP);
      if Rn /= Ln then return Rn > Ln; end if;
      Rp2 := Parse_Part (Remote, RP);
      Lp2 := Parse_Part (Local,  LP);
      return Rp2 > Lp2;
   end Is_Newer;

   -- ── Open_Browser ─────────────────────────────────────────────────────────
   procedure Open_Browser (URL : String) is
      Op  : chars_ptr := New_String ("open");
      Shell_Url : chars_ptr := New_String (URL);
   begin
      ShellExecuteA
        (System.Null_Address, Op, Shell_Url,
         System.Null_Address, System.Null_Address, 1);
      Free (Op); Free (Shell_Url);
   end Open_Browser;

end Updater;
