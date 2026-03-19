-- llm_bridge.adb
-- BlackVideo Mini Player — LLM Cloud Caption Provider Bridge  v2.4

with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.Storage_Elements;

package body LLM_Bridge is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Interfaces.C;
   use Interfaces.C.Strings;
   use System;
   use System.Storage_Elements;

   -- ── C HTTP import (from csrc/llm_client.c) ──────────────────────────────
   function BV_HTTP_Post
     (URL     : chars_ptr;
      Headers : chars_ptr;
      Payload : chars_ptr;
      Out_Buf : System.Address;
      Buf_Len : int) return int
   with Import, Convention => C, External_Name => "bv_http_post";

   -- ── Keys file path ───────────────────────────────────────────────────────
   function Keys_Path return String is
      function GetEnvironmentVariableA
        (Name : chars_ptr; Buf : System.Address; Size : unsigned)
         return unsigned
      with Import, Convention => C, External_Name => "GetEnvironmentVariableA";
      Buf : String (1 .. 512) := (others => ASCII.NUL);
      N   : chars_ptr := New_String ("APPDATA");
      Len : unsigned;
   begin
      Len := GetEnvironmentVariableA (N, Buf (1)'Address, 511);
      Free (N);
      if Len = 0 then return ".\keys.cfg"; end if;
      return Buf (1 .. Integer (Len)) & "\BlackVideo\keys.cfg";
   end Keys_Path;

   -- ── Ensure keys directory exists ─────────────────────────────────────────
   procedure Ensure_Keys_Dir is
      P : constant String := Keys_Path;
      D : Natural         := P'Last;
   begin
      while D > P'First and then P (D) /= '\' and then P (D) /= '/'
      loop D := D - 1; end loop;
      if D > P'First then
         declare
            Dir : constant String := P (P'First .. D - 1);
         begin
            if not Ada.Directories.Exists (Dir) then
               Ada.Directories.Create_Path (Dir);
            end if;
         end;
      end if;
   exception
      when others => null;
   end Ensure_Keys_Dir;

   -- ── Key file helpers ─────────────────────────────────────────────────────
   function Provider_Key_Name (P : Provider) return String is
   begin
      case P is
         when Claude   => return "ANTHROPIC_API_KEY";
         when OpenAI   => return "OPENAI_API_KEY";
         when Gemini   => return "GEMINI_API_KEY";
         when DeepSeek => return "DEEPSEEK_API_KEY";
         when Grok     => return "GROK_API_KEY";
      end case;
   end Provider_Key_Name;

   function Provider_Name (P : Provider) return String is
   begin
      case P is
         when Claude   => return "Claude (Anthropic)";
         when OpenAI   => return "OpenAI";
         when Gemini   => return "Gemini (Google)";
         when DeepSeek => return "DeepSeek";
         when Grok     => return "Grok (xAI)";
      end case;
   end Provider_Name;

   function Get_API_Key (P : Provider) return String is
      KN   : constant String := Provider_Key_Name (P);
      Path : constant String := Keys_Path;
      F    : File_Type;
      Buf  : String (1 .. 2048);
      Last : Natural;
   begin
      if not Ada.Directories.Exists (Path) then return ""; end if;
      Open (F, In_File, Path);
      while not End_Of_File (F) loop
         Get_Line (F, Buf, Last);
         declare
            Line : constant String := Buf (1 .. Last);
         begin
            if Line'Length > KN'Length + 1 and then
               Line (Line'First .. Line'First + KN'Length - 1) = KN and then
               Line (Line'First + KN'Length) = '='
            then
               Close (F);
               return Line (Line'First + KN'Length + 1 .. Line'Last);
            end if;
         end;
      end loop;
      Close (F);
      return "";
   exception
      when others => return "";
   end Get_API_Key;

   procedure Set_API_Key (P : Provider; Key : String) is
      KN   : constant String := Provider_Key_Name (P);
      Path : constant String := Keys_Path;

      --  Read all existing lines, replace or append
      Lines : array (1 .. 16) of Unbounded_String;
      Count : Natural := 0;
      Found : Boolean := False;
      F     : File_Type;
      Buf   : String (1 .. 2048);
      Last  : Natural;
   begin
      Ensure_Keys_Dir;

      if Ada.Directories.Exists (Path) then
         Open (F, In_File, Path);
         while not End_Of_File (F) and then Count < Lines'Last loop
            Get_Line (F, Buf, Last);
            Count := Count + 1;
            Lines (Count) := To_Unbounded_String (Buf (1 .. Last));
         end loop;
         Close (F);
      end if;

      --  Replace matching line or append
      for I in 1 .. Count loop
         declare
            L : constant String := To_String (Lines (I));
         begin
            if L'Length > KN'Length and then
               L (L'First .. L'First + KN'Length - 1) = KN
            then
               Lines (I) := To_Unbounded_String (KN & "=" & Key);
               Found := True;
            end if;
         end;
      end loop;
      if not Found then
         Count := Count + 1;
         Lines (Count) := To_Unbounded_String (KN & "=" & Key);
      end if;

      Create (F, Out_File, Path);
      for I in 1 .. Count loop
         Put_Line (F, To_String (Lines (I)));
      end loop;
      Close (F);
   exception
      when others => null;
   end Set_API_Key;

   -- ── JSON string escaper ──────────────────────────────────────────────────
   function JSON_Escape (S : String) return String is
      R : Unbounded_String;
   begin
      for C of S loop
         case C is
            when '"'      => Append (R, "\""");
            when '\'      => Append (R, "\\");
            when ASCII.LF => Append (R, "\n");
            when ASCII.CR => Append (R, "\r");
            when ASCII.HT => Append (R, "\t");
            when others   => Append (R, C);
         end case;
      end loop;
      return To_String (R);
   end JSON_Escape;

   -- ── Build SRT prompt ─────────────────────────────────────────────────────
   function Build_Prompt (Transcript : String; Duration_Sec : Float)
     return String
   is
      Dur_Str : constant String :=
        Integer'Image (Integer (Duration_Sec)) & " seconds";
   begin
      return
        "Convert the following transcript into a valid SRT subtitle file. " &
        "The video is " & Dur_Str & " long. " &
        "Use natural phrasing and split lines at sentence boundaries. " &
        "Each subtitle entry should be 1-2 lines, max 42 characters per line. " &
        "Output ONLY the raw SRT content with no extra text, markdown, or " &
        "explanation." &
        ASCII.LF & ASCII.LF &
        "Transcript:" & ASCII.LF &
        Transcript;
   end Build_Prompt;

   -- ── Provider-specific request builders ──────────────────────────────────

   function Claude_Request (Key, Prompt : String) return String is
   begin
      return
        "{""model"":""claude-3-haiku-20240307""," &
        """max_tokens"":4096," &
        """messages"":[{""role"":""user"",""content"":""" &
        JSON_Escape (Prompt) & """}]}";
   end Claude_Request;

   function OpenAI_Request (Key, Prompt : String) return String is
   begin
      return
        "{""model"":""gpt-4o-mini""," &
        """messages"":[{""role"":""user"",""content"":""" &
        JSON_Escape (Prompt) & """}]}";
   end OpenAI_Request;

   function Gemini_Request (Prompt : String) return String is
   begin
      return
        "{""contents"":[{""parts"":[{""text"":""" & Prompt & """}]}]}";
   end Gemini_Request;

   function DeepSeek_Request (Prompt : String) return String is
   begin
      return
        "{""model"":""deepseek-chat""," &
        """messages"":[{""role"":""user"",""content"":""" & JSON_Escape (Prompt) & """}]}";
   end DeepSeek_Request;

   function Grok_Request (Prompt : String) return String is
   begin
      return
        "{""model"":""grok-2-latest""," &
        """messages"":[{""role"":""user"",""content"":""" & JSON_Escape (Prompt) & """}]}";
   end Grok_Request;

   -- ── Endpoint + headers per provider ─────────────────────────────────────
   procedure Provider_Config
     (P       : Provider;
      Key     : String;
      URL     : out Unbounded_String;
      Headers : out Unbounded_String;
      Payload : out Unbounded_String;
      Prompt  : String)
   is begin
      case P is
         when Claude =>
            URL := To_Unbounded_String
              ("https://api.anthropic.com/v1/messages");
            Headers := To_Unbounded_String
              ("Content-Type: application/json" & ASCII.CR & ASCII.LF &
               "Accept: application/json" & ASCII.CR & ASCII.LF &
               "x-api-key: " & Key & ASCII.CR & ASCII.LF &
               "anthropic-version: 2023-06-01" & ASCII.CR & ASCII.LF);
            Payload := To_Unbounded_String (Claude_Request (Key, Prompt));

         when OpenAI =>
            URL := To_Unbounded_String
              ("https://api.openai.com/v1/chat/completions");
            Headers := To_Unbounded_String
              ("Content-Type: application/json" & ASCII.CR & ASCII.LF &
               "Authorization: Bearer " & Key & ASCII.CR & ASCII.LF);
            Payload := To_Unbounded_String (OpenAI_Request (Key, Prompt));

         when Gemini =>
            --  Key goes BOTH in URL query string AND x-goog-api-key header
            --  to ensure at least one method reaches the server.
            URL := To_Unbounded_String
              ("https://generativelanguage.googleapis.com/v1beta/models/" &
               "gemini-1.5-flash:generateContent?key=" & Key);
            Headers := To_Unbounded_String
              ("Content-Type: application/json" & ASCII.CR & ASCII.LF &
               "x-goog-api-key: " & Key & ASCII.CR & ASCII.LF);
            Payload := To_Unbounded_String (Gemini_Request (Prompt));

         when DeepSeek =>
            URL := To_Unbounded_String
              ("https://api.deepseek.com/v1/chat/completions");
            Headers := To_Unbounded_String
              ("Content-Type: application/json" & ASCII.CR & ASCII.LF &
               "Accept: application/json" & ASCII.CR & ASCII.LF &
               "Authorization: Bearer " & Key & ASCII.CR & ASCII.LF);
            Payload := To_Unbounded_String (DeepSeek_Request (Prompt));

         when Grok =>
            URL := To_Unbounded_String
              ("https://api.x.ai/v1/chat/completions");
            Headers := To_Unbounded_String
              ("Content-Type: application/json" & ASCII.CR & ASCII.LF &
               "Authorization: Bearer " & Key & ASCII.CR & ASCII.LF);
            Payload := To_Unbounded_String (Grok_Request (Prompt));
      end case;
   end Provider_Config;

   -- ── Extract SRT text from JSON response ────────────────────────────────
   --  Handles all five provider response formats.
   function Extract_SRT_From_Response (JSON : String) return String is
      use Ada.Strings.Unbounded;

      function Decode_JSON_String (S : String; Start : Natural) return String is
         R : Unbounded_String;
         I : Natural := Start;
      begin
         while I <= S'Last loop
            if S (I) = '\' and then I < S'Last then
               case S (I + 1) is
                  when 'n'   => Append (R, ASCII.LF);  I := I + 2;
                  when 'r'   => Append (R, ASCII.CR);  I := I + 2;
                  when 't'   => Append (R, ASCII.HT);  I := I + 2;
                  when '"' | '\' | '/' =>
                                 Append (R, S (I + 1)); I := I + 2;
                  when others => I := I + 1;
               end case;
            elsif S (I) = '"' then
               return To_String (R);
            else
               Append (R, S (I));
               I := I + 1;
            end if;
         end loop;
         return To_String (R);
      end Decode_JSON_String;

      function Find_After (S, Pat : String) return Natural is
      begin
         if Pat'Length = 0 then return 0; end if;
         for I in S'First .. S'Last - Pat'Length + 1 loop
            if S (I .. I + Pat'Length - 1) = Pat then
               return I + Pat'Length;
            end if;
         end loop;
         return 0;
      end Find_After;

      --  Ordered from most-specific to most-generic
      type Pat_String is access constant String;
      Patterns : constant array (1 .. 5) of Pat_String :=
        (new String'("""type"":""text"",""text"":"""),   --  Claude
         new String'("""parts"":[{""text"":"""),          --  Gemini
         new String'("""content"":"""),                   --  OpenAI/DeepSeek/Grok
         new String'("""text"":"""),                      --  generic
         new String'("""message"":{""role"":""assistant"",""content"":"""));

      Best   : Unbounded_String;
      Best_L : Natural := 0;
   begin
      for P of Patterns loop
         declare
            Pos : constant Natural := Find_After (JSON, P.all);
         begin
            if Pos > 0 then
               declare
                  Text : constant String := Decode_JSON_String (JSON, Pos);
               begin
                  if Text'Length > Best_L then
                     Best_L := Text'Length;
                     Best   := To_Unbounded_String (Text);
                  end if;
               end;
            end if;
         end;
      end loop;
      return To_String (Best);
   end Extract_SRT_From_Response;

   -- ── Generate ─────────────────────────────────────────────────────────────
   function Generate
     (P            : Provider;
      Video_Path   : String;
      Transcript   : String;
      Duration_Sec : Float;
      Out_SRT      : String;
      Cb           : Status_Callback := null)
     return Boolean
   is
      Key : constant String := Get_API_Key (P);
   begin
      if Key = "" then
         Put_Line ("[LLM] ERROR: No API key for " & Provider_Name (P));
         if Cb /= null then
            Cb (Provider_Name (P) & ": no API key set");
         end if;
         return False;
      end if;

      if Cb /= null then
         Cb (Provider_Name (P) & ": sending request...");
      end if;

      declare
         Prompt  : constant String := Build_Prompt (Transcript, Duration_Sec);
         URL_UB, Hdrs_UB, Body_UB : Unbounded_String;
      begin
         Provider_Config (P, Key, URL_UB, Hdrs_UB, Body_UB, Prompt);

         declare
            URL_S  : constant String := To_String (URL_UB);
            Hdrs_S : constant String := To_String (Hdrs_UB);
            Body_S : constant String := To_String (Body_UB);
            Resp   : String (1 .. 524_288) := (others => ASCII.NUL);
            CU     : chars_ptr := New_String (URL_S);
            CH     : chars_ptr := New_String (Hdrs_S);
            CB2    : chars_ptr := New_String (Body_S);
            Len    : int;
         begin
            Len := BV_HTTP_Post (CU, CH, CB2, Resp (1)'Address, Resp'Length - 1);
            Free (CU); Free (CH); Free (CB2);

            if Len <= 0 then
               Put_Line ("[LLM] ERROR: HTTP request failed for " &
                         Provider_Name (P));
               if Cb /= null then
                  Cb (Provider_Name (P) & ": request failed");
               end if;
               return False;
            end if;

            if Cb /= null then
               Cb (Provider_Name (P) & ": parsing response...");
            end if;

            declare
               SRT_Text : constant String :=
                 Extract_SRT_From_Response (Resp (1 .. Integer (Len)));
               F        : File_Type;
            begin
               if SRT_Text'Length < 10 then
                  Put_Line ("[LLM] ERROR: Empty or invalid SRT in response.");
                  Put_Line ("[LLM] Raw response (first 500 chars):");
                  if Integer (Len) > 500 then
                     Put_Line (Resp (1 .. 500));
                  elsif Integer (Len) > 0 then
                     Put_Line (Resp (1 .. Integer (Len)));
                  end if;
                  if Cb /= null then
                     Cb (Provider_Name (P) & ": failed — see console");
                  end if;
                  return False;
               end if;

               Create (F, Out_File, Out_SRT);
               Put (F, SRT_Text);
               Close (F);

               Put_Line ("[LLM] SRT written: " & Out_SRT);
               if Cb /= null then
                  Cb (Provider_Name (P) & ": done");
               end if;
               return True;
            end;
         end;
      end;
   exception
      when E : others =>
         Put_Line ("[LLM] Exception: " &
                   Ada.Exceptions.Exception_Message (E));
         if Cb /= null then Cb (Provider_Name (P) & ": error"); end if;
         return False;
   end Generate;

end LLM_Bridge;
