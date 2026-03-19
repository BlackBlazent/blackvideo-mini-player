-- sdl-events.adb
-- Flat byte-field extractors for SDL_Event.
-- Uses Unchecked_Conversion so unsigned→int reinterpret never raises
-- Constraint_Error (even when the high bit is set).

with Ada.Unchecked_Conversion;

package body SDL.Events is

   use Interfaces.C;

   -- Safe bit-reinterpret: unsigned → int (no range check)
   function U32_To_S32 is new Ada.Unchecked_Conversion
     (Source => unsigned, Target => int);

   -- ─── Read 4 bytes little-endian → unsigned ────────────────────────────
   function Read_U32 (B : Raw_Bytes; Offset : Natural) return unsigned is
   begin
      return unsigned (B (Offset))
           + unsigned (B (Offset + 1)) * 256
           + unsigned (B (Offset + 2)) * 65_536
           + unsigned (B (Offset + 3)) * 16_777_216;
   end Read_U32;

   -- ─── Read 4 bytes → signed int (bit-exact reinterpret) ───────────────
   function Read_S32 (B : Raw_Bytes; Offset : Natural) return int is
   begin
      return U32_To_S32 (Read_U32 (B, Offset));
   end Read_S32;

   -- ─── Public extractors ────────────────────────────────────────────────

   function Event_Type (E : SDL_Event) return unsigned is
   begin
      return Read_U32 (E.Bytes, 0);
   end Event_Type;

   function Window_Sub_Event (E : SDL_Event) return unsigned_char is
   begin
      return E.Bytes (12);
   end Window_Sub_Event;

   --  SDL_WindowEvent: data1 @ offset 16, data2 @ offset 20
   function Window_Data1 (E : SDL_Event) return int is
   begin
      return Read_S32 (E.Bytes, 16);
   end Window_Data1;

   function Window_Data2 (E : SDL_Event) return int is
   begin
      return Read_S32 (E.Bytes, 20);
   end Window_Data2;

   --  SDL_KeyboardEvent: scancode@16 sym@20 mod@24 (2 bytes LE)
   function Key_Sym      (E : SDL_Event) return int is (Read_S32 (E.Bytes, 20));
   function Key_Scancode (E : SDL_Event) return int is (Read_S32 (E.Bytes, 16));
   function Key_Mod      (E : SDL_Event) return unsigned is
     (unsigned (E.Bytes (24)) + unsigned (E.Bytes (25)) * 256);

   function Poll (E : out SDL_Event) return Boolean is
   begin
      return SDL_PollEvent (E) = 1;
   end Poll;


   -- ─── Mouse motion (SDL_MouseMotionEvent) ──────────────────────────────
   -- Offsets: type@0 timestamp@4 windowID@8 which@12 state@16 x@20 y@24
   function Mouse_X (E : SDL_Event) return int is (Read_S32 (E.Bytes, 20));
   function Mouse_Y (E : SDL_Event) return int is (Read_S32 (E.Bytes, 24));

   -- ─── Mouse button (SDL_MouseButtonEvent) ─────────────────────────────
   -- SDL_MouseButtonEvent: type@0 timestamp@4 windowID@8 which@12 button@16 state@17 clicks@18 pad@19 x@20 y@24
   function Mouse_Button (E : SDL_Event) return unsigned_char is (E.Bytes (16));
   function Mouse_Btn_X  (E : SDL_Event) return int is (Read_S32 (E.Bytes, 20));
   function Mouse_Btn_Y  (E : SDL_Event) return int is (Read_S32 (E.Bytes, 24));

   -- ─── Mouse wheel (SDL_MouseWheelEvent) ────────────────────────────────
   -- Offsets: type@0 timestamp@4 windowID@8 which@12 x@16 y@20
   function Mouse_Wheel_Y (E : SDL_Event) return int is (Read_S32 (E.Bytes, 20));

   -- ─── Text input (SDL_TextInputEvent) ─────────────────────────────────
   -- Offsets: type@0 timestamp@4 windowID@8 text@8 (up to 32 UTF-8 bytes)
   function Text_Chars (E : SDL_Event) return String is
      Last : Natural := 8;
   begin
      while Last < 40 and then E.Bytes (Last) /= 0 loop
         Last := Last + 1;
      end loop;
      if Last = 8 then return ""; end if;
      declare
         S : String (1 .. Last - 8);
      begin
         for I in S'Range loop
            S (I) := Character'Val (E.Bytes (7 + I));
         end loop;
         return S;
      end;
   end Text_Chars;

end SDL.Events;
