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

   --  SDL_KeyboardEvent: scancode @ 16, sym @ 20
   function Key_Sym      (E : SDL_Event) return int is (Read_S32 (E.Bytes, 20));
   function Key_Scancode (E : SDL_Event) return int is (Read_S32 (E.Bytes, 16));

   function Poll (E : out SDL_Event) return Boolean is
   begin
      return SDL_PollEvent (E) = 1;
   end Poll;

end SDL.Events;
