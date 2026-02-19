-- bindings/sdl2/sdl-events.ads
-- Ada bindings for SDL2 events
-- SDL_PollEvent, SDL_Event, keyboard, window events

with Interfaces.C;
with System;

package SDL.Events is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  Event type codes (SDL_EventType)
   -- ─────────────────────────────────────────────
   Quit           : constant unsigned := 16#100#;
   Key_Down       : constant unsigned := 16#300#;
   Key_Up         : constant unsigned := 16#301#;
   Mouse_Motion   : constant unsigned := 16#400#;
   Mouse_Button_Down : constant unsigned := 16#401#;
   Window_Event   : constant unsigned := 16#200#;

   --  SDL_WindowEventID
   Window_Size_Changed : constant unsigned_char := 6;
   Window_Resized      : constant unsigned_char := 5;
   Window_Close        : constant unsigned_char := 14;

   -- ─────────────────────────────────────────────
   --  Common event header
   -- ─────────────────────────────────────────────
   type Common_Event is record
      Event_Type : unsigned;
      Timestamp  : unsigned;
   end record with Convention => C;

   -- ─────────────────────────────────────────────
   --  Window event
   -- ─────────────────────────────────────────────
   type Window_Event_Data is record
      Event_Type  : unsigned;
      Timestamp   : unsigned;
      Window_ID   : unsigned;
      Event       : unsigned_char;
      Padding1    : unsigned_char;
      Padding2    : unsigned_char;
      Padding3    : unsigned_char;
      Data1       : int;
      Data2       : int;
   end record with Convention => C;

   -- ─────────────────────────────────────────────
   --  Keyboard
   -- ─────────────────────────────────────────────
   package Keyboards is

      type Scancode is new int;
      type Keycode  is new int;
      type Keymod   is new unsigned_short;

      --  Common keycodes (SDL_Keycode)
      SDLK_Escape    : constant Keycode := 27;
      SDLK_Space     : constant Keycode := 32;
      SDLK_q         : constant Keycode := 113;
      SDLK_m         : constant Keycode := 109;
      SDLK_f         : constant Keycode := 102;
      SDLK_Left      : constant Keycode := 16#4000_0050#;
      SDLK_Right     : constant Keycode := 16#4000_004F#;
      SDLK_Up        : constant Keycode := 16#4000_0052#;
      SDLK_Down      : constant Keycode := 16#4000_0051#;

      type Keysym is record
         Scancode : Keyboards.Scancode;
         Sym      : Keyboards.Keycode;
         Mod_Keys : Keyboards.Keymod;
         Unused   : unsigned;
      end record with Convention => C;

      type Key_Event is record
         Event_Type : unsigned;
         Timestamp  : unsigned;
         Window_ID  : unsigned;
         State      : unsigned_char;
         Repeat     : unsigned_char;
         Padding2   : unsigned_char;
         Padding3   : unsigned_char;
         Key_Sym    : Keysym;
      end record with Convention => C;

   end Keyboards;

   -- ─────────────────────────────────────────────
   --  SDL_Event union (padded to 56 bytes)
   -- ─────────────────────────────────────────────
   type Event_Padding is array (0 .. 55) of unsigned_char;

   type Event (Discriminant : unsigned := 0) is record
      case Discriminant is
         when 16#100# =>
            Common : Common_Event;
         when 16#200# =>
            Window : Window_Event_Data;
         when 16#300# | 16#301# =>
            Key    : Keyboards.Key_Event;
         when others =>
            Raw    : Event_Padding;
      end case;
   end record with Convention => C, Unchecked_Union;

   -- ─────────────────────────────────────────────
   --  Simplified flat event (avoid union issues in Ada)
   -- ─────────────────────────────────────────────
   type Flat_Event is record
      Event_Type  : unsigned;       -- offset 0
      Timestamp   : unsigned;       -- offset 4
      Window_ID   : unsigned;       -- offset 8
      --  Key fields at offset 12
      State       : unsigned_char;
      Repeat      : unsigned_char;
      Padding2    : unsigned_char;
      Padding3    : unsigned_char;
      Scancode    : int;
      Sym         : int;            -- keycode
      Mod_Keys    : unsigned_short;
      Unused      : unsigned;
      --  Window event
      Win_Event   : unsigned_char;  -- at offset 12 (overlaps key)
      Win_Data1   : int;
      Win_Data2   : int;
   end record with Convention => C;
   type Flat_Event_Ptr is access Flat_Event;

   -- ─────────────────────────────────────────────
   --  SDL_PollEvent
   -- ─────────────────────────────────────────────
   function Poll (E : out Flat_Event) return Boolean;

private
   function SDL_PollEvent (E : out Flat_Event) return int
   with Import, Convention => C, External_Name => "SDL_PollEvent";

end SDL.Events;
