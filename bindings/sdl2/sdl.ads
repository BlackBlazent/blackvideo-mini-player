-- sdl.ads
-- Root SDL2 package: Init, Quit, Delay, GetTicks
-- Ada file name convention: sdl.ads → package SDL

with Interfaces.C;
with Interfaces.C.Strings;

package SDL is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  SDL_Init flags
   -- ─────────────────────────────────────────────
   package Flags is
      Enable_Timer      : constant unsigned := 16#0001#;
      Enable_Audio      : constant unsigned := 16#0010#;
      Enable_Video      : constant unsigned := 16#0020#;
      Enable_Joystick   : constant unsigned := 16#0200#;
      Enable_Events     : constant unsigned := 16#4000#;
      Enable_Everything : constant unsigned := 16#FFFF#;
   end Flags;

   -- ─────────────────────────────────────────────
   --  Core API
   -- ─────────────────────────────────────────────

   function Initialize (Init_Flags : unsigned) return int
   with Import, Convention => C, External_Name => "SDL_Init";

   procedure Quit
   with Import, Convention => C, External_Name => "SDL_Quit";

   --  Delay in milliseconds
   procedure Delay_MS (MS : unsigned)
   with Import, Convention => C, External_Name => "SDL_Delay";

   function Get_Ticks return unsigned
   with Import, Convention => C, External_Name => "SDL_GetTicks";

   function Get_Error return Interfaces.C.Strings.chars_ptr
   with Import, Convention => C, External_Name => "SDL_GetError";

end SDL;
