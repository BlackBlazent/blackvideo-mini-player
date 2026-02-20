-- sdl-events-keyboards.ads
-- Package SDL.Events.Keyboards — SDL keycode constants

with Interfaces.C;

package SDL.Events.Keyboards is

   use Interfaces.C;

   subtype SDL_Keycode is int;

   --  Printable keys
   SDLK_SPACE     : constant SDL_Keycode := 32;
   SDLK_a         : constant SDL_Keycode := 97;
   SDLK_f         : constant SDL_Keycode := 102;
   SDLK_m         : constant SDL_Keycode := 109;
   SDLK_q         : constant SDL_Keycode := 113;

   --  Control keys
   SDLK_ESCAPE    : constant SDL_Keycode := 27;
   SDLK_RETURN    : constant SDL_Keycode := 13;
   SDLK_TAB       : constant SDL_Keycode := 9;
   SDLK_BACKSPACE : constant SDL_Keycode := 8;

   --  Arrow keys (SDLK_SCANCODE_MASK | scancode)
   SDLK_RIGHT : constant SDL_Keycode := 16#4000_004F#;
   SDLK_LEFT  : constant SDL_Keycode := 16#4000_0050#;
   SDLK_DOWN  : constant SDL_Keycode := 16#4000_0051#;
   SDLK_UP    : constant SDL_Keycode := 16#4000_0052#;

   SDLK_F1    : constant SDL_Keycode := 16#4000_003A#;
   SDLK_F11   : constant SDL_Keycode := 16#4000_0044#;
   SDLK_F12   : constant SDL_Keycode := 16#4000_0045#;

end SDL.Events.Keyboards;
