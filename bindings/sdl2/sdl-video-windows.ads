-- sdl-video-windows.ads
-- Package SDL.Video.Windows — window creation / destruction
-- File name rule: package SDL.Video.Windows → sdl-video-windows.ads

with Interfaces.C;
with Interfaces.C.Strings;

package SDL.Video.Windows is

   use Interfaces.C;
   use Interfaces.C.Strings;

   -- ─────────────────────────────────────────────
   --  SDL_WindowFlags
   -- ─────────────────────────────────────────────
   SDL_WINDOW_FULLSCREEN    : constant unsigned := 16#0000_0001#;
   SDL_WINDOW_SHOWN         : constant unsigned := 16#0000_0004#;
   SDL_WINDOW_BORDERLESS    : constant unsigned := 16#0000_0010#;
   SDL_WINDOW_RESIZABLE     : constant unsigned := 16#0000_0020#;
   SDL_WINDOW_MAXIMIZED     : constant unsigned := 16#0000_0080#;

   --  Convenience aliases (match names used in player.adb)
   Resizable : constant unsigned := SDL_WINDOW_RESIZABLE;
   Fullscreen : constant unsigned := SDL_WINDOW_FULLSCREEN;

   --  SDL_WINDOWPOS_CENTERED = 0x2FFF0000
   --  SDL2 passes this as Sint32 but the bit pattern > Integer'Last.
   --  We keep it as unsigned and explicitly cast when calling SDL_CreateWindow.
   SDL_WINDOWPOS_CENTERED : constant unsigned := 16#2FFF_0000#;
   SDL_WINDOWPOS_UNDEFINED : constant unsigned := 16#1FFF_0000#;

   -- ─────────────────────────────────────────────
   --  Window subtype (alias of SDL.Video.Window_Handle)
   -- ─────────────────────────────────────────────
   subtype Window is SDL.Video.Window_Handle;

   -- ─────────────────────────────────────────────
   --  C Imports
   -- ─────────────────────────────────────────────

   function SDL_CreateWindow
     (Title  : chars_ptr;
      X, Y   : unsigned;   -- accepts SDL_WINDOWPOS_CENTERED (unsigned bit pattern)
      W, H   : int;
      Flags  : unsigned) return Window
   with Import, Convention => C, External_Name => "SDL_CreateWindow";

   procedure SDL_DestroyWindow (Win : Window)
   with Import, Convention => C, External_Name => "SDL_DestroyWindow";

   function SDL_SetWindowFullscreen
     (Win : Window; Flags : unsigned) return int
   with Import, Convention => C, External_Name => "SDL_SetWindowFullscreen";

   procedure SDL_GetWindowSize (Win : Window; W, H : out int)
   with Import, Convention => C, External_Name => "SDL_GetWindowSize";

   -- ─────────────────────────────────────────────
   --  Ada-friendly wrappers
   -- ─────────────────────────────────────────────

   procedure Create
     (Win    : out Window;
      Title  : String;
      X, Y   : unsigned;   -- use SDL_WINDOWPOS_CENTERED or SDL_WINDOWPOS_UNDEFINED
      Width  : int;
      Height : int;
      Flags  : unsigned);

   procedure Destroy (Win : in out Window);

   procedure Set_Fullscreen (Win : Window; Enable : Boolean);

   procedure Get_Size (Win : Window; W, H : out int);

end SDL.Video.Windows;
