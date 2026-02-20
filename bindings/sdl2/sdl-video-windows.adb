-- sdl-video-windows.adb

package body SDL.Video.Windows is

   --  Interfaces.C.Strings is already use-visible from the spec (sdl-video-windows.ads)

   procedure Create
     (Win    : out Window;
      Title  : String;
      X, Y   : unsigned;
      Width  : int;
      Height : int;
      Flags  : unsigned)
   is
      use Interfaces.C.Strings;
      C_Title : chars_ptr := New_String (Title);
   begin
      Win := SDL_CreateWindow (C_Title, X, Y, Width, Height, Flags);
      Free (C_Title);
      if Win = SDL.Video.Null_Window then
         raise Program_Error with "SDL_CreateWindow failed";
      end if;
   end Create;

   procedure Destroy (Win : in out Window) is
   begin
      if Win /= SDL.Video.Null_Window then
         SDL_DestroyWindow (Win);
         Win := SDL.Video.Null_Window;
      end if;
   end Destroy;

   procedure Set_Fullscreen (Win : Window; Enable : Boolean) is
      Flag    : constant unsigned :=
                  (if Enable then SDL_WINDOW_FULLSCREEN else 0);
      Discard : int;
      pragma Unreferenced (Discard);
   begin
      Discard := SDL_SetWindowFullscreen (Win, Flag);
   end Set_Fullscreen;

   procedure Get_Size (Win : Window; W, H : out int) is
   begin
      SDL_GetWindowSize (Win, W, H);
   end Get_Size;

end SDL.Video.Windows;
