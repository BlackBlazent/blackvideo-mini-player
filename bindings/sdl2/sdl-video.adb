-- sdl-video.adb
-- SDL2 bindings implementation (C extern calls)

with Interfaces.C.Strings;
with System;

package body SDL.Video is

   use Interfaces.C;
   use Interfaces.C.Strings;

   -- ─────────────────────────────────────────────
   --  C Imports
   -- ─────────────────────────────────────────────

   function SDL_CreateWindow
     (Title  : chars_ptr;
      X, Y   : int;
      W, H   : int;
      Flags  : unsigned) return System.Address
   with Import, Convention => C, External_Name => "SDL_CreateWindow";

   procedure SDL_DestroyWindow (Win : System.Address)
   with Import, Convention => C, External_Name => "SDL_DestroyWindow";

   function SDL_SetWindowFullscreen (Win : System.Address; Flags : unsigned) return int
   with Import, Convention => C, External_Name => "SDL_SetWindowFullscreen";

   procedure SDL_GetWindowSize (Win : System.Address; W, H : out int)
   with Import, Convention => C, External_Name => "SDL_GetWindowSize";

   function SDL_CreateRenderer (Win : System.Address; Index : int; Flags : unsigned)
      return System.Address
   with Import, Convention => C, External_Name => "SDL_CreateRenderer";

   procedure SDL_DestroyRenderer (Rend : System.Address)
   with Import, Convention => C, External_Name => "SDL_DestroyRenderer";

   function SDL_RenderClear (Rend : System.Address) return int
   with Import, Convention => C, External_Name => "SDL_RenderClear";

   procedure SDL_RenderPresent (Rend : System.Address)
   with Import, Convention => C, External_Name => "SDL_RenderPresent";

   function SDL_SetRenderDrawColor (Rend : System.Address; R, G, B, A : unsigned_char)
      return int
   with Import, Convention => C, External_Name => "SDL_SetRenderDrawColor";

   function SDL_RenderCopy
     (Rend    : System.Address;
      Tex     : System.Address;
      Src_Rec : System.Address;
      Dst_Rec : access Rectangles.Rectangle) return int
   with Import, Convention => C, External_Name => "SDL_RenderCopy";

   function SDL_CreateTexture
     (Rend        : System.Address;
      Format      : unsigned;
      Access_Kind : int;
      W, H        : int) return System.Address
   with Import, Convention => C, External_Name => "SDL_CreateTexture";

   procedure SDL_DestroyTexture (Tex : System.Address)
   with Import, Convention => C, External_Name => "SDL_DestroyTexture";

   function SDL_LockTexture
     (Tex    : System.Address;
      Rect   : System.Address;
      Pixels : out System.Address;
      Pitch  : out int) return int
   with Import, Convention => C, External_Name => "SDL_LockTexture";

   procedure SDL_UnlockTexture (Tex : System.Address)
   with Import, Convention => C, External_Name => "SDL_UnlockTexture";

   function SDL_UpdateTexture
     (Tex    : System.Address;
      Rect   : System.Address;
      Pixels : System.Address;
      Pitch  : int) return int
   with Import, Convention => C, External_Name => "SDL_UpdateTexture";

   -- ─────────────────────────────────────────────
   --  Windows
   -- ─────────────────────────────────────────────
   package body Windows is

      procedure Create
        (Win    : out Window;
         Title  : String;
         X, Y   : int;
         Width  : int;
         Height : int;
         Flags  : unsigned)
      is
         C_Title : chars_ptr := New_String (Title);
      begin
         Win := Window (SDL_CreateWindow (C_Title, X, Y, Width, Height, Flags));
         Free (C_Title);
         if System.Address (Win) = System.Null_Address then
            raise Program_Error with "SDL_CreateWindow failed";
         end if;
      end Create;

      procedure Destroy (Win : in out Window) is
      begin
         SDL_DestroyWindow (System.Address (Win));
         Win := Window (System.Null_Address);
      end Destroy;

      procedure Set_Fullscreen (Win : in out Window; Enable : Boolean) is
         Flag : unsigned := (if Enable then Fullscreen else 0);
         Ret  : int;
      begin
         Ret := SDL_SetWindowFullscreen (System.Address (Win), Flag);
      end Set_Fullscreen;

      procedure Get_Size (Win : Window; W, H : out int) is
      begin
         SDL_GetWindowSize (System.Address (Win), W, H);
      end Get_Size;

   end Windows;

   -- ─────────────────────────────────────────────
   --  Renderers
   -- ─────────────────────────────────────────────
   package body Renderers is

      procedure Create
        (Rend  : out Renderer;
         Win   : Windows.Window;
         Index : int    := -1;
         Flags : unsigned := Accelerated)
      is
      begin
         Rend := Renderer (SDL_CreateRenderer (System.Address (Win), Index, Flags));
         if System.Address (Rend) = System.Null_Address then
            raise Program_Error with "SDL_CreateRenderer failed";
         end if;
      end Create;

      procedure Destroy (Rend : in out Renderer) is
      begin
         SDL_DestroyRenderer (System.Address (Rend));
         Rend := Renderer (System.Null_Address);
      end Destroy;

      procedure Clear (Rend : Renderer) is
         Ret : int := SDL_RenderClear (System.Address (Rend));
      begin null; end Clear;

      procedure Present (Rend : Renderer) is
      begin
         SDL_RenderPresent (System.Address (Rend));
      end Present;

      procedure Set_Draw_Color
        (Rend : Renderer;
         R, G, B, A : unsigned_char)
      is
         Ret : int := SDL_SetRenderDrawColor (System.Address (Rend), R, G, B, A);
      begin null; end Set_Draw_Color;

      procedure Copy
        (Rend : Renderer;
         Tex  : System.Address;
         Dst  : Rectangles.Rectangle)
      is
         D   : aliased Rectangles.Rectangle := Dst;
         Ret : int := SDL_RenderCopy
           (System.Address (Rend), Tex, System.Null_Address, D'Access);
      begin null; end Copy;

      procedure Copy_Full (Rend : Renderer; Tex : System.Address) is
         Ret : int := SDL_RenderCopy
           (System.Address (Rend), Tex,
            System.Null_Address, null);
      begin null; end Copy_Full;

   end Renderers;

   -- ─────────────────────────────────────────────
   --  Textures
   -- ─────────────────────────────────────────────
   package body Textures is

      procedure Create
        (Tex         : out Texture;
         Rend        : Renderers.Renderer;
         Format      : unsigned;
         Access_Kind : Textures.Access_Kind;
         Width       : int;
         Height      : int)
      is
      begin
         Tex := Texture (SDL_CreateTexture
           (System.Address (Rend),
            Format,
            Textures.Access_Kind'Pos (Access_Kind),
            Width, Height));
         if System.Address (Tex) = System.Null_Address then
            raise Program_Error with "SDL_CreateTexture failed";
         end if;
      end Create;

      procedure Destroy (Tex : in out Texture) is
      begin
         SDL_DestroyTexture (System.Address (Tex));
         Tex := Texture (System.Null_Address);
      end Destroy;

      procedure Lock
        (Tex    : Texture;
         Pixels : out System.Address;
         Pitch  : out int)
      is
         Ret : int := SDL_LockTexture
           (System.Address (Tex), System.Null_Address, Pixels, Pitch);
      begin null; end Lock;

      procedure Unlock (Tex : Texture) is
      begin
         SDL_UnlockTexture (System.Address (Tex));
      end Unlock;

      procedure Update
        (Tex    : Texture;
         Pixels : System.Address;
         Pitch  : int)
      is
         Ret : int := SDL_UpdateTexture
           (System.Address (Tex), System.Null_Address, Pixels, Pitch);
      begin null; end Update;

   end Textures;

end SDL.Video;
