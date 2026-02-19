-- bindings/sdl2/sdl-video.ads
-- Ada bindings for SDL2 video subsystem
-- Windows, Renderers, Textures, Rectangles, Pixel Formats

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package SDL.Video is

   use Interfaces.C;
   use Interfaces.C.Strings;

   -- ═══════════════════════════════════════════════
   --  Rectangles
   -- ═══════════════════════════════════════════════
   package Rectangles is
      type Rectangle is record
         X, Y          : int;
         Width, Height : int;
      end record with Convention => C;
   end Rectangles;

   -- ═══════════════════════════════════════════════
   --  Pixel Formats
   -- ═══════════════════════════════════════════════
   package Pixel_Formats is
      --  SDL_PIXELFORMAT values
      Pixel_Format_RGB24   : constant unsigned := 16#17101803#;
      Pixel_Format_RGBA32  : constant unsigned := 16#16462004#;
      Pixel_Format_YV12    : constant unsigned := 16#32315659#;
      Pixel_Format_IYUV    : constant unsigned := 16#56555949#;
   end Pixel_Formats;

   -- ═══════════════════════════════════════════════
   --  Windows
   -- ═══════════════════════════════════════════════
   package Windows is

      Centered_Window : constant int := 16#2FFF0000#;

      --  SDL_WindowFlags
      Resizable    : constant unsigned := 16#00000020#;
      Fullscreen   : constant unsigned := 16#00000001#;
      Borderless   : constant unsigned := 16#00000010#;
      Shown        : constant unsigned := 16#00000004#;

      type Window is new System.Address;

      procedure Create
        (Win    : out Window;
         Title  : String;
         X, Y   : int;
         Width  : int;
         Height : int;
         Flags  : unsigned)
      with Inline;

      procedure Destroy (Win : in out Window);

      procedure Set_Fullscreen (Win : in out Window; Enable : Boolean);

      procedure Get_Size (Win : Window; W, H : out int);

   end Windows;

   -- ═══════════════════════════════════════════════
   --  Renderers
   -- ═══════════════════════════════════════════════
   package Renderers is

      --  SDL_RendererFlags
      Accelerated    : constant unsigned := 16#00000002#;
      Present_V_Sync : constant unsigned := 16#00000004#;
      Software       : constant unsigned := 16#00000001#;

      type Renderer is new System.Address;

      procedure Create
        (Rend  : out Renderer;
         Win   : Windows.Window;
         Index : int    := -1;
         Flags : unsigned := Accelerated);

      procedure Destroy (Rend : in out Renderer);

      procedure Clear (Rend : Renderer);
      procedure Present (Rend : Renderer);

      procedure Set_Draw_Color
        (Rend : Renderer;
         R, G, B, A : unsigned_char);

      procedure Copy
        (Rend : Renderer;
         Tex  : System.Address;   -- Texture
         Dst  : Rectangles.Rectangle);

      procedure Copy_Full
        (Rend : Renderer;
         Tex  : System.Address);

   end Renderers;

   -- ═══════════════════════════════════════════════
   --  Textures
   -- ═══════════════════════════════════════════════
   package Textures is

      type Access_Kind is (Static, Streaming, Target);
      for Access_Kind use (Static => 0, Streaming => 1, Target => 2);

      type Texture is new System.Address;

      procedure Create
        (Tex         : out Texture;
         Rend        : Renderers.Renderer;
         Format      : unsigned;
         Access_Kind : Textures.Access_Kind;
         Width       : int;
         Height      : int);

      procedure Destroy (Tex : in out Texture);

      procedure Lock
        (Tex    : Texture;
         Pixels : out System.Address;
         Pitch  : out int);

      procedure Unlock (Tex : Texture);

      procedure Update
        (Tex    : Texture;
         Pixels : System.Address;
         Pitch  : int);

   end Textures;

end SDL.Video;
