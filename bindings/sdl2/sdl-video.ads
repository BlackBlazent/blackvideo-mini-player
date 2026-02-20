-- sdl-video.ads
-- Package SDL.Video — shared types used by child packages
-- File name rule: package SDL.Video → sdl-video.ads

with Interfaces.C;
with System;

package SDL.Video is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  SDL_Rect  (used by renderers + textures)
   -- ─────────────────────────────────────────────
   type SDL_Rect is record
      X, Y, W, H : int;
   end record with Convention => C;

   type SDL_Rect_Ptr is access SDL_Rect;

   -- ─────────────────────────────────────────────
   --  Pixel format constants
   -- ─────────────────────────────────────────────
   SDL_PIXELFORMAT_RGB24  : constant unsigned := 16#17101803#;
   SDL_PIXELFORMAT_RGBA32 : constant unsigned := 16#16462004#;
   SDL_PIXELFORMAT_YV12   : constant unsigned := 16#32315659#;
   SDL_PIXELFORMAT_IYUV   : constant unsigned := 16#56555949#;

   -- ─────────────────────────────────────────────
   --  Texture access
   -- ─────────────────────────────────────────────
   SDL_TEXTUREACCESS_STATIC    : constant int := 0;
   SDL_TEXTUREACCESS_STREAMING : constant int := 1;
   SDL_TEXTUREACCESS_TARGET    : constant int := 2;

   -- ─────────────────────────────────────────────
   --  Opaque handle types (thin wrappers over System.Address)
   --  Defined here so child packages share the same types.
   -- ─────────────────────────────────────────────
   type Window_Handle   is new System.Address;
   type Renderer_Handle is new System.Address;
   type Texture_Handle  is new System.Address;

   Null_Window   : constant Window_Handle   := Window_Handle   (System.Null_Address);
   Null_Renderer : constant Renderer_Handle := Renderer_Handle (System.Null_Address);
   Null_Texture  : constant Texture_Handle  := Texture_Handle  (System.Null_Address);

end SDL.Video;
