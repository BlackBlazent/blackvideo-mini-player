-- sdl-video-renderers.ads
-- Package SDL.Video.Renderers
-- File name rule: sdl-video-renderers.ads → package SDL.Video.Renderers

with Interfaces.C;
with System;

package SDL.Video.Renderers is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  SDL_RendererFlags
   -- ─────────────────────────────────────────────
   SDL_RENDERER_SOFTWARE      : constant unsigned := 16#0000_0001#;
   SDL_RENDERER_ACCELERATED   : constant unsigned := 16#0000_0002#;
   SDL_RENDERER_PRESENTVSYNC  : constant unsigned := 16#0000_0004#;

   --  Aliases
   Accelerated    : constant unsigned := SDL_RENDERER_ACCELERATED;
   Present_V_Sync : constant unsigned := SDL_RENDERER_PRESENTVSYNC;

   -- ─────────────────────────────────────────────
   --  Renderer subtype
   -- ─────────────────────────────────────────────
   subtype Renderer is SDL.Video.Renderer_Handle;

   -- ─────────────────────────────────────────────
   --  C Imports
   -- ─────────────────────────────────────────────

   function SDL_CreateRenderer
     (Win   : SDL.Video.Window_Handle;
      Index : int;
      Flags : unsigned) return Renderer
   with Import, Convention => C, External_Name => "SDL_CreateRenderer";

   procedure SDL_DestroyRenderer (Rend : Renderer)
   with Import, Convention => C, External_Name => "SDL_DestroyRenderer";

   function SDL_RenderClear (Rend : Renderer) return int
   with Import, Convention => C, External_Name => "SDL_RenderClear";

   procedure SDL_RenderPresent (Rend : Renderer)
   with Import, Convention => C, External_Name => "SDL_RenderPresent";

   function SDL_SetRenderDrawColor
     (Rend    : Renderer;
      R, G, B, A : unsigned_char) return int
   with Import, Convention => C, External_Name => "SDL_SetRenderDrawColor";

   function SDL_RenderCopy
     (Rend : Renderer;
      Tex  : SDL.Video.Texture_Handle;
      Src  : System.Address;          -- null = full source
      Dst  : access SDL.Video.SDL_Rect) return int
   with Import, Convention => C, External_Name => "SDL_RenderCopy";

   -- ─────────────────────────────────────────────
   --  Ada-friendly wrappers
   -- ─────────────────────────────────────────────

   procedure Create
     (Rend  : out Renderer;
      Win   : SDL.Video.Window_Handle;
      Index : int      := -1;
      Flags : unsigned := SDL_RENDERER_ACCELERATED);

   procedure Destroy (Rend : in out Renderer);

   procedure Clear (Rend : Renderer);
   procedure Present (Rend : Renderer);

   procedure Set_Draw_Color
     (Rend       : Renderer;
      R, G, B, A : unsigned_char);

   --  Copy texture to renderer with destination rect
   procedure Copy
     (Rend : Renderer;
      Tex  : SDL.Video.Texture_Handle;
      Dst  : SDL.Video.SDL_Rect);

   --  Copy full texture (stretch to viewport)
   procedure Copy_Full
     (Rend : Renderer;
      Tex  : SDL.Video.Texture_Handle);

end SDL.Video.Renderers;
