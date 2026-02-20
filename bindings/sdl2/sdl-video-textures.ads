-- sdl-video-textures.ads
-- Package SDL.Video.Textures
-- File name: sdl-video-textures.ads → package SDL.Video.Textures

with Interfaces.C;
with System;

package SDL.Video.Textures is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  Texture subtype
   -- ─────────────────────────────────────────────
   subtype Texture is SDL.Video.Texture_Handle;

   -- ─────────────────────────────────────────────
   --  C Imports
   -- ─────────────────────────────────────────────

   function SDL_CreateTexture
     (Rend        : SDL.Video.Renderer_Handle;
      Format      : unsigned;
      Access_Kind : int;
      W, H        : int) return Texture
   with Import, Convention => C, External_Name => "SDL_CreateTexture";

   procedure SDL_DestroyTexture (Tex : Texture)
   with Import, Convention => C, External_Name => "SDL_DestroyTexture";

   function SDL_LockTexture
     (Tex    : Texture;
      Rect   : System.Address;   -- null = whole texture
      Pixels : out System.Address;
      Pitch  : out int) return int
   with Import, Convention => C, External_Name => "SDL_LockTexture";

   procedure SDL_UnlockTexture (Tex : Texture)
   with Import, Convention => C, External_Name => "SDL_UnlockTexture";

   function SDL_UpdateTexture
     (Tex    : Texture;
      Rect   : System.Address;   -- null = whole texture
      Pixels : System.Address;
      Pitch  : int) return int
   with Import, Convention => C, External_Name => "SDL_UpdateTexture";

   -- ─────────────────────────────────────────────
   --  Ada-friendly wrappers
   -- ─────────────────────────────────────────────

   procedure Create
     (Tex         : out Texture;
      Rend        : SDL.Video.Renderer_Handle;
      Format      : unsigned;
      Access_Kind : int;
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

end SDL.Video.Textures;
