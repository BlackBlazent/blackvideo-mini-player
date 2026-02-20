-- sdl-video-textures.adb

with System;

package body SDL.Video.Textures is

   procedure Create
     (Tex         : out Texture;
      Rend        : SDL.Video.Renderer_Handle;
      Format      : unsigned;
      Access_Kind : int;
      Width       : int;
      Height      : int)
   is
   begin
      Tex := SDL_CreateTexture (Rend, Format, Access_Kind, Width, Height);
      if Tex = SDL.Video.Null_Texture then
         raise Program_Error with "SDL_CreateTexture failed";
      end if;
   end Create;

   procedure Destroy (Tex : in out Texture) is
   begin
      if Tex /= SDL.Video.Null_Texture then
         SDL_DestroyTexture (Tex);
         Tex := SDL.Video.Null_Texture;
      end if;
   end Destroy;

   procedure Lock
     (Tex    : Texture;
      Pixels : out System.Address;
      Pitch  : out int)
   is
      Ret : int := SDL_LockTexture (Tex, System.Null_Address, Pixels, Pitch);
   begin null; end Lock;

   procedure Unlock (Tex : Texture) is
   begin
      SDL_UnlockTexture (Tex);
   end Unlock;

   procedure Update
     (Tex    : Texture;
      Pixels : System.Address;
      Pitch  : int)
   is
      Ret : int := SDL_UpdateTexture (Tex, System.Null_Address, Pixels, Pitch);
   begin null; end Update;

end SDL.Video.Textures;
