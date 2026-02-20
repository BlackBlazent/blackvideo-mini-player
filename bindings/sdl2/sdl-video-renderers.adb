-- sdl-video-renderers.adb

with System;

package body SDL.Video.Renderers is

   procedure Create
     (Rend  : out Renderer;
      Win   : SDL.Video.Window_Handle;
      Index : int      := -1;
      Flags : unsigned := SDL_RENDERER_ACCELERATED)
   is
   begin
      Rend := SDL_CreateRenderer (Win, Index, Flags);
      if Rend = SDL.Video.Null_Renderer then
         raise Program_Error with "SDL_CreateRenderer failed";
      end if;
   end Create;

   procedure Destroy (Rend : in out Renderer) is
   begin
      if Rend /= SDL.Video.Null_Renderer then
         SDL_DestroyRenderer (Rend);
         Rend := SDL.Video.Null_Renderer;
      end if;
   end Destroy;

   procedure Clear (Rend : Renderer) is
      Discard : int;
      pragma Unreferenced (Discard);
   begin
      Discard := SDL_RenderClear (Rend);
   end Clear;

   procedure Present (Rend : Renderer) is
   begin
      SDL_RenderPresent (Rend);
   end Present;

   procedure Set_Draw_Color
     (Rend       : Renderer;
      R, G, B, A : unsigned_char)
   is
      Discard : int;
      pragma Unreferenced (Discard);
   begin
      Discard := SDL_SetRenderDrawColor (Rend, R, G, B, A);
   end Set_Draw_Color;

   procedure Copy
     (Rend : Renderer;
      Tex  : SDL.Video.Texture_Handle;
      Dst  : SDL.Video.SDL_Rect)
   is
      D       : aliased SDL.Video.SDL_Rect := Dst;
      Discard : int;
      pragma Unreferenced (Discard);
   begin
      Discard := SDL_RenderCopy (Rend, Tex, System.Null_Address, D'Access);
   end Copy;

   procedure Copy_Full
     (Rend : Renderer;
      Tex  : SDL.Video.Texture_Handle)
   is
      Discard : int;
      pragma Unreferenced (Discard);
   begin
      Discard := SDL_RenderCopy (Rend, Tex, System.Null_Address, null);
   end Copy_Full;

end SDL.Video.Renderers;
