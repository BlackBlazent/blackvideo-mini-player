-- renderer.ads
-- BlackVideo Mini Player - SDL2 Renderer specification

with SDL.Video;
with System;
with Video_Decoder;

package Renderer is

   procedure Init_Texture
     (Rend   : SDL.Video.Renderer_Handle;
      Tex    : out SDL.Video.Texture_Handle;
      Width  : Integer;
      Height : Integer);

   procedure Upload_Frame
     (Tex    : SDL.Video.Texture_Handle;
      Frame  : Video_Decoder.RGB_Frame;
      Width  : Integer;
      Height : Integer);

   -- Draw clears + copies texture but does NOT call SDL_RenderPresent.
   -- Call UI_Overlay.Draw after this, then Renderer.Present.
   procedure Draw
     (Rend   : SDL.Video.Renderer_Handle;
      Tex    : SDL.Video.Texture_Handle;
      Width  : Integer;
      Height : Integer);

   -- Present the finished frame (video + overlay) to the screen.
   procedure Present (Rend : SDL.Video.Renderer_Handle);

   procedure On_Resize (New_Width : Integer; New_Height : Integer);

   procedure Destroy_Texture (Tex : in out SDL.Video.Texture_Handle);

   -- Renderer address as System.Address (for C overlay calls)
   function To_Address (Rend : SDL.Video.Renderer_Handle)
     return System.Address;

end Renderer;
