-- renderer.ads
-- BlackVideo Mini Player - SDL2 Renderer specification
-- Only 'with' what is needed directly in the spec (parameter types).
-- SDL.Video.Renderers and SDL.Video.Textures are used only in the body.

with SDL.Video;
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

   procedure Draw
     (Rend   : SDL.Video.Renderer_Handle;
      Tex    : SDL.Video.Texture_Handle;
      Width  : Integer;
      Height : Integer);

   procedure On_Resize (New_Width : Integer; New_Height : Integer);

   procedure Destroy_Texture (Tex : in out SDL.Video.Texture_Handle);

end Renderer;
