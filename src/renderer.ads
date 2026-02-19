-- renderer.ads
-- BlackVideo Mini Player - SDL2 Renderer Specification

with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL.Video.Textures;
with Video_Decoder;

package Renderer is

   --  Create streaming RGB texture matching video dimensions
   procedure Init_Texture
     (Rend   : SDL.Video.Renderers.Renderer;
      Tex    : out SDL.Video.Textures.Texture;
      Width  : Integer;
      Height : Integer);

   --  Upload a decoded RGB frame to the texture
   procedure Upload_Frame
     (Tex    : in out SDL.Video.Textures.Texture;
      Frame  : Video_Decoder.RGB_Frame;
      Width  : Integer;
      Height : Integer);

   --  Clear, blit texture, present
   procedure Draw
     (Rend   : SDL.Video.Renderers.Renderer;
      Tex    : SDL.Video.Textures.Texture;
      Width  : Integer;
      Height : Integer);

   --  Handle window resize
   procedure On_Resize
     (Rend      : SDL.Video.Renderers.Renderer;
      New_Width  : Integer;
      New_Height : Integer);

   --  Free texture
   procedure Destroy_Texture (Tex : in out SDL.Video.Textures.Texture);

end Renderer;
