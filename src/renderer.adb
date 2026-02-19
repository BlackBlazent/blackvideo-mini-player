-- renderer.adb
-- BlackVideo Mini Player - SDL2 Renderer Implementation
--
-- Renders decoded video frames using SDL2 streaming textures.
-- Maintains aspect ratio (letterbox / pillarbox) when window is resized.

with Ada.Text_IO;
with Interfaces.C;
with SDL.Video.Pixel_Formats;
with SDL.Video.Rectangles;

package body Renderer is

   use Ada.Text_IO;
   use Interfaces.C;

   --  Store native video size for aspect ratio calculation
   Native_W : Integer := 0;
   Native_H : Integer := 0;

   -- ─────────────────────────────────────────────
   --  Init_Texture
   -- ─────────────────────────────────────────────
   procedure Init_Texture
     (Rend   : SDL.Video.Renderers.Renderer;
      Tex    : out SDL.Video.Textures.Texture;
      Width  : Integer;
      Height : Integer)
   is
   begin
      Native_W := Width;
      Native_H := Height;

      SDL.Video.Textures.Create
        (Tex,
         Rend,
         Format => SDL.Video.Pixel_Formats.Pixel_Format_RGB24,
         Access_Kind => SDL.Video.Textures.Streaming,
         Width  => Width,
         Height => Height);

      Put_Line ("[Renderer] Texture created:" & Width'Image & "x" & Height'Image);
   end Init_Texture;

   -- ─────────────────────────────────────────────
   --  Upload_Frame
   -- ─────────────────────────────────────────────
   procedure Upload_Frame
     (Tex    : in out SDL.Video.Textures.Texture;
      Frame  : Video_Decoder.RGB_Frame;
      Width  : Integer;
      Height : Integer)
   is
      Pitch   : Integer := Width * 3;   -- RGB24: 3 bytes per pixel
      Pixels  : System.Address;
      P_Pitch : Integer;
   begin
      if not Frame.Valid or Frame.Data = null then
         return;
      end if;

      --  Lock texture for direct pixel write
      SDL.Video.Textures.Lock (Tex, Pixels, P_Pitch);

      --  Bulk copy RGB data
      declare
         Src : Video_Decoder.Byte_Array renames Frame.Data.all;
         Dst : Video_Decoder.Byte_Array (0 .. Width * Height * 3 - 1)
             with Import, Address => Pixels;
      begin
         for I in 0 .. Width * Height * 3 - 1 loop
            Dst (I) := Src (I);
         end loop;
      end;

      SDL.Video.Textures.Unlock (Tex);
   end Upload_Frame;

   -- ─────────────────────────────────────────────
   --  Compute aspect-ratio-correct destination rect
   -- ─────────────────────────────────────────────
   function Fit_Rect (Window_W, Window_H, Video_W, Video_H : Integer)
      return SDL.Video.Rectangles.Rectangle
   is
      Scale_X : Float := Float (Window_W) / Float (Video_W);
      Scale_Y : Float := Float (Window_H) / Float (Video_H);
      Scale   : Float := Float'Min (Scale_X, Scale_Y);
      DW      : Integer := Integer (Float (Video_W) * Scale);
      DH      : Integer := Integer (Float (Video_H) * Scale);
      DX      : Integer := (Window_W - DW) / 2;
      DY      : Integer := (Window_H - DH) / 2;
   begin
      return SDL.Video.Rectangles.Rectangle'
        (X => DX, Y => DY, Width => DW, Height => DH);
   end Fit_Rect;

   --  Current window dimensions (updated on resize)
   Win_W : Integer := 0;
   Win_H : Integer := 0;

   -- ─────────────────────────────────────────────
   --  Draw
   -- ─────────────────────────────────────────────
   procedure Draw
     (Rend   : SDL.Video.Renderers.Renderer;
      Tex    : SDL.Video.Textures.Texture;
      Width  : Integer;
      Height : Integer)
   is
      Dst : SDL.Video.Rectangles.Rectangle;
   begin
      if Win_W = 0 then
         Win_W := Width;
         Win_H := Height;
      end if;

      --  Black background (letterbox / pillarbox)
      SDL.Video.Renderers.Set_Draw_Color (Rend, 0, 0, 0, 255);
      SDL.Video.Renderers.Clear (Rend);

      --  Compute letterboxed destination
      Dst := Fit_Rect (Win_W, Win_H, Native_W, Native_H);

      --  Blit video texture
      SDL.Video.Renderers.Copy (Rend, Tex, Dst);

      --  Present
      SDL.Video.Renderers.Present (Rend);
   end Draw;

   -- ─────────────────────────────────────────────
   --  On_Resize
   -- ─────────────────────────────────────────────
   procedure On_Resize
     (Rend       : SDL.Video.Renderers.Renderer;
      New_Width  : Integer;
      New_Height : Integer)
   is
   begin
      Win_W := New_Width;
      Win_H := New_Height;
      Put_Line ("[Renderer] Resized to:" & New_Width'Image & "x" & New_Height'Image);
   end On_Resize;

   -- ─────────────────────────────────────────────
   --  Destroy_Texture
   -- ─────────────────────────────────────────────
   procedure Destroy_Texture (Tex : in out SDL.Video.Textures.Texture) is
   begin
      SDL.Video.Textures.Destroy (Tex);
      Put_Line ("[Renderer] Texture destroyed.");
   end Destroy_Texture;

end Renderer;
