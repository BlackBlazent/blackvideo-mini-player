-- renderer.adb
-- BlackVideo Mini Player - SDL2 letterbox rendering

with Ada.Text_IO;
with Interfaces.C;
with SDL.Video;
with SDL.Video.Renderers;
with SDL.Video.Textures;
with Video_Decoder;

package body Renderer is

   use Ada.Text_IO;
   use Interfaces.C;
   use Video_Decoder;   -- makes Byte_Array_Access indexing visible (fixes error 59)

   Native_W : Integer := 0;
   Native_H : Integer := 0;
   Win_W    : Integer := 0;
   Win_H    : Integer := 0;

   -- ─────────────────────────────────────────────
   --  Init_Texture
   -- ─────────────────────────────────────────────
   procedure Init_Texture
     (Rend   : SDL.Video.Renderer_Handle;
      Tex    : out SDL.Video.Texture_Handle;
      Width  : Integer;
      Height : Integer)
   is
   begin
      Native_W := Width;
      Native_H := Height;
      Win_W    := Width;
      Win_H    := Height;

      SDL.Video.Textures.Create
        (Tex,
         Rend,
         Format      => SDL.Video.SDL_PIXELFORMAT_RGB24,
         Access_Kind => SDL.Video.SDL_TEXTUREACCESS_STREAMING,
         Width       => int (Width),
         Height      => int (Height));

      Put_Line ("[Renderer] Texture "
                & Integer'Image (Width) & "x" & Integer'Image (Height));
   end Init_Texture;

   -- ─────────────────────────────────────────────
   --  Upload_Frame
   --  Frame.Data is Byte_Array_Access; indexing (0) requires
   --  Video_Decoder to be use-visible so the array type is known.
   -- ─────────────────────────────────────────────
   procedure Upload_Frame
     (Tex    : SDL.Video.Texture_Handle;
      Frame  : Video_Decoder.RGB_Frame;
      Width  : Integer;
      Height : Integer)
   is
      pragma Unreferenced (Height);
      Pitch : constant int := int (Width) * 3;   -- RGB24 = 3 bytes/pixel
   begin
      if not Frame.Valid or else Frame.Data = null then
         return;
      end if;
      SDL.Video.Textures.Update
        (Tex,
         Pixels => Frame.Data (0)'Address,   -- requires 'use Video_Decoder'
         Pitch  => Pitch);
   end Upload_Frame;

   -- ─────────────────────────────────────────────
   --  Fit_Rect — letterbox/pillarbox aspect-ratio rect
   -- ─────────────────────────────────────────────
   function Fit_Rect return SDL.Video.SDL_Rect is
      Scale_X : constant Float := Float (Win_W) / Float (Native_W);
      Scale_Y : constant Float := Float (Win_H) / Float (Native_H);
      Scale   : constant Float := Float'Min (Scale_X, Scale_Y);
      DW      : constant Integer := Integer (Float (Native_W) * Scale);
      DH      : constant Integer := Integer (Float (Native_H) * Scale);
      DX      : constant Integer := (Win_W - DW) / 2;
      DY      : constant Integer := (Win_H - DH) / 2;
   begin
      return SDL.Video.SDL_Rect'
        (X => int (DX), Y => int (DY), W => int (DW), H => int (DH));
   end Fit_Rect;

   -- ─────────────────────────────────────────────
   --  Draw
   -- ─────────────────────────────────────────────
   procedure Draw
     (Rend   : SDL.Video.Renderer_Handle;
      Tex    : SDL.Video.Texture_Handle;
      Width  : Integer;
      Height : Integer)
   is
      pragma Unreferenced (Width, Height);
      Dst : constant SDL.Video.SDL_Rect := Fit_Rect;
   begin
      SDL.Video.Renderers.Set_Draw_Color (Rend, 0, 0, 0, 255);
      SDL.Video.Renderers.Clear (Rend);
      SDL.Video.Renderers.Copy (Rend, Tex, Dst);
      SDL.Video.Renderers.Present (Rend);
   end Draw;

   -- ─────────────────────────────────────────────
   --  On_Resize
   -- ─────────────────────────────────────────────
   procedure On_Resize (New_Width : Integer; New_Height : Integer) is
   begin
      Win_W := New_Width;
      Win_H := New_Height;
   end On_Resize;

   -- ─────────────────────────────────────────────
   --  Destroy_Texture
   -- ─────────────────────────────────────────────
   procedure Destroy_Texture (Tex : in out SDL.Video.Texture_Handle) is
   begin
      SDL.Video.Textures.Destroy (Tex);
   end Destroy_Texture;

end Renderer;
