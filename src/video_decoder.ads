-- video_decoder.ads
-- BlackVideo Mini Player - FFmpeg Video Decoder Specification
-- Handles: format open, codec setup, packet reading, frame decode, YUV→RGB

with Interfaces.C;
with System;

package Video_Decoder is

   -- ─────────────────────────────────────────────
   --  RGB Frame (converted from YUV by swscale)
   -- ─────────────────────────────────────────────
   type RGB_Pixel is record
      R, G, B : Interfaces.C.unsigned_char;
   end record with Convention => C;

   type RGB_Row    is array (Natural range <>) of RGB_Pixel;
   type RGB_Buffer is array (Natural range <>, Natural range <>) of RGB_Pixel;

   --  Flat byte buffer passed to SDL texture
   type Byte_Array is array (Natural range <>) of Interfaces.C.unsigned_char;
   type Byte_Array_Ptr is access Byte_Array;

   type RGB_Frame is record
      Data   : Byte_Array_Ptr;   -- Width * Height * 3 bytes (RGB24)
      Width  : Integer;
      Height : Integer;
      Valid  : Boolean := False;
   end record;

   -- ─────────────────────────────────────────────
   --  Lifecycle
   -- ─────────────────────────────────────────────

   --  Open a video file; returns video dimensions and frame delay (ms)
   procedure Open
     (File_Path       : String;
      Width           : out Integer;
      Height          : out Integer;
      Frame_Delay_MS  : out Integer);

   --  Close and free all FFmpeg resources
   procedure Close;

   -- ─────────────────────────────────────────────
   --  Decode Control
   -- ─────────────────────────────────────────────
   procedure Start_Decoding;
   procedure Pause;
   procedure Resume;

   --  Seek by Delta_Seconds (positive = forward, negative = backward)
   procedure Seek (Delta_Seconds : Float);

   -- ─────────────────────────────────────────────
   --  Frame Access
   -- ─────────────────────────────────────────────

   --  Non-blocking: returns next available frame, or Got=False
   procedure Next_Frame (Frame : out RGB_Frame; Got : out Boolean);

   --  True when the stream has no more packets
   function Is_EOF return Boolean;

end Video_Decoder;
