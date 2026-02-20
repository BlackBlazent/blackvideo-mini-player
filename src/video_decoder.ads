-- video_decoder.ads
-- BlackVideo Mini Player - Video Decoder Specification

with Interfaces.C;

package Video_Decoder is

   use Interfaces.C;

   -- ─── RGB frame (decoded + colour-converted) ───────────────────────────
   type Byte_Array        is array (Natural range <>) of unsigned_char;
   type Byte_Array_Access is access Byte_Array;

   type RGB_Frame is record
      Data   : Byte_Array_Access := null;   -- Width * Height * 3 (RGB24)
      Width  : Integer           := 0;
      Height : Integer           := 0;
      Valid  : Boolean           := False;
   end record;

   -- ─── Lifecycle ────────────────────────────────────────────────────────
   procedure Open
     (File_Path      : String;
      Width          : out Integer;
      Height         : out Integer;
      Frame_Delay_MS : out Integer);

   procedure Close;

   -- ─── Decode control ───────────────────────────────────────────────────
   procedure Start_Decoding;
   procedure Pause;
   procedure Resume;
   procedure Seek (Delta_Seconds : Float);

   -- ─── Frame access (called from main loop) ────────────────────────────
   procedure Next_Frame (Frame : out RGB_Frame; Got : out Boolean);
   function  Is_EOF     return Boolean;

end Video_Decoder;
