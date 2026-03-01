-- ffmpeg-swresample.ads
-- Ada bindings for libswresample

with Interfaces.C;
with System;
with FFmpeg.AVUtil;

package FFmpeg.SWResample is

   use Interfaces.C;

   type SwrContext is limited private;
   type SwrContext_Ptr is access SwrContext;

   AV_CH_LAYOUT_MONO   : constant unsigned_long := 16#0000_0004#;
   AV_CH_LAYOUT_STEREO : constant unsigned_long := 16#0000_0003#;

   -- ── Setup (handles FFmpeg 5 vs 6+/8 API difference internally) ───────
   function bv_swr_setup
     (Out_Ch_Layout   : unsigned_long;
      Out_Sample_Fmt  : int;
      Out_Sample_Rate : int;
      In_Ch_Layout    : unsigned_long;
      In_Sample_Fmt   : int;
      In_Sample_Rate  : int) return SwrContext_Ptr
   with Import, Convention => C, External_Name => "bv_swr_setup";

   -- ── Convert a full AVFrame → S16 stereo output buffer ─────────────────
   -- Correctly handles planar formats (FLTP fmt=8, FLTP fmt=9, etc.)
   -- by passing all data[] plane pointers via the real AVFrame in C.
   -- This fixes the scraping/choppy audio with planar-format AAC/MP3.
   function bv_swr_convert_frame
     (Swr       : SwrContext_Ptr;
      Out_Buf   : System.Address;   -- caller S16 buffer
      Out_Count : int;              -- capacity in samples/channel
      Frame     : FFmpeg.AVUtil.AVFrame_Ptr) return int
   with Import, Convention => C, External_Name => "bv_swr_convert_frame";

   -- ── Free ──────────────────────────────────────────────────────────────
   procedure swr_free (S : in out SwrContext_Ptr)
   with Import, Convention => C, External_Name => "swr_free";

private
   type SwrContext is null record;

end FFmpeg.SWResample;
