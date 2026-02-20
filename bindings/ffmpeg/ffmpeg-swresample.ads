-- ffmpeg-swresample.ads
-- Ada bindings for libswresample — audio resampling
-- File name: ffmpeg-swresample.ads → package FFmpeg.SWResample

with Interfaces.C;
with System;

package FFmpeg.SWResample is

   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  SwrContext (opaque)
   -- ─────────────────────────────────────────────
   type SwrContext is limited private;
   type SwrContext_Ptr is access SwrContext;

   -- ─────────────────────────────────────────────
   --  Channel layout constants
   -- ─────────────────────────────────────────────
   AV_CH_LAYOUT_MONO   : constant unsigned_long := 16#0000_0004#;
   AV_CH_LAYOUT_STEREO : constant unsigned_long := 16#0000_0003#;

   -- ─────────────────────────────────────────────
   --  API
   -- ─────────────────────────────────────────────

   function swr_alloc return SwrContext_Ptr
   with Import, Convention => C, External_Name => "swr_alloc";

   function swr_alloc_set_opts
     (S               : SwrContext_Ptr;
      Out_Ch_Layout   : unsigned_long;
      Out_Sample_Fmt  : int;
      Out_Sample_Rate : int;
      In_Ch_Layout    : unsigned_long;
      In_Sample_Fmt   : int;
      In_Sample_Rate  : int;
      Log_Offset      : int;
      Log_Ctx         : System.Address) return SwrContext_Ptr
   with Import, Convention => C, External_Name => "swr_alloc_set_opts";

   function swr_init (S : SwrContext_Ptr) return int
   with Import, Convention => C, External_Name => "swr_init";

   function swr_convert
     (S         : SwrContext_Ptr;
      Out_Buf   : System.Address;
      Out_Count : int;
      In_Buf    : System.Address;
      In_Count  : int) return int
   with Import, Convention => C, External_Name => "swr_convert";

   procedure swr_free (S : in out SwrContext_Ptr)
   with Import, Convention => C, External_Name => "swr_free";

private
   type SwrContext is null record;

end FFmpeg.SWResample;
