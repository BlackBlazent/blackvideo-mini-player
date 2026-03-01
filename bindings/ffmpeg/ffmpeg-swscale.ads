-- ffmpeg-swscale.ads
-- Ada bindings for libswscale — YUV → RGB pixel conversion
-- File name: ffmpeg-swscale.ads → package FFmpeg.SWScale

with Interfaces.C;
with System;
with FFmpeg.AVUtil;

package FFmpeg.SWScale is

   use Interfaces.C;
   -- Note: FFmpeg.AVUtil types referenced by full name below (no use clause needed)

   -- ─────────────────────────────────────────────
   --  SwsContext (opaque)
   -- ─────────────────────────────────────────────
   type SwsContext is limited private;
   type SwsContext_Ptr is access SwsContext;

   -- ─────────────────────────────────────────────
   --  Scaling algorithm flags
   -- ─────────────────────────────────────────────
   SWS_FAST_BILINEAR : constant int :=   1;
   SWS_BILINEAR      : constant int :=   2;
   SWS_BICUBIC       : constant int :=   4;
   SWS_LANCZOS       : constant int := 512;

   -- ─────────────────────────────────────────────
   --  API
   -- ─────────────────────────────────────────────

   function sws_getContext
     (Src_W       : int;
      Src_H       : int;
      Src_Format  : int;
      Dst_W       : int;
      Dst_H       : int;
      Dst_Format  : int;
      Flags       : int;
      Src_Filter  : System.Address;   -- null
      Dst_Filter  : System.Address;   -- null
      Param       : System.Address)   -- null
      return SwsContext_Ptr
   with Import, Convention => C, External_Name => "sws_getContext";

   function sws_scale
     (Ctx          : SwsContext_Ptr;
      Src_Slice    : FFmpeg.AVUtil.Plane_Data_Array;
      Src_Stride   : FFmpeg.AVUtil.Plane_Linesize_Array;
      Src_Slice_Y  : int;
      Src_Slice_H  : int;
      Dst          : FFmpeg.AVUtil.Plane_Data_Array;
      Dst_Stride   : FFmpeg.AVUtil.Plane_Linesize_Array) return int
   with Import, Convention => C, External_Name => "sws_scale";

   procedure sws_freeContext (Ctx : SwsContext_Ptr)
   with Import, Convention => C, External_Name => "sws_freeContext";

private
   type SwsContext is null record;

end FFmpeg.SWScale;
