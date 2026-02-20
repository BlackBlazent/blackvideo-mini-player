-- ffmpeg.ads
-- Parent package for all FFmpeg Ada bindings.
-- Ada requires every child package (FFmpeg.AVUtil, FFmpeg.AVCodec, etc.)
-- to have its parent package declared. This file fulfils that requirement.
-- The package body is intentionally absent (pure declarations only).

package FFmpeg is
   pragma Pure;
end FFmpeg;
