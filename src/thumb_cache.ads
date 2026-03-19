-- thumb_cache.ads
-- BlackVideo Mini Player — Video Thumbnail Cache  v2.4
--
-- Extracts one JPEG frame every 10 seconds from the video using a background
-- ffmpeg job (same WinExec/CreateProcess pattern as Whisper).
-- The cache directory is:  build\cache\<8-char-hash>\frame_%04d.jpg
--
-- Usage:
--   1. Call Generate(Video_Path) once when a video is opened.
--   2. Call Frame_Path(Video_Path, Hover_Pos) each frame to get the JPEG
--      for a given hover position on the seek bar.  Returns "" if not ready.
--   3. Call Clear(Video_Path) from "Clear Thumbnail Cache" menu item.

package Thumb_Cache is

   -- Start background ffmpeg job to extract thumbnails.
   -- Non-blocking.  Returns immediately.
   procedure Generate (Video_Path : String);

   -- Return the JPEG path for a hover position (seconds).
   -- Returns "" if the cache is not ready or the file does not exist yet.
   function Frame_Path (Video_Path : String; Pos_Seconds : Float)
     return String;

   -- True while the background extraction job is running.
   function Is_Generating return Boolean;

   -- Delete all cached thumbnails for this video.
   procedure Clear (Video_Path : String);

   -- Delete all cached thumbnails for all videos (full cache wipe).
   procedure Clear_All;

   -- The interval between extracted frames (seconds).
   Thumb_Interval : constant Float := 10.0;

end Thumb_Cache;
