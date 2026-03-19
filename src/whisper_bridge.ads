-- whisper_bridge.ads
-- BlackVideo Mini Player — Whisper.cpp Offline Caption Bridge  v2.3
--
-- Runs whisper-cli.exe as a subprocess to generate or translate
-- subtitles for any video file without network access.
--
-- Architecture:
--   1. Ada extracts audio from the video to a temp WAV via FFmpeg CLI.
--   2. whisper-cli.exe transcribes/translates the WAV, writing an SRT file.
--   3. The output SRT path is returned to the player which loads it as Track 1.
--   4. Status callbacks let the UI show progress while running.
--
-- Whisper path resolution (in order):
--   a) BLACKVIDEO_WHISPER_PATH env var (dev override / .env file)
--   b) whisper-cli.exe in same folder as the player .exe
--   c) whisper-cli.exe on PATH
--
-- Model path resolution:
--   a) BLACKVIDEO_WHISPER_MODEL env var
--   b) models\ggml-base.bin beside the player .exe
--   c) models\ggml-base.bin in the whisper bin directory
--
-- Usage:
--   Generate:  Whisper_Bridge.Generate (Video, Out_SRT, Lang => "auto")
--   Translate: Whisper_Bridge.Generate (Video, Out_SRT, Lang => "auto",
--                                       Translate => True)
--   Both procedures block until done and update g_whisper_status for the UI.

package Whisper_Bridge is

   -- Return the player executable directory (includes trailing backslash)
   function Exe_Dir return String;

   -- Resolve whisper-cli.exe path (returns "" if not found)
   function Find_Whisper return String;

   -- Resolve model path (returns "" if not found)
   function Find_Model return String;

   -- Generate SRT for Video_Path.
   --   Out_SRT      : will hold the path to the generated .srt file on return.
   --   Translate    : if True, add --translate (whisper outputs English).
   --   Status_Cb    : called periodically with a human-readable progress string.
   --
   -- Returns True on success, False on error (prints reason to stdout).
   type Status_Callback is access procedure (Msg : String);

   function Generate
     (Video_Path : String;
      Translate  : Boolean        := False;
      Cb         : Status_Callback := null)
     return String;   -- returns SRT path, or "" on failure

end Whisper_Bridge;
