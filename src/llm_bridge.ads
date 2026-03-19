-- llm_bridge.ads
-- BlackVideo Mini Player — LLM Cloud Caption Provider Bridge  v2.4
--
-- Sends a Whisper-generated transcript to a cloud LLM and asks it to produce
-- a properly timed SRT subtitle file.  Uses WinINet (no libcurl) for HTTPS.
--
-- Provider enum matches CTX_LLM_* constants in ui_overlay.ads.
-- API keys are stored in %APPDATA%\BlackVideo\keys.cfg (INI-style).
--
-- Two-step flow:
--   1. Whisper-tiny generates a raw transcript (fast, ~30s for 60s clip).
--   2. The transcript is sent to the chosen LLM with a timing prompt.
--   3. The LLM returns a formatted SRT which is saved beside the video.
--
-- For OpenAI / Gemini (native audio support), the WAV is uploaded directly
-- and step 1 is skipped.

package LLM_Bridge is

   type Provider is
     (Claude, OpenAI, Gemini, DeepSeek, Grok);

   -- Returns the human-readable name for a provider (used in status banners).
   function Provider_Name (P : Provider) return String;

   -- Returns the stored API key for a provider, or "" if not set.
   function Get_API_Key (P : Provider) return String;

   -- Persists an API key to %APPDATA%\BlackVideo\keys.cfg.
   procedure Set_API_Key (P : Provider; Key : String);

   -- Generate an SRT via the given LLM provider.
   --   Video_Path   : input video (WAV extracted internally if needed)
   --   Transcript   : raw text transcript (from Whisper-tiny); ignored for
   --                  providers that accept audio natively.
   --   Duration_Sec : total video duration in seconds.
   --   Out_SRT      : path where the resulting .srt file should be written.
   --
   -- Returns True on success (file written), False on any error.
   -- Calls Status_CB periodically with progress strings.
   type Status_Callback is access procedure (Msg : String);

   function Generate
     (P            : Provider;
      Video_Path   : String;
      Transcript   : String;
      Duration_Sec : Float;
      Out_SRT      : String;
      Cb           : Status_Callback := null)
     return Boolean;

end LLM_Bridge;
