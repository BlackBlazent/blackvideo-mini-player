-- updater.ads
-- BlackVideo Mini Player — GitHub Release Auto-Updater  v2.4
--
-- Checks latest.json in the repo root for a newer version.
-- The check runs on a background Windows thread at startup so it
-- never delays the player opening.
--
-- latest.json format (place in repo root):
--   {
--     "version":      "2.4.0",
--     "release_url":  "https://github.com/.../releases/tag/v2.4.0",
--     "download_url": "https://sourceforge.net/.../download",
--     "notes":        "Short description of what changed"
--   }

package Updater is

   Current_Version : constant String := "2.4.0";

   -- URL to the latest.json in the repo root.
   Manifest_URL : constant String :=
     "https://raw.githubusercontent.com/BlackBlazent/" &
     "blackvideo-mini-player/main/latest.json";

   -- Start the background version check thread.
   -- Call once at player startup.  Non-blocking.
   procedure Start_Check;

   -- Returns True once the background check has completed.
   function Check_Done return Boolean;

   -- True if a newer version is available (only valid when Check_Done).
   function Update_Available return Boolean;

   -- Fields from the remote manifest (valid when Check_Done = True).
   function Remote_Version  return String;
   function Release_Notes   return String;
   function Download_URL    return String;

   -- Compare two "MAJOR.MINOR.PATCH" version strings.
   -- Returns True if Remote > Local.
   function Is_Newer (Remote, Local : String) return Boolean;

   -- Open the download URL in the default browser.
   procedure Open_Browser (URL : String);

end Updater;
