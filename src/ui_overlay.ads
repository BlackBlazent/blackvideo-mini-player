-- ui_overlay.ads
-- BlackVideo Mini Player — UI Overlay (Ada spec)  v2.3
-- Thin wrapper over ui_overlay.c.

with Interfaces.C;
with System;

package UI_Overlay is

   use Interfaces.C;

   -- ── Button indices ─────────────────────────────────────────────────────
   BTN_PREV       : constant int := 0;
   BTN_PLAY_PAUSE : constant int := 1;
   BTN_NEXT       : constant int := 2;
   BTN_LOOP       : constant int := 3;
   BTN_VOLUME     : constant int := 4;
   BTN_SPEED      : constant int := 5;
   BTN_FULLSCREEN : constant int := 6;
   BTN_MENU       : constant int := 7;

   -- ── Context menu item indices ──────────────────────────────────────────
   CTX_OPEN_FILE     : constant int := 0;
   CTX_SUB_NONE      : constant int := 1;
   CTX_SUB_1         : constant int := 2;
   CTX_SUB_2         : constant int := 3;
   CTX_SUB_3         : constant int := 4;
   CTX_WHISPER_GEN   : constant int := 5;  -- Generate captions
   CTX_WHISPER_TRANS : constant int := 6;  -- Translate to English

   -- ── Speed table ─────────────────────────────────────────────────────────
   type Speed_Index is new int range 0 .. 3;
   Speed_Values : constant array (Speed_Index) of Float :=
     (0 => 0.5, 1 => 1.0, 2 => 1.5, 3 => 2.0);

   -- ── Lifecycle ───────────────────────────────────────────────────────────
   procedure Init (Font_Path : String);
   procedure Quit;
   procedure Set_Window_Size (W, H : Integer);

   -- ── Draw ────────────────────────────────────────────────────────────────
   procedure Draw
     (Renderer   : System.Address;
      Position   : Float;
      Duration   : Float;
      Playing    : Boolean;
      Looping    : Boolean;
      Muted      : Boolean;
      Volume     : Integer;
      Fullscreen : Boolean;
      Speed_Idx  : Speed_Index;
      Visible    : Boolean);

   -- ── Hit-testing ─────────────────────────────────────────────────────────
   function Hit_Seek    (X, Y : Integer) return Boolean;
   function Seek_Frac   (X   : Integer) return Float;
   function Hit_Button  (X, Y : Integer) return int;
   function In_Bar      (X, Y : Integer) return Boolean;
   function Hit_Ctx     (X, Y : Integer) return int;

   -- ── Interaction state ───────────────────────────────────────────────────
   procedure Set_Hover_Btn (Btn : int);
   procedure Set_Seeking   (S   : Boolean);
   function  Is_Seeking    return Boolean;

   -- ── Context menu ────────────────────────────────────────────────────────
   procedure Open_Ctx  (X, Y : Integer);
   procedure Close_Ctx;
   function  Ctx_Open  return Boolean;

   -- ── Subtitles (up to 3 tracks) ──────────────────────────────────────────
   procedure Set_Subtitles
     (Path0, Path1, Path2 : String;
      Count               : Integer;
      Active              : Integer);

   -- ── Subtitle cue text (current displayed line) ──────────────────────────
   procedure Set_Sub_Text (Text : String);

   -- ── Whisper status banner ────────────────────────────────────────────────
   procedure Set_Whisper_Status (Status : String);

end UI_Overlay;
