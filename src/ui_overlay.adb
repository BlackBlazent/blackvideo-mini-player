-- ui_overlay.adb
-- BlackVideo Mini Player — UI Overlay (Ada body)  v2.3

with Interfaces.C.Strings;

package body UI_Overlay is

   use Interfaces.C.Strings;

   -- ── Raw C imports ───────────────────────────────────────────────────────

   function  C_Init (Path : chars_ptr) return int
   with Import, Convention => C, External_Name => "bv_ui_init";

   procedure C_Quit
   with Import, Convention => C, External_Name => "bv_ui_quit";

   procedure C_Set_Win_Size (W, H : int)
   with Import, Convention => C, External_Name => "bv_ui_set_window_size";

   procedure C_Draw
     (Rend       : System.Address;
      Position   : C_float;
      Duration   : C_float;
      Playing    : int;
      Looping    : int;
      Muted      : int;
      Volume     : int;
      Fullscreen : int;
      Speed_Idx  : int;
      Visible    : int)
   with Import, Convention => C, External_Name => "bv_ui_draw";

   function  C_Hit_Seek   (X, Y : int) return int
   with Import, Convention => C, External_Name => "bv_ui_hit_seek";

   function  C_Seek_Frac  (X : int) return C_float
   with Import, Convention => C, External_Name => "bv_ui_seek_fraction";

   function  C_Hit_Btn    (X, Y : int) return int
   with Import, Convention => C, External_Name => "bv_ui_hit_button";

   function  C_In_Bar     (X, Y : int) return int
   with Import, Convention => C, External_Name => "bv_ui_in_bar";

   function  C_Hit_Ctx    (X, Y : int) return int
   with Import, Convention => C, External_Name => "bv_ui_hit_ctx";

   procedure C_Set_Hover  (I : int)
   with Import, Convention => C, External_Name => "bv_ui_set_hover_btn";

   procedure C_Set_Seeking (S : int)
   with Import, Convention => C, External_Name => "bv_ui_set_seeking";

   function  C_Is_Seeking return int
   with Import, Convention => C, External_Name => "bv_ui_is_seeking";

   procedure C_Open_Ctx   (X, Y : int)
   with Import, Convention => C, External_Name => "bv_ui_open_ctx";

   procedure C_Close_Ctx
   with Import, Convention => C, External_Name => "bv_ui_close_ctx";

   function  C_Ctx_Open  return int
   with Import, Convention => C, External_Name => "bv_ui_ctx_open";

   procedure C_Set_Subs   (P0, P1, P2 : chars_ptr; Count, Active : int)
   with Import, Convention => C, External_Name => "bv_ui_set_subtitles";

   procedure C_Set_Sub_Text (Text : chars_ptr)
   with Import, Convention => C, External_Name => "bv_ui_set_sub_text";

   procedure C_Set_Whisper_Status (Status : chars_ptr)
   with Import, Convention => C, External_Name => "bv_ui_set_whisper_status";

   -- ── Ada wrappers ────────────────────────────────────────────────────────

   procedure Init (Font_Path : String) is
      CP : chars_ptr := New_String (Font_Path);
      R  : int;
   begin
      R := C_Init (CP);
      Free (CP);
      pragma Unreferenced (R);
   end Init;

   procedure Quit is begin C_Quit; end Quit;

   procedure Set_Window_Size (W, H : Integer) is
   begin C_Set_Win_Size (int (W), int (H)); end Set_Window_Size;

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
      Visible    : Boolean)
   is
      B : constant int := 1;
   begin
      C_Draw (Renderer,
              C_float (Position), C_float (Duration),
              (if Playing    then B else 0),
              (if Looping    then B else 0),
              (if Muted      then B else 0),
              int (Volume),
              (if Fullscreen then B else 0),
              int (Speed_Idx),
              (if Visible    then B else 0));
   end Draw;

   function Hit_Seek  (X, Y : Integer) return Boolean is
     (C_Hit_Seek (int (X), int (Y)) /= 0);

   function Seek_Frac (X : Integer) return Float is
     (Float (C_Seek_Frac (int (X))));

   function Hit_Button (X, Y : Integer) return int is
     (C_Hit_Btn (int (X), int (Y)));

   function In_Bar (X, Y : Integer) return Boolean is
     (C_In_Bar (int (X), int (Y)) /= 0);

   function Hit_Ctx (X, Y : Integer) return int is
     (C_Hit_Ctx (int (X), int (Y)));

   procedure Set_Hover_Btn (Btn : int)   is begin C_Set_Hover (Btn); end;
   procedure Set_Seeking (S : Boolean)   is begin C_Set_Seeking (if S then 1 else 0); end;
   function  Is_Seeking  return Boolean  is (C_Is_Seeking /= 0);

   procedure Open_Ctx (X, Y : Integer)  is begin C_Open_Ctx (int (X), int (Y)); end;
   procedure Close_Ctx                  is begin C_Close_Ctx; end;
   function  Ctx_Open  return Boolean   is (C_Ctx_Open /= 0);

   procedure Set_Subtitles
     (Path0, Path1, Path2 : String;
      Count               : Integer;
      Active              : Integer)
   is
      CP0 : chars_ptr := New_String (Path0);
      CP1 : chars_ptr := New_String (Path1);
      CP2 : chars_ptr := New_String (Path2);
   begin
      C_Set_Subs (CP0, CP1, CP2, int (Count), int (Active));
      Free (CP0); Free (CP1); Free (CP2);
   end Set_Subtitles;

   procedure Set_Sub_Text (Text : String) is
      CP : chars_ptr := New_String (Text);
   begin
      C_Set_Sub_Text (CP);
      Free (CP);
   end Set_Sub_Text;

   procedure Set_Whisper_Status (Status : String) is
      CP : chars_ptr := New_String (Status);
   begin
      C_Set_Whisper_Status (CP);
      Free (CP);
   end Set_Whisper_Status;

end UI_Overlay;
