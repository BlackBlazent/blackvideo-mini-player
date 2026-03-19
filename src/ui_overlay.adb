-- ui_overlay.adb
-- BlackVideo Mini Player — UI Overlay (Ada body)  v2.4

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

   procedure C_Set_No_Video (Active : int)
   with Import, Convention => C, External_Name => "bv_ui_set_no_video";

   procedure C_Set_Thumb_Preview (Path : chars_ptr; Ts : C_float)
   with Import, Convention => C, External_Name => "bv_ui_set_thumb_preview";

   procedure C_Set_Update_Available (Avail : int; Version : chars_ptr)
   with Import, Convention => C, External_Name => "bv_ui_set_update_available";

   procedure C_Show_Update_Overlay
     (Version, Notes, Url : chars_ptr)
   with Import, Convention => C, External_Name => "bv_ui_show_update_overlay";

   procedure C_Hide_Update_Overlay
   with Import, Convention => C, External_Name => "bv_ui_hide_update_overlay";

   function  C_Update_Download_Clicked return int
   with Import, Convention => C, External_Name => "bv_ui_update_download_clicked";

   function  C_Update_Later_Clicked return int
   with Import, Convention => C, External_Name => "bv_ui_update_later_clicked";

   procedure C_Show_Key_Input (Provider : chars_ptr)
   with Import, Convention => C, External_Name => "bv_ui_show_key_input";

   procedure C_Hide_Key_Input
   with Import, Convention => C, External_Name => "bv_ui_hide_key_input";

   function  C_Key_Input_Submitted return int
   with Import, Convention => C, External_Name => "bv_ui_key_input_submitted";

   function  C_Key_Input_Value return chars_ptr
   with Import, Convention => C, External_Name => "bv_ui_key_input_value";

   function  C_Key_Input_Visible return int
   with Import, Convention => C, External_Name => "bv_ui_key_input_visible";

   function  C_Key_Input_Cancelled return int
   with Import, Convention => C, External_Name => "bv_ui_key_input_cancelled";

   procedure C_Handle_Text_Input (Text : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C, External_Name => "bv_ui_handle_text_input";

   procedure C_Handle_Key_Backspace
   with Import, Convention => C, External_Name => "bv_ui_handle_key_backspace";

   procedure C_Paste_Clipboard
   with Import, Convention => C, External_Name => "bv_ui_paste_clipboard";

   procedure C_Clear_Key_Input
   with Import, Convention => C, External_Name => "bv_ui_clear_key_input";

   procedure C_Handle_Click (X, Y : int)
   with Import, Convention => C, External_Name => "bv_ui_handle_click";

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

   procedure Set_No_Video_Mode (Active : Boolean) is
   begin
      C_Set_No_Video (if Active then 1 else 0);
   end Set_No_Video_Mode;

   procedure Set_Thumb_Preview (Path : String; Timestamp : Float) is
      CP : chars_ptr := New_String (Path);
   begin
      C_Set_Thumb_Preview (CP, C_float (Timestamp));
      Free (CP);
   end Set_Thumb_Preview;

   procedure Set_Update_Available (Available : Boolean; Version : String) is
      CV : chars_ptr := New_String (Version);
   begin
      C_Set_Update_Available ((if Available then 1 else 0), CV);
      Free (CV);
   end Set_Update_Available;

   procedure Show_Update_Overlay
     (Version, Notes, Download_URL : String)
   is
      CV : chars_ptr := New_String (Version);
      CN : chars_ptr := New_String (Notes);
      CU : chars_ptr := New_String (Download_URL);
   begin
      C_Show_Update_Overlay (CV, CN, CU);
      Free (CV); Free (CN); Free (CU);
   end Show_Update_Overlay;

   procedure Hide_Update_Overlay is begin C_Hide_Update_Overlay; end;

   function Update_Overlay_Download_Clicked return Boolean is
     (C_Update_Download_Clicked /= 0);

   function Update_Overlay_Later_Clicked return Boolean is
     (C_Update_Later_Clicked /= 0);

   procedure Show_Key_Input (Provider : String) is
      CP : chars_ptr := New_String (Provider);
   begin
      C_Show_Key_Input (CP);
      Free (CP);
   end Show_Key_Input;

   procedure Hide_Key_Input is begin C_Hide_Key_Input; end;

   function Key_Input_Submitted return Boolean is
     (C_Key_Input_Submitted /= 0);

   function Key_Input_Value return String is
      CP : constant chars_ptr := C_Key_Input_Value;
   begin
      if CP = Null_Ptr then return ""; end if;
      return Value (CP);
   end Key_Input_Value;

   function Key_Input_Visible return Boolean is
     (C_Key_Input_Visible /= 0);

   function Key_Input_Cancelled return Boolean is
     (C_Key_Input_Cancelled /= 0);

   procedure Handle_Text_Input (Text : String) is
      CP : Interfaces.C.Strings.chars_ptr :=
             Interfaces.C.Strings.New_String (Text);
   begin
      C_Handle_Text_Input (CP);
      Interfaces.C.Strings.Free (CP);
   end Handle_Text_Input;

   procedure Handle_Key_Backspace is
   begin C_Handle_Key_Backspace; end Handle_Key_Backspace;

   procedure Paste_Clipboard is
   begin C_Paste_Clipboard; end Paste_Clipboard;

   procedure Clear_Key_Input is
   begin C_Clear_Key_Input; end Clear_Key_Input;

   procedure Handle_Click (X, Y : Integer) is
   begin C_Handle_Click (int (X), int (Y)); end Handle_Click;

end UI_Overlay;
