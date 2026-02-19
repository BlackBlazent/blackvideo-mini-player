-- player.adb
-- BlackVideo Mini Player - Core Player Logic
-- Integrates SDL2 window + event loop + FFmpeg decode + audio

with Ada.Text_IO;
with Ada.Exceptions;
with Interfaces.C;

with SDL;
with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL.Video.Textures;
with SDL.Events;
with SDL.Events.Keyboards;
with SDL.Audio;

with Video_Decoder;
with Renderer;
with Audio;
with Utils;

package body Player is

   use Ada.Text_IO;
   use Interfaces.C;

   -- ─────────────────────────────────────────────
   --  Internal State
   -- ─────────────────────────────────────────────
   State        : Player_State := Stopped;
   Volume       : Integer      := 80;     -- default 80/128 (~63%)
   Fullscreen   : Boolean      := False;

   function Current_State  return Player_State is (State);
   function Current_Volume return Integer      is (Volume);
   function Is_Fullscreen  return Boolean      is (Fullscreen);

   -- ─────────────────────────────────────────────
   --  Run
   -- ─────────────────────────────────────────────
   procedure Run (Video_File : String) is
      Win      : SDL.Video.Windows.Window;
      Rend     : SDL.Video.Renderers.Renderer;
      Tex      : SDL.Video.Textures.Texture;
      Event    : SDL.Events.Event;
      Quit     : Boolean := False;

      Vid_W    : Integer;
      Vid_H    : Integer;
      Frame_Rate_Delay : Integer;  -- ms per frame

   begin
      Put_Line ("[Player] Initializing SDL2 ...");

      --  Init SDL subsystems
      SDL.Initialize (SDL.Flags.Enable_Video or SDL.Flags.Enable_Audio);

      --  ── Open FFmpeg decoder ──────────────────
      Put_Line ("[Player] Opening: " & Video_File);
      Video_Decoder.Open (Video_File, Vid_W, Vid_H, Frame_Rate_Delay);

      --  ── Create Window ────────────────────────
      SDL.Video.Windows.Create
        (Win,
         Title  => "BlackVideo - " & Utils.Base_Name (Video_File),
         X      => SDL.Video.Windows.Centered_Window,
         Y      => SDL.Video.Windows.Centered_Window,
         Width  => Vid_W,
         Height => Vid_H,
         Flags  => SDL.Video.Windows.Resizable);

      --  ── Create Renderer ──────────────────────
      SDL.Video.Renderers.Create
        (Rend, Win,
         Flags => SDL.Video.Renderers.Accelerated
                  or SDL.Video.Renderers.Present_V_Sync);

      --  ── Create streaming YUV texture ─────────
      Renderer.Init_Texture (Rend, Tex, Vid_W, Vid_H);

      --  ── Init Audio ───────────────────────────
      Audio.Init (Volume);

      --  ── Start decode / audio threads ─────────
      Video_Decoder.Start_Decoding;
      Audio.Start;

      State := Playing;
      Put_Line ("[Player] Playback started. Press SPACE to pause.");

      -- ─────────────────────────────────────────
      --  Main Loop
      -- ─────────────────────────────────────────
      while not Quit loop

         --  ── Handle Events ─────────────────────
         while SDL.Events.Poll (Event) loop
            case Event.Common.Event_Type is

               when SDL.Events.Quit =>
                  Quit := True;

               when SDL.Events.Key_Down =>
                  Handle_Key (Event.Key, Win, Rend, Quit);

               when SDL.Events.Window_Event =>
                  --  handle resize
                  if Event.Window.Event = SDL.Events.Window_Size_Changed then
                     Renderer.On_Resize (Rend, Event.Window.Data1, Event.Window.Data2);
                  end if;

               when others =>
                  null;
            end case;
         end loop;

         --  ── Get next decoded frame ────────────
         if State = Playing then
            declare
               Frame : Video_Decoder.RGB_Frame;
               Got   : Boolean;
            begin
               Video_Decoder.Next_Frame (Frame, Got);

               if Got then
                  Renderer.Upload_Frame (Tex, Frame, Vid_W, Vid_H);
               elsif Video_Decoder.Is_EOF then
                  Put_Line ("[Player] End of file.");
                  Quit := True;
               end if;
            end;
         end if;

         --  ── Render ───────────────────────────
         Renderer.Draw (Rend, Tex, Vid_W, Vid_H);

         --  ── Frame pacing ─────────────────────
         SDL.Delay_MS (Interfaces.C.unsigned (Frame_Rate_Delay));

      end loop;

      --  ── Cleanup ──────────────────────────────
      Put_Line ("[Player] Shutting down ...");
      Audio.Stop;
      Video_Decoder.Close;
      Renderer.Destroy_Texture (Tex);
      SDL.Video.Renderers.Destroy (Rend);
      SDL.Video.Windows.Destroy (Win);
      SDL.Quit;
      State := Stopped;

   exception
      when E : others =>
         Put_Line ("[Player] FATAL: " & Ada.Exceptions.Exception_Message (E));
         SDL.Quit;
         raise;
   end Run;

   -- ─────────────────────────────────────────────
   --  Keyboard Handler
   -- ─────────────────────────────────────────────
   procedure Handle_Key
     (Key  : SDL.Events.Keyboards.Key_Event;
      Win  : in out SDL.Video.Windows.Window;
      Rend : in out SDL.Video.Renderers.Renderer;
      Quit : out Boolean)
   is
      use SDL.Events.Keyboards;
   begin
      Quit := False;

      case Key.Key_Sym.Sym is

         --  Quit
         when SDLK_Escape | SDLK_q =>
            Quit := True;

         --  Play / Pause
         when SDLK_Space =>
            if State = Playing then
               State := Paused;
               Video_Decoder.Pause;
               Audio.Pause;
               Put_Line ("[Player] Paused");
            else
               State := Playing;
               Video_Decoder.Resume;
               Audio.Resume;
               Put_Line ("[Player] Resumed");
            end if;

         --  Seek backward 5s
         when SDLK_Left =>
            Video_Decoder.Seek (-5.0);
            Put_Line ("[Player] Seek -5s");

         --  Seek forward 5s
         when SDLK_Right =>
            Video_Decoder.Seek (+5.0);
            Put_Line ("[Player] Seek +5s");

         --  Volume up
         when SDLK_Up =>
            Volume := Integer'Min (Volume + 10, 128);
            Audio.Set_Volume (Volume);
            Put_Line ("[Player] Volume:" & Volume'Image);

         --  Volume down
         when SDLK_Down =>
            Volume := Integer'Max (Volume - 10, 0);
            Audio.Set_Volume (Volume);
            Put_Line ("[Player] Volume:" & Volume'Image);

         --  Mute toggle
         when SDLK_m =>
            Audio.Toggle_Mute;
            Put_Line ("[Player] Mute toggled");

         --  Fullscreen toggle
         when SDLK_f =>
            Fullscreen := not Fullscreen;
            SDL.Video.Windows.Set_Fullscreen (Win, Fullscreen);
            Put_Line ("[Player] Fullscreen:" & Fullscreen'Image);

         when others =>
            null;
      end case;
   end Handle_Key;

end Player;
