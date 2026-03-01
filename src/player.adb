-- player.adb
-- BlackVideo Mini Player - Core Player

with Ada.Text_IO;
with Ada.Exceptions;
with Interfaces.C;

with SDL;
with SDL.Video;
with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL.Events;
with SDL.Events.Keyboards;

with Video_Decoder;
with Renderer;
with Audio;
with Utils;

package body Player is

   use Ada.Text_IO;
   use Interfaces.C;
   use SDL.Events;
   use SDL.Events.Keyboards;

   State      : Player_State := Stopped;
   Volume     : Integer      := 80;
   Fullscreen : Boolean      := False;

   function Current_State  return Player_State is (State);
   function Current_Volume return Integer      is (Volume);
   function Is_Fullscreen  return Boolean      is (Fullscreen);

   procedure Handle_Key
     (Ev   : SDL.Events.SDL_Event;
      Win  : SDL.Video.Window_Handle;
      Quit : out Boolean);

   -- ── Run ──────────────────────────────────────────────────────────────
   procedure Run (Video_File : String) is
      Win  : SDL.Video.Window_Handle;
      Rend : SDL.Video.Renderer_Handle;
      Tex  : SDL.Video.Texture_Handle;

      Ev   : SDL.Events.SDL_Event;
      Quit : Boolean := False;

      Vid_W          : Integer;
      Vid_H          : Integer;
      Frame_Delay_MS : Integer;
      Ret            : int;

      -- Frame timing: measure how long each iteration actually takes and
      -- sleep only the remainder, so the video runs at the correct speed.
      Frame_Start : unsigned;
      Elapsed     : unsigned;
      Sleep_MS    : unsigned;
   begin
      Put_Line ("[Player] Init SDL2 ...");
      Ret := SDL.Initialize (SDL.Flags.Enable_Video or SDL.Flags.Enable_Audio);
      if Ret /= 0 then
         raise Program_Error with "[Player] SDL_Init failed";
      end if;

      Put_Line ("[Player] Opening: " & Video_File);
      Video_Decoder.Open (Video_File, Vid_W, Vid_H, Frame_Delay_MS);

      SDL.Video.Windows.Create
        (Win,
         Title  => "BlackVideo - " & Utils.Base_Name (Video_File),
         X      => SDL.Video.Windows.SDL_WINDOWPOS_CENTERED,
         Y      => SDL.Video.Windows.SDL_WINDOWPOS_CENTERED,
         Width  => int (Vid_W),
         Height => int (Vid_H),
         Flags  => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Create
        (Rend, Win,
         Flags => SDL.Video.Renderers.SDL_RENDERER_ACCELERATED
                  or SDL.Video.Renderers.SDL_RENDERER_PRESENTVSYNC);

      Renderer.Init_Texture (Rend, Tex, Vid_W, Vid_H);
      Audio.Init (Volume);
      Video_Decoder.Start_Decoding;
      Audio.Start;

      State := Playing;
      Put_Line ("[Player] Playing. SPACE=pause  F=fullscreen  Q/ESC=quit");

      -- ── Main loop ─────────────────────────────────────────────────────
      while not Quit loop

         Frame_Start := SDL.Get_Ticks;

         -- ── Event pump ────────────────────────────────────────────────
         while SDL.Events.Poll (Ev) loop
            declare
               Ev_Type : constant unsigned := SDL.Events.Event_Type (Ev);
            begin
               if Ev_Type = SDL_QUIT then
                  Quit := True;
               elsif Ev_Type = SDL_KEYDOWN then
                  Handle_Key (Ev, Win, Quit);
               elsif Ev_Type = SDL_WINDOWEVENT then
                  if SDL.Events.Window_Sub_Event (Ev)
                     = SDL_WINDOWEVENT_SIZE_CHANGED
                  then
                     Renderer.On_Resize
                       (Integer (SDL.Events.Window_Data1 (Ev)),
                        Integer (SDL.Events.Window_Data2 (Ev)));
                  end if;
               end if;
            end;
         end loop;

         -- ── Decode + display ──────────────────────────────────────────
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

         Renderer.Draw (Rend, Tex, Vid_W, Vid_H);

         -- ── Frame pacing: sleep only the time remaining in this frame ──
         -- Without this, decode+render time adds to the fixed delay,
         -- making the video run slower than its actual FPS.
         Elapsed := SDL.Get_Ticks - Frame_Start;
         if Elapsed < unsigned (Frame_Delay_MS) then
            Sleep_MS := unsigned (Frame_Delay_MS) - Elapsed;
            SDL.Delay_MS (Sleep_MS);
         end if;
         -- If elapsed >= Frame_Delay_MS (heavy frame), don't sleep at all.

      end loop;

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

   -- ── Handle_Key ───────────────────────────────────────────────────────
   procedure Handle_Key
     (Ev   : SDL.Events.SDL_Event;
      Win  : SDL.Video.Window_Handle;
      Quit : out Boolean)
   is
      K : constant int := SDL.Events.Key_Sym (Ev);
   begin
      Quit := False;

      if K = SDLK_ESCAPE or else K = SDLK_q then
         Quit := True;
      elsif K = SDLK_SPACE then
         if State = Playing then
            State := Paused;
            Video_Decoder.Pause; Audio.Pause;
            Put_Line ("[Player] Paused");
         else
            State := Playing;
            Video_Decoder.Resume; Audio.Resume;
            Put_Line ("[Player] Resumed");
         end if;
      elsif K = SDLK_LEFT then
         Video_Decoder.Seek (-5.0);
         Put_Line ("[Player] Seek -5 s");
      elsif K = SDLK_RIGHT then
         Video_Decoder.Seek (+5.0);
         Put_Line ("[Player] Seek +5 s");
      elsif K = SDLK_UP then
         Volume := Integer'Min (Volume + 10, 128);
         Audio.Set_Volume (Volume);
         Put_Line ("[Player] Volume =" & Integer'Image (Volume));
      elsif K = SDLK_DOWN then
         Volume := Integer'Max (Volume - 10, 0);
         Audio.Set_Volume (Volume);
         Put_Line ("[Player] Volume =" & Integer'Image (Volume));
      elsif K = SDLK_m then
         Audio.Toggle_Mute;
      elsif K = SDLK_f then
         Fullscreen := not Fullscreen;
         SDL.Video.Windows.Set_Fullscreen (Win, Fullscreen);
         Put_Line ("[Player] Fullscreen=" & Boolean'Image (Fullscreen));
      end if;
   end Handle_Key;

end Player;
