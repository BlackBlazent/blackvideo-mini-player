-- sdl-events.ads
-- Package SDL.Events — flat SDL_Event polling (avoids Ada union issues)

with Interfaces.C;

package SDL.Events is

   use Interfaces.C;

   -- ─── Event type codes (SDL_EventType) ────────────────────────────────
   SDL_QUIT            : constant unsigned := 16#100#;
   SDL_KEYDOWN         : constant unsigned := 16#300#;
   SDL_KEYUP           : constant unsigned := 16#301#;
   SDL_MOUSEMOTION     : constant unsigned := 16#400#;
   SDL_MOUSEBUTTONDOWN : constant unsigned := 16#401#;
   SDL_WINDOWEVENT     : constant unsigned := 16#200#;

   --  SDL_WindowEventID
   SDL_WINDOWEVENT_SIZE_CHANGED : constant unsigned_char := 6;
   SDL_WINDOWEVENT_RESIZED      : constant unsigned_char := 5;
   SDL_WINDOWEVENT_CLOSE        : constant unsigned_char := 14;

   -- ─── Raw SDL_Event: 56-byte flat buffer ──────────────────────────────
   SDL_EVENT_SIZE : constant := 56;

   type Raw_Bytes is array (0 .. SDL_EVENT_SIZE - 1) of unsigned_char
     with Convention => C;

   type SDL_Event is record
      Bytes : Raw_Bytes;
   end record with Convention => C, Size => SDL_EVENT_SIZE * 8;

   -- ─── Field extractors ─────────────────────────────────────────────────
   function Event_Type      (E : SDL_Event) return unsigned;
   function Window_Sub_Event (E : SDL_Event) return unsigned_char;
   function Window_Data1    (E : SDL_Event) return int;
   function Window_Data2    (E : SDL_Event) return int;
   function Key_Sym         (E : SDL_Event) return int;
   function Key_Scancode    (E : SDL_Event) return int;

   -- ─── Poll ─────────────────────────────────────────────────────────────
   function Poll (E : out SDL_Event) return Boolean;

private
   function SDL_PollEvent (E : out SDL_Event) return int
   with Import, Convention => C, External_Name => "SDL_PollEvent";

end SDL.Events;
