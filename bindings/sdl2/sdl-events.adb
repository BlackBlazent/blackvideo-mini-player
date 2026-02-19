-- sdl-events.adb

package body SDL.Events is

   function Poll (E : out Flat_Event) return Boolean is
   begin
      return SDL_PollEvent (E) = 1;
   end Poll;

end SDL.Events;
