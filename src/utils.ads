-- utils.ads
-- BlackVideo Mini Player - Utility Functions

package Utils is

   --  Extract filename from a full path (cross-platform)
   function Base_Name (Path : String) return String;

   --  Format seconds as MM:SS
   function Format_Time (Seconds : Integer) return String;

end Utils;
