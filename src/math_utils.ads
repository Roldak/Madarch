with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types; use GL.Types;

with Interfaces;

package Math_Utils is
   function Log_2 (X : Interfaces.Unsigned_64) return Natural;

   package Single_Elementaries is
      new Ada.Numerics.Generic_Elementary_Functions (Single);
end Math_Utils;
