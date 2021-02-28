with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types; use GL.Types;

with Interfaces;

package Math_Utils is
   function Log_2 (X : Interfaces.Unsigned_64) return Natural;

   function Sign (X : Single) return Single is
     (if    X < 0.0 then -1.0
      elsif X > 0.0 then 1.0
      else               0.0)
      with Inline;

   function Clamp (X, Min, Max : Single) return Single is
     (if    X < Min then Min
      elsif X > Max then Max
      else               X)
      with Inline;

   function Saturate (X : Single) return Single is
     (Clamp (X, 0.0, 1.0))
      with Inline;

   package Single_Elementaries is
      new Ada.Numerics.Generic_Elementary_Functions (Single);

   function Cos (X : Single) return Single
      renames Single_Elementaries.Cos;

   function Sin (X : Single) return Single
      renames Single_Elementaries.Sin;

   function Sqrt (X : Single) return Single
      renames Single_Elementaries.Sqrt;

   function Mul (X, Y : Singles.Vector3) return Singles.Vector3
      with Inline;

   function Div (X, Y : Singles.Vector3) return Singles.Vector3
      with Inline;

   function Dot (X, Y : Singles.Vector3) return Single
      renames Singles.Dot_Product;

   function Cross (X, Y : Singles.Vector3) return Singles.Vector3
      renames Singles.Cross_Product;

   function Min (X, Y : Singles.Vector3) return Singles.Vector3 is
     ((Single'Min (X (GL.X), Y (GL.X)),
       Single'Min (X (GL.Y), Y (GL.Y)),
       Single'Min (X (GL.Z), Y (GL.Z))))
      with Inline;

   function Max (X, Y : Singles.Vector3) return Singles.Vector3 is
     ((Single'Max (X (GL.X), Y (GL.X)),
       Single'Max (X (GL.Y), Y (GL.Y)),
       Single'Max (X (GL.Z), Y (GL.Z))))
      with Inline;

   function Dot2 (V : Singles.Vector3) return Single is
     (Singles.Dot_Product (V, V))
      with Inline;

   function Length (V : Singles.Vector3) return Single is
     (Single_Elementaries.Sqrt (Dot2 (V)))
      with Inline;

   function Normalize (V : Singles.Vector3) return Singles.Vector3 is
     (Singles."/" (V, Length (V)))
     with Inline;

   function Vec_Abs (V : Singles.Vector3) return Singles.Vector3 is
     ((abs V (GL.X), abs V (GL.Y), abs V (GL.Z)))
     with Inline;
end Math_Utils;
