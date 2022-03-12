package body Math_Utils is
   function Image (V : Singles.Vector3) return String is
     ("("    & Single'Image (V (GL.X))
      & ", " & Single'Image (V (GL.Y))
      & ", " & Single'Image (V (GL.Z)) & ")");

   function Log_2 (X : Interfaces.Unsigned_64) return Natural is
      use Interfaces;

      Y      : Unsigned_64 := X;
      Result : Natural := 0;
   begin
      while Y /= 0 loop
         Y      := Shift_Right (Y, 1);
         Result := Result + 1;
      end loop;
      return Result;
   end Log_2;

   function Mul (X, Y : Singles.Vector3) return Singles.Vector3 is
      R : Singles.Vector3;
   begin
      for C in GL.X .. GL.Z loop
         R (C) := X (C) * Y (C);
      end loop;
      return R;
   end Mul;

   function Div (X, Y : Singles.Vector3) return Singles.Vector3 is
      R : Singles.Vector3;
   begin
      for C in GL.X .. GL.Z loop
         R (C) := X (C) / Y (C);
      end loop;
      return R;
   end Div;

   function Reflect (Dir, Normal : Singles.Vector3) return Singles.Vector3 is
      use GL.Types.Singles;
   begin
      return Dir - 2.0 * (Dot (Dir, Normal)) * Normal;
   end Reflect;
end Math_Utils;
