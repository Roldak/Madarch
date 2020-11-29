package body Math_Utils is
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
end Math_Utils;
