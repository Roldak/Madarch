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

   function Normalize (V : Singles.Vector3) return Singles.Vector3 is
      R : Singles.Vector3;
      L : Single := Length (V);
   begin
      for C in GL.X .. GL.Z loop
         R (C) := V (C) / L;
      end loop;
      return R;
   end Normalize;
end Math_Utils;
