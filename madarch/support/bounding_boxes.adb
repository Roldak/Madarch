package body Bounding_Boxes is
   use GL;

   procedure Extend
     (BB : in out Bounding_Box; Point : Singles.Vector3)
   is
   begin
      for C in X .. Z loop
         if Point (C) < BB.From (C) then
            BB.From (C) := Point (C);
         end if;
         if Point (C) > BB.To (C) then
            BB.To (C) := Point (C);
         end if;
      end loop;
   end Extend;

   function Surface_Area (B : Bounding_Box) return Single is
      use type Singles.Vector3;

      Dim : constant Singles.Vector3 := B.To - B.From;
   begin
      return 2.0 * (Dim (X) * Dim (Y) + Dim (Y) * Dim (Z) + Dim (X) * Dim (Z));
   end Surface_Area;
end Bounding_Boxes;

