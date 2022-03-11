package body Bounding_Boxes is
   use GL;

   function Surface_Area (B : Bounding_Box) return Single is
      use type Singles.Vector3;

      Dim : constant Singles.Vector3 := B.To - B.From;
   begin
      return 2.0 * (Dim (X) * Dim (Y) + Dim (Y) * Dim (Z) + Dim (X) * Dim (Z));
   end Surface_Area;
end Bounding_Boxes;

