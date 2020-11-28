with GL.Vectors;

package body Meshes.Voxels is
   use GL;

   package NVectors3 is new GL.Vectors (GL.Index_3D, Natural);

   function Voxelize
     (Target : Mesh;
      Bounds : Bounding_Box;
      Freq   : Grid_Frequency) return Voxelization
   is
      use type Singles.Vector3;

      Result : Voxelization
        (1 .. Freq.Width, 1 .. Freq.Height, 1 .. Freq.Depth) :=
           (others => (others => (others => False)));

      Freq_Vector : Singles.Vector3 :=
        (Single (Freq.Width), Single (Freq.Height), Single (Freq.Depth));

      Bounds_Size : Singles.Vector3 := Bounds.To - Bounds.From;

      function In_Bounds (V : Singles.Vector3) return Boolean is
      begin
         for C in X .. Z loop
            if V (C) < Bounds.From (C) or else V (C) >= Bounds.To (C) then
               return False;
            end if;
         end loop;
         return True;
      end In_Bounds;

      procedure Set (V : Singles.Vector3) is
         Offset : Singles.Vector3 := V - Bounds.From;
         Coord  : NVectors3.Vector;
      begin
         for C in X .. Z loop
            Offset (C) := Offset (C) * Freq_Vector (C);
            Offset (C) := Offset (C) / Bounds_Size (C);
            Coord  (C) := Natural (Single'Floor (Offset (C))) + 1;
         end loop;

         Result (Coord (X), Coord (Y), Coord (Z)) := True;
      end Set;
   begin
      for V of Target.Vertices loop
         if In_Bounds (V) then
            Set (V);
         end if;
      end loop;
      return Result;
   end Voxelize;
end Meshes.Voxels;
