with GL.Vectors;

package body Meshes.Distance_Maps is
   use GL;

   package NVectors3 is new GL.Vectors (GL.Index_3D, Natural);

   function Build_Danielsson
     (Input     : Voxels.Voxelization) return Distance_Map
   is
      use type Singles.Vector3;

      type Vector_Map is array
        (Positive range <>,
         Positive range <>,
         Positive range <>) of Singles.Vector3;

      Inf_Vector : Singles.Vector3 :=
        (others => 1.0e5);

      Bot_Bound : NVectors3.Vector :=
        (Input'First (1), Input'First (2), Input'First (3));

      Top_Bound : NVectors3.Vector :=
        (Input'Last (1), Input'Last (2), Input'Last (3));

      Normalization : Singles.Vector3 :=
        (Single (Input'Length (1)),
         Single (Input'Length (2)),
         Single (Input'Length (3)));

      Result : Vector_Map
        (Input'Range (1), Input'Range (2), Input'Range (3)) :=
           (others => (others => (others => Inf_Vector)));

      Output : Distance_Map
        (Input'Range (1), Input'Range (2), Input'Range (3));

      function Value (V : NVectors3.Vector) return Singles.Vector3
      is
      begin
         for C in X .. Z loop
            if V (C) < Bot_Bound (C) or else V (C) > Top_Bound (C) then
               return Inf_Vector;
            end if;
         end loop;
         return Result (V (X), V (Y), V (Z));
      end Value;

      function Metric (X : Singles.Vector3) return Single is
        (Singles.Dot_Product (X, X));

      function Min_Value
        (A, B, C, D : Singles.Vector3) return Singles.Vector3
      is
         Metric_A : Single := Metric (A);
         Metric_B : Single := Metric (B);
         Metric_C : Single := Metric (C);
         Metric_D : Single := Metric (D);

         Min_Vector : Singles.Vector3 := A;
         Min_Metric : Single := Metric_A;
      begin
         if Metric_B < Min_Metric then
            Min_Metric := Metric_B;
            Min_Vector := B;
         end if;
         if Metric_C < Min_Metric then
            Min_Metric := Metric_C;
            Min_Vector := C;
         end if;
         if Metric_D < Min_Metric then
            Min_Vector := D;
         end if;
         return Min_Vector;
      end Min_Value;

      procedure Rescale (V : in out Singles.Vector3) is
      begin
         for C in X .. Z loop
            V (C) := V (C) / Normalization (C);
         end loop;
      end Rescale;
   begin
      --  First pass: write 0 distance vectors for the shell of the object
      for X in Result'Range (1) loop
         for Y in Result'Range (2) loop
            for Z in Result'Range (3) loop
               if Input (X, Y, Z) then
                  Result (X, Y, Z) := (others => 0.0);
               end if;
            end loop;
         end loop;
      end loop;

      --  Second pass: bottom corner to top corner, looking at the left,
      --  bottom and back values
      for X in Result'Range (1) loop
         for Y in Result'Range (2) loop
            for Z in Result'Range (3) loop
               Result (X, Y, Z) := Min_Value
                 (Value ((X - 1, Y, Z)) + (-1.0, 0.0, 0.0),
                  Value ((X, Y - 1, Z)) + (0.0, -1.0, 0.0),
                  Value ((X, Y, Z - 1)) + (0.0, 0.0, -1.0),
                  Value ((X, Y, Z)));
            end loop;
         end loop;
      end loop;

      --  Third pass: top corner to bottom corner, looking at the right,
      --  top and front values
      for X in reverse Result'Range (1) loop
         for Y in reverse Result'Range (2) loop
            for Z in reverse Result'Range (3) loop
               Result (X, Y, Z) := Min_Value
                 (Value ((X + 1, Y, Z)) + (1.0, 0.0, 0.0),
                  Value ((X, Y + 1, Z)) + (0.0, 1.0, 0.0),
                  Value ((X, Y, Z + 1)) + (0.0, 0.0, 1.0),
                  Value ((X, Y, Z)));
            end loop;
         end loop;
      end loop;

      --  Fourth pass: scale everything down to [0, 1[ range and output
      --  distance map.
      for X in reverse Result'Range (1) loop
         for Y in reverse Result'Range (2) loop
            for Z in reverse Result'Range (3) loop
               Rescale (Result (X, Y, Z));
               Output (X, Y, Z) := Length (Result (X, Y, Z));
            end loop;
         end loop;
      end loop;

      return Output;
   end Build_Danielsson;

   function Build_From_Voxelization
     (Input     : Voxels.Voxelization;
      Algorithm : Transformation_Algorithm) return Distance_Map
   is
   begin
      case Algorithm is
         when Danielsson =>
            return Build_Danielsson (Input);
      end case;
   end Build_From_Voxelization;
end Meshes.Distance_Maps;
