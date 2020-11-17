with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Vectors;

with Math_Utils; use Math_Utils;

package body Meshes.Distance_Maps is
   use GL;

   pragma Suppress (All_Checks);

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

   function Build_From_Mesh
     (Input  : Mesh;
      Bounds : Bounding_Box;
      Res_X, Res_Y, Res_Z : Natural) return Distance_Map
   is
      Normalization : Singles.Vector3 :=
        (Single (Res_X),
         Single (Res_Y),
         Single (Res_Z));

      function Rebased (X, Y, Z : Positive) return Singles.Vector3 is
         R : Singles.Vector3 :=
           (Single (X), Single (Y), Single (Z));
      begin
         for C in GL.X .. GL.Z loop
            -- [1 .. Res_C] => [0 .. 1]
            R (C) := (R (C) - 1.0) / (Normalization (C) - 1.0);

            -- [0 .. 1] => [From .. To]
            R (C) := R (C) * (Bounds.To (C) - Bounds.From (C)) + Bounds.From (C);
         end loop;
         return R;
      end Rebased;

      function Intersects_Triangle
        (From, Dir : Singles.Vector3; T : Triangle) return Boolean
      is
         -- pragma Suppress (All_Checks);
         use type Singles.Vector3;

         V1 : Singles.Vector3 := Input.Vertices (T.A.Vertex_Index);
         V2 : Singles.Vector3 := Input.Vertices (T.B.Vertex_Index);
         V3 : Singles.Vector3 := Input.Vertices (T.C.Vertex_Index);

         V12 : Singles.Vector3 := V2 - V1;
         V13 : Singles.Vector3 := V3 - V1;

         OV1 : Singles.Vector3 := From - V1;

         N : Singles.Vector3 := Cross (V12, V13);
         Q : Singles.Vector3 := Cross (OV1, Dir);

         D : Single := 1.0 / Dot (Dir, N);
         U : Single := D * Dot (-Q, V13);
         V : Single := D * Dot ( Q, V12);
         K : Single := D * Dot (-N, OV1);
      begin
         return K >= 0.0 and U >= 0.0 and V >= 0.0 and U + V <= 1.0;
      end Intersects_Triangle;

      function Squared_Dist_To_Triangle
        (P : Singles.Vector3;
         T : Triangle;
         Back_Facing : out Boolean) return Single
      is
         use type Singles.Vector3;

         V1 : Singles.Vector3 := Input.Vertices (T.A.Vertex_Index);
         V2 : Singles.Vector3 := Input.Vertices (T.B.Vertex_Index);
         V3 : Singles.Vector3 := Input.Vertices (T.C.Vertex_Index);

         V21 : Singles.Vector3 := V2 - V1;
         V32 : Singles.Vector3 := V3 - v2;
         V13 : Singles.Vector3 := V1 - V3;

         P1 : Singles.Vector3 := P - V1;
         P2 : Singles.Vector3 := P - V2;
         P3 : Singles.Vector3 := P - V3;

         Normal : Singles.Vector3 := Singles.Cross_Product (V21, V13);

         C1 : Single := Sign (Dot (Cross (V21, Normal), P1));
         C2 : Single := Sign (Dot (Cross (V32, Normal), P2));
         C3 : Single := Sign (Dot (Cross (V13, Normal), P3));
      begin
         Back_Facing := Dot (Normal, P1) > 0.0;

         if C1 + C2 + C3 < 2.0 then
            declare
               D1 : Single :=
                  Dot2 (V21 * Saturate (Dot (V21, P1) / Dot2 (V21)) - P1);
               D2 : Single :=
                  Dot2 (V32 * Saturate (Dot (V32, P1) / Dot2 (V32)) - P2);
               D3 : Single :=
                  Dot2 (V13 * Saturate (Dot (V13, P1) / Dot2 (V13)) - P3);
            begin
               return Single'Min (D1, Single'Min (D2, D3));
            end;
         else
            declare
               D : Single := Dot (Normal, P1);
            begin
               return D * D / Dot2 (Normal);
            end;
         end if;
      end Squared_Dist_To_Triangle;

      Ray_Dir : constant Singles.Vector3 := (1.0, 0.0, 0.0);

      function Dist_To_Closest_Triangle (P : Singles.Vector3) return Single is
         Closest    : Single := 1.0e10;
         Closest_BF : Boolean := False;

         Dist : Single;
         BF   : Boolean;
      begin
         for T of Input.Triangles loop
            Dist := Squared_Dist_To_Triangle (P, T, BF);
            if Dist < Closest then
               Closest := Dist;
               Closest_BF := BF;
            end if;
         end loop;

         if Closest_BF then
            return 0.0;
         else
            return Sqrt (Closest);
         end if;
      end Dist_To_Closest_Triangle;

      Output : Distance_Map
        (1 .. Res_Z, 1 .. Res_Y, 1 .. Res_X);
   begin
      for Z in Output'Range (1) loop
         for Y in Output'Range (2) loop
            for X in Output'Range (3) loop
               Output (Z, Y, X) :=
                  Dist_To_Closest_Triangle (Rebased (X, Y, Z));
            end loop;
         end loop;
      end loop;

      return Output;
   end Build_From_Mesh;

   procedure Load_To_Texture (Map : aliased Distance_Map) is
   begin
      GL.Objects.Textures.Targets.Texture_3D.Load_From_Data
        (0, GL.Pixels.R16F,
         Map'Length (1), Map'Length (2), Map'Length (3),
         GL.Pixels.Red, GL.Pixels.Float,
         GL.Objects.Textures.Image_Source (Map'Address));
   end Load_To_Texture;
end Meshes.Distance_Maps;
