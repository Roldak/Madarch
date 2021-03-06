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

   procedure Build_From_Mesh
     (Input   : Mesh;
      Bounds  : Bounding_Box;
      Dists   : in out Distance_Map;
      Normals : in out Normal_Map)
   is
      Normalization : Singles.Vector3 :=
        (Single (Dists'Length (1)),
         Single (Dists'Length (2)),
         Single (Dists'Length (3)));

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

      procedure Triangle_Info
        (From        : Singles.Vector3;
         Tri         : Triangle;
         Sq_Dist     : out Single;
         Back_Facing : out Boolean;
         Normal      : out Singles.Vector3)
      is
         use type Singles.Vector3;

         V1 : Singles.Vector3 := Input.Vertices (Tri.A.Vertex_Index);
         V2 : Singles.Vector3 := Input.Vertices (Tri.B.Vertex_Index);
         V3 : Singles.Vector3 := Input.Vertices (Tri.C.Vertex_Index);

         V21 : Singles.Vector3 := V2 - V1;
         V32 : Singles.Vector3 := V3 - V2;
         V13 : Singles.Vector3 := V1 - V3;

         P1 : Singles.Vector3 := From - V1;
         P2 : Singles.Vector3 := From - V2;
         P3 : Singles.Vector3 := From - V3;

         N : Singles.Vector3 := Singles.Cross_Product (V21, V13);

         C1 : Single := Sign (Dot (Cross (V21, N), P1));
         C2 : Single := Sign (Dot (Cross (V32, N), P2));
         C3 : Single := Sign (Dot (Cross (V13, N), P3));
      begin
         Normal      :=
           (Input.Normals (Tri.A.Normal_Index) +
            Input.Normals (Tri.B.Normal_Index) +
            Input.Normals (Tri.C.Normal_Index));
         Back_Facing := Dot (N, P1) >= 0.0;

         if C1 + C2 + C3 < 2.0 then
            declare
               D1 : Single :=
                  Dot2 (V21 * Saturate (Dot (V21, P1) / Dot2 (V21)) - P1);
               D2 : Single :=
                  Dot2 (V32 * Saturate (Dot (V32, P1) / Dot2 (V32)) - P2);
               D3 : Single :=
                  Dot2 (V13 * Saturate (Dot (V13, P1) / Dot2 (V13)) - P3);
            begin
               Sq_Dist := Single'Min (D1, Single'Min (D2, D3));
            end;
         else
            declare
               D : Single := Dot (N, P1);
            begin
               Sq_Dist := D * D / Dot2 (N);
            end;
         end if;
      end Triangle_Info;

      procedure Closest_Triangle_Info
        (Point  : Singles.Vector3;
         Dist   : in out Single;
         Normal : in out Singles.Vector3)
      is
         BF : Boolean := False;

         Cur_Sq_Dist : Single;
         Cur_BF      : Boolean;
         Cur_Normal  : Singles.Vector3;
      begin
         Dist := 1.0e10;
         for T of Input.Triangles loop
            Triangle_Info (Point, T, Cur_Sq_Dist, Cur_BF, Cur_Normal);
            if Cur_Sq_Dist < Dist then
               Dist   := Cur_Sq_Dist;
               BF     := Cur_BF;
               Normal := Cur_Normal;
            end if;
         end loop;

         if BF then
            Dist := 0.0;
         else
            Dist := Sqrt (Dist);
         end if;

         Normal := Normalize (Normal);
      end Closest_Triangle_Info;
   begin
      for Z in Dists'Range (1) loop
         for Y in Dists'Range (2) loop
            for X in Dists'Range (3) loop
               Closest_Triangle_Info
                 (Rebased (X, Y, Z),
                  Dists   (Z, Y, X),
                  Normals (Z, Y, X));
            end loop;
         end loop;
      end loop;
   end Build_From_Mesh;

   procedure Load_To_Texture (Map : Distance_Map) is
   begin
      GL.Objects.Textures.Targets.Texture_3D.Load_From_Data
        (0, GL.Pixels.R16F,
         Map'Length (1), Map'Length (2), Map'Length (3),
         GL.Pixels.Red, GL.Pixels.Float,
         GL.Objects.Textures.Image_Source (Map'Address));
   end Load_To_Texture;

   procedure Load_To_Texture (Map : Normal_Map) is
   begin
      GL.Objects.Textures.Targets.Texture_3D.Load_From_Data
        (0, GL.Pixels.RGB32F,
         Map'Length (1), Map'Length (2), Map'Length (3),
         GL.Pixels.RGB, GL.Pixels.Float,
         GL.Objects.Textures.Image_Source (Map'Address));
   end Load_To_Texture;
end Meshes.Distance_Maps;
