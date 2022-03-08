with Ada.Text_IO;

package body Meshes is
   use GL;

   function Compute_Bounding_Box (M : Mesh) return Bounding_Box is
      use type Ada.Containers.Count_Type;

      From : Singles.Vector3;
      To   : Singles.Vector3;
   begin
      if M.Vertices.Length = 0 then
         return ((others => 0.0), (others => 0.0));
      end if;

      From := M.Vertices (1);
      To   := From;

      for V of M.Vertices loop
         for C in X .. Z loop
            if V (C) < From (C) then
               From (C) := V (C);
            end if;
            if V (C) > To (C) then
               To (C) := V (C);
            end if;
         end loop;
      end loop;

      return (From, To);
   end Compute_Bounding_Box;

   procedure Iterate_Triangles
     (Self    : Mesh;
      Process : access procedure (A, B, C: Singles.Vector3))
   is
   begin
      for Triangle of Self.Triangles loop
         Process
           (Self.Vertices (Triangle.A.Vertex_Index),
            Self.Vertices (Triangle.B.Vertex_Index),
            Self.Vertices (Triangle.C.Vertex_Index));
      end loop;
   end Iterate_Triangles;

   procedure Dump_Info (M : Mesh) is
      use Ada.Text_IO;
   begin
      Put_Line ("Vertices  : " & M.Vertices.Length'Image);
      Put_Line ("Normals   : " & M.Normals.Length'Image);
      Put_Line ("Triangles : " & M.Triangles.Length'Image);
   end Dump_Info;
end Meshes;
