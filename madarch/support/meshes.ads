with Ada.Containers.Vectors;

with Bounding_Boxes;

with GL.Types;

package Meshes is
   use GL.Types;
   use Bounding_Boxes;

   type Mesh is private;

   function Compute_Bounding_Box (M : Mesh) return Bounding_Box;

   procedure Iterate_Triangles
     (Self    : Mesh;
      Process : access procedure (A, B, C: Singles.Vector3));

   procedure Dump_Info (M : Mesh);

private
   package Vec3_Vectors is new Ada.Containers.Vectors
     (Positive, Singles.Vector3, Singles."=");

   type Mesh_Index is record
      Vertex_Index : Natural;
      Normal_Index : Natural;
   end record;

   type Triangle is record
      A, B, C : Mesh_Index;
   end record;

   package Triangle_Vectors is new Ada.Containers.Vectors
     (Positive, Triangle);

   type Mesh is record
      Vertices  : Vec3_Vectors.Vector;
      Normals   : Vec3_Vectors.Vector;
      Triangles : Triangle_Vectors.Vector;
   end record;
end Meshes;
