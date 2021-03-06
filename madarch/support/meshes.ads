with Ada.Containers.Vectors;

with GL.Types;

package Meshes is
   use GL.Types;

   type Mesh is private;

   type Bounding_Box is record
      From, To : Singles.Vector3;
   end record;

   function Compute_Bounding_Box (M : Mesh) return Bounding_Box;

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
