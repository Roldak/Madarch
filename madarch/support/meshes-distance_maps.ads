with GL.Objects.Textures;

with Meshes.Voxels;

package Meshes.Distance_Maps is
   type Distance_Map is array
     (Positive range <>,
      Positive range <>,
      Positive range <>) of Single
      with Convention => C;

   type Normal_Map is array
     (Positive range <>,
      Positive range <>,
      Positive range <>) of Singles.Vector3
      with Convention => C;

   type Transformation_Algorithm is (Danielsson);

   function Build_From_Voxelization
     (Input     : Voxels.Voxelization;
      Algorithm : Transformation_Algorithm) return Distance_Map;

   procedure Build_From_Mesh
     (Input   : Mesh;
      Bounds  : Bounding_Box;
      Dists   : in out Distance_Map;
      Normals : in out Normal_Map);

   procedure Load_To_Texture (Map : Distance_Map);
   procedure Load_To_Texture (Map : Normal_Map);
end Meshes.Distance_Maps;
