with Meshes.Voxels;

package Meshes.Distance_Maps is
   type Distance_Map is array
     (Positive range <>,
      Positive range <>,
      Positive range <>) of Singles.Vector3;

   type Transformation_Algorithm is (Danielsson);

   function Build
     (Input     : Voxels.Voxelization;
      Algorithm : Transformation_Algorithm) return Distance_Map;
end Meshes.Distance_Maps;
