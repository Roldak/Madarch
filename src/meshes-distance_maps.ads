with Meshes.Voxels;

package Meshes.Distance_Maps is
   type Distance_Map is array
     (Positive range <>,
      Positive range <>,
      Positive range <>) of Single
      with Convention => C;

   type Transformation_Algorithm is (Danielsson);

   function Build_From_Voxelization
     (Input     : Voxels.Voxelization;
      Algorithm : Transformation_Algorithm) return Distance_Map;
end Meshes.Distance_Maps;
