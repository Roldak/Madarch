package Meshes.Voxels is
   type Grid_Frequency is record
      Width  : Positive;
      Height : Positive;
      Depth  : Positive;
   end record;

   type Voxelization is array
     (Positive range <>,
      Positive range <>,
      Positive range <>) of Boolean;

   function Voxelize
     (Target : Mesh;
      Bounds : Bounding_Box;
      Freq   : Grid_Frequency) return Voxelization;
end Meshes.Voxels;
