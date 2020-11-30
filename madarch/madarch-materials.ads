with GL.Types;

package Madarch.Materials is
   use GL.Types;

   type Material is record
      Albedo    : Singles.Vector3;
      Metallic  : Single;
      Roughness : Single;
   end record;
end Madarch.Materials;
