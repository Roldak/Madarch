with GL.Types;

package Materials is
   use GL.Types;

   type Material is record
      Albedo    : Singles.Vector3;
      Metallic  : Single;
      Roughness : Single;
   end record;

   type Material_Array is array (Positive range <>) of Material;
end Materials;
