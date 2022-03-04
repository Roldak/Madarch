with Madarch.Components;
with Madarch.Entities;
with Madarch.Values;

with GL.Types;

package Madarch.Materials is
   use Components;
   use Values;
   use GL.Types;

   subtype Id is GL.Types.Int;

   Albedo    : constant Component := Create ("albedo", Vector3_Kind);
   Metallic  : constant Component := Create ("metallic", Float_Kind);
   Roughness : constant Component := Create ("roughness", Float_Kind);

   function Create
     (Instance_Albedo    : Singles.Vector3;
      Instance_Metallic  : Single;
      Instance_Roughness : Single) return Entities.Entity
   is (Entities.Create
     (((Albedo, Values.Vector3 (Instance_Albedo)),
       (Metallic, Values.Float (Instance_Metallic)),
       (Roughness, Values.Float (Instance_Roughness)))));
end Madarch.Materials;
