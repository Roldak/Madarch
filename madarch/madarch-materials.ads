with Madarch.Components;
with Madarch.Entities;
with Madarch.Values;

with GL.Types;

package Madarch.Materials is
   use GL.Types;

   Albedo    : constant Components.Component;
   Metallic  : constant Components.Component;
   Roughness : constant Components.Component;

private

   Albedo    : constant Components.Component :=
      Components.Create ("albedo", Values.Vector3_Kind);

   Metallic  : constant Components.Component :=
      Components.Create ("metallic", Values.Float_Kind);

   Roughness : constant Components.Component :=
      Components.Create ("roughness", Values.Float_Kind);
end Madarch.Materials;
