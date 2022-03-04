with GL.Types;

with Madarch.Primitives.Materials;
with Madarch.Values;
with Madarch.Entities;

package Madarch.Lights.Spot_Lights is
   use Components;
   use Exprs;
   use Values;

   use GL.Types;

   Position  : constant Component := Create ("position", Vector3_Kind);
   Direction : constant Component := Create ("direction", Vector3_Kind);
   Aperture  : constant Component := Create ("aperture", Float_Kind);
   Color     : constant Component := Create ("color", Vector3_Kind);

   function Get_Position (L : Struct_Expr) return Expr'Class is
     (L.Get (Position));

   function Get_Color
     (L : Struct_Expr; Pos, Normal, Dir, Dist : Expr'Class) return Expr'Class;

   Spot_Light : constant Light := Create
     ("SpotLight",
      (Position, Direction, Aperture, Color),
      Get_Color'Access,
      Get_Position'Access);

   function Create
     (Instance_Position  : Singles.Vector3;
      Instance_Direction : Singles.Vector3;
      Instance_Aperture  : Single;
      Instance_Color     : Singles.Vector3) return Entities.Entity
   is (Entities.Create
     (((Position,  Values.Vector3 (Instance_Position)),
       (Direction, Values.Vector3 (Instance_Direction)),
       (Aperture,  Values.Float (Instance_Aperture)),
       (Color,     Values.Vector3 (Instance_Color)))));
end Madarch.Lights.Spot_Lights;

