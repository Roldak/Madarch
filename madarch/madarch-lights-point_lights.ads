with GL.Types;

with Madarch.Primitives.Materials;
with Madarch.Values;
with Madarch.Entities;

package Madarch.Lights.Point_Lights is
   use Components;
   use Exprs;
   use Values;

   use GL.Types;

   Position : constant Component := Create ("position", Vector3_Kind);
   Color    : constant Component := Create ("color", Vector3_Kind);

   function Get_Position (L : Struct_Expr) return Expr'Class is
     (L.Get (Position));

   function Get_Color
     (L : Struct_Expr; P : Expr'Class; N : Expr'Class) return Expr'Class
   is (L.Get (Color));

   Point_Light : constant Light := Create
     ("PointLight",
      (Position, Color),
      Get_Color'Access,
      Get_Position'Access);

   function Create
     (Instance_Position : Singles.Vector3;
      Instance_Color    : Singles.Vector3) return Entities.Entity
   is (Entities.Create
     (((Position, Values.Vector3 (Instance_Position)),
       (Color,    Values.Vector3 (Instance_Color)))));
end Madarch.Lights.Point_Lights;
