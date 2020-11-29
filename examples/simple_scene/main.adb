with Ada.Text_IO; use Ada.Text_IO;

with Madarch.Components;
with Madarch.Exprs;
with Madarch.Lights;
with Madarch.Primitives;
with Madarch.Scenes;
with Madarch.Values;

procedure Main is
   use Madarch;
   use type Madarch.Exprs.Expr;

   Sphere_Center : Components.Component := Components.Create
     ("center", Values.Vector3_Kind);
   Sphere_Radius : Components.Component := Components.Create
     ("radius", Values.Float_Kind);

   function Sphere_Dist
     (S : Exprs.Struct_Expr; P : Exprs.Expr'Class) return Exprs.Expr'Class
   is (S.Get (Sphere_Center)."-" (P).Length - S.Get (Sphere_Radius));

   function Sphere_Normal
     (S : Exprs.Struct_Expr; P : Exprs.Expr'Class) return Exprs.Expr'Class
   is (P."-" (S.Get (Sphere_Center)).Normalize);

   Sphere : Primitives.Primitive := Primitives.Create
     ("Sphere",
      (Sphere_Center, Sphere_Radius),
      Sphere_Dist'Unrestricted_Access,
      Sphere_Normal'Unrestricted_Access);

   Light_Color : Components.Component := Components.Create
     ("color", Values.Vector3_Kind);

   function Sample_Point_Light
     (L : Exprs.Struct_Expr; P : Exprs.Expr'Class; N : Exprs.Expr'Class)
      return Exprs.Expr'Class
   is (L.Get (Light_Color));

   Point_Light : Lights.Light := Lights.Create
     ("PointLight",
      (1 => Light_Color),
      Sample_Point_Light'Unrestricted_Access);

   Scene : Scenes.Scene := Scenes.Compile
     (All_Primitives => (1 => (Sphere, 20)),
      All_Lights     => (1 => (Point_Light, 4)));
begin
   Scenes.Print_GLSL (Scene);
end Main;
