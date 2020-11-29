with Ada.Text_IO; use Ada.Text_IO;

with Madarch.Components;
with Madarch.Exprs;
with Madarch.Primitives;
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
     ("sphere",
      (Sphere_Center, Sphere_Radius),
      Sphere_Dist'Unrestricted_Access,
      Sphere_Normal'Unrestricted_Access);
begin
   null;
end Main;
