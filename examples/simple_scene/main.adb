with Ada.Text_IO; use Ada.Text_IO;

with Madarch.Components;
with Madarch.Exprs;
with Madarch.Lights;
with Madarch.Materials;
with Madarch.Primitives;
with Madarch.Renderers;
with Madarch.Scenes;
with Madarch.Values;
with Madarch.Windows;

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

   Window : Windows.Window := Windows.Open (1000, 1000, "Simple_Scene");

   Renderer : Renderers.Renderer := Renderers.Create (Window, Scene);

   Red_Mat : Materials.Material :=
     (Albedo    => (0.9, 0.1, 0.1),
      Metallic  => 0.1,
      Roughness => 0.9);

   Sphere_Instance : Values.Value_Array :=
     ((Values.Vector3_Kind, (0.0, 0.0, 2.0)),
      (Values.Float_Kind, 1.0));
begin
   Renderers.Set_Material (Renderer, 1, Red_Mat);
   Renderers.Set_Primitive (Renderer, 1, Sphere, Sphere_Instance, 1);
   while Window.Is_Opened loop
      Renderers.Render (Renderer);
      Window.Poll_Events;
   end loop;
end Main;
