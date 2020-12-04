with Ada.Text_IO; use Ada.Text_IO;

with Madarch.Components;
with Madarch.Entities;
with Madarch.Exprs;
with Madarch.Lights;
with Madarch.Materials;
with Madarch.Primitives;
with Madarch.Primitives.Spheres;
with Madarch.Renderers;
with Madarch.Scenes;
with Madarch.Values;
with Madarch.Windows;

with Math_Utils; use Math_Utils;

with GL.Types;

procedure Main is
   use Madarch;
   use type Madarch.Exprs.Expr;
   use GL.Types;

   Light_Position : Components.Component := Components.Create
     ("position", Values.Vector3_Kind);

   Light_Color : Components.Component := Components.Create
     ("color", Values.Vector3_Kind);

   function Point_Light_Position
     (L : Exprs.Struct_Expr) return Exprs.Expr'Class
   is (L.Get (Light_Position));

   function Sample_Point_Light
     (L : Exprs.Struct_Expr; P : Exprs.Expr'Class; N : Exprs.Expr'Class)
      return Exprs.Expr'Class
   is (L.Get (Light_Color));

   Point_Light : Lights.Light := Lights.Create
     ("PointLight",
      (Light_Position, Light_Color),
      Sample_Point_Light'Unrestricted_Access,
      Point_Light_Position'Unrestricted_Access);

   Scene : Scenes.Scene := Scenes.Compile
     (All_Primitives => (1 => (Primitives.Spheres.Sphere, 20)),
      All_Lights     => (1 => (Point_Light, 4)));

   Window : Windows.Window := Windows.Open (1000, 1000, "Simple_Scene");

   Renderer : Renderers.Renderer := Renderers.Create (Window, Scene);

   Red_Mat : Entities.Entity := Entities.Create
     (((Materials.Albedo, Values.Vector3 ((0.9, 0.1, 0.1))),
       (Materials.Metallic, Values.Float (0.1)),
       (Materials.Roughness, Values.Float (0.9))));

   Sphere_Instance : Entities.Entity :=
      Primitives.Spheres.Create ((1.0, 1.0, 2.0), 1.0, 0);

   Point_Light_Instance : Entities.Entity := Entities.Create
     (((Light_Color, Values.Vector3 ((0.9, 0.9, 0.9))),
       (Light_Position, Values.Vector3 ((0.0, 5.0, 0.0)))));

   Time : Single := 0.0;

   Dist : Single := Primitives.Eval_Dist
     (Primitives.Spheres.Sphere, Sphere_Instance, (0.0, 0.0, 0.0));
begin
   Ada.Text_IO.Put_Line (Dist'Image);

   Renderers.Set_Material (Renderer, 1, Red_Mat);
   Renderers.Set_Primitive (Renderer, 1, Primitives.Spheres.Sphere, Sphere_Instance);
   Renderers.Set_Light (Renderer, 1, Point_Light, Point_Light_Instance);
   while Window.Is_Opened loop
      Renderers.Render (Renderer);
      Window.Poll_Events;
      Time := Time + 0.01;
   end loop;
end Main;
