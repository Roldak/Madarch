with Ada.Text_IO; use Ada.Text_IO;

with Madarch.Components;
with Madarch.Entities;
with Madarch.Exprs;
with Madarch.Lights;
with Madarch.Lights.Point_Lights;
with Madarch.Materials;
with Madarch.Primitives;
with Madarch.Primitives.Planes;
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

   Scene : Scenes.Scene := Scenes.Compile
     (All_Primitives => (1 => (Primitives.Spheres.Sphere, 20),
                         2 => (Primitives.Planes.Plane, 20)),
      All_Lights     => (1 => (Lights.Point_Lights.Point_Light, 4)));

   Window : Windows.Window := Windows.Open (1000, 1000, "Simple_Scene");

   Renderer : Renderers.Renderer := Renderers.Create (Window, Scene);

   Red_Mat : Entities.Entity :=
      Materials.Create ((0.9, 0.1, 0.1), 0.1, 0.9);

   Sphere_Instance : Entities.Entity :=
      Primitives.Spheres.Create ((1.0, 1.0, 2.0), 1.0, 0);

   Plane_Instance : Entities.Entity :=
      Primitives.Planes.Create ((0.0, 1.0, 0.0), 1.0, 0);

   Point_Light_Instance : Entities.Entity :=
      Lights.Point_Lights.Create ((0.0, 3.0, 0.0), (0.9, 0.9, 0.9));

   Time : Single := 0.0;

   Dist : Single := Primitives.Eval_Dist
     (Primitives.Spheres.Sphere, Sphere_Instance, (0.0, 0.0, 0.0));
begin
   Ada.Text_IO.Put_Line (Dist'Image);

   Renderers.Set_Material (Renderer, 1, Red_Mat);
   Renderers.Set_Primitive (Renderer, 1, Primitives.Spheres.Sphere, Sphere_Instance);
   Renderers.Set_Primitive (Renderer, 1, Primitives.Planes.Plane, Plane_Instance);
   Renderers.Set_Light (Renderer, 1, Lights.Point_Lights.Point_Light, Point_Light_Instance);
   while Window.Is_Opened loop
      Renderers.Render (Renderer);
      Window.Poll_Events;
      Time := Time + 0.01;
   end loop;
end Main;
