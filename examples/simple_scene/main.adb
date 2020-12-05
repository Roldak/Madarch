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

   Sphere_Instance : Entities.Entity :=
      Primitives.Spheres.Create ((3.5, 3.0, 3.0), 1.0, 3);

   Point_Light_Instance : Entities.Entity :=
      Lights.Point_Lights.Create ((0.0, 3.0, 0.0), (0.9, 0.9, 0.9));

   Time : Single := 0.0;

   Prim_Count : Natural := 1;

   Planes : Entities.Entity_Array :=
     (Primitives.Planes.Create ((0.0,  1.0, 0.0), 1.0, 0),
      Primitives.Planes.Create ((0.0, -1.0, 0.0), 7.0, 0),
      Primitives.Planes.Create (( 1.0, 0.0, 0.0), 1.0, 1),
      Primitives.Planes.Create ((-1.0, 0.0, 0.0), 7.0, 2),
      Primitives.Planes.Create ((0.0, 0.0,  1.0), 6.0, 0),
      Primitives.Planes.Create ((0.0, 0.0, -1.0), 7.0, 0));
begin
   for Plane of Planes loop
      Renderers.Set_Primitive (Renderer, Prim_Count, Primitives.Planes.Plane, Plane);
      Prim_Count := Prim_Count + 1;
   end loop;

   Renderers.Set_Material
     (Renderer, 1, Materials.Create ((0.0, 0.0, 0.0), 0.0, 0.6));
   Renderers.Set_Material
     (Renderer, 2, Materials.Create ((1.0, 0.0, 0.0), 0.0, 0.6));
   Renderers.Set_Material
     (Renderer, 3, Materials.Create ((0.0, 0.0, 1.0), 0.0, 0.6));
   Renderers.Set_Material
     (Renderer, 4, Materials.Create ((0.1, 0.1, 0.1), 0.9, 0.1));

   Renderers.Set_Primitive
     (Renderer, Prim_Count, Primitives.Spheres.Sphere, Sphere_Instance);

   Renderers.Set_Light (Renderer, 1, Lights.Point_Lights.Point_Light, Point_Light_Instance);

   while Window.Is_Opened loop
      Renderers.Render (Renderer);
      Window.Poll_Events;
      Time := Time + 0.01;
   end loop;
end Main;
