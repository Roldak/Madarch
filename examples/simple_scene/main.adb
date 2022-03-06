with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;

with Madarch.Components;
with Madarch.Entities;
with Madarch.Exprs;
with Madarch.Lights;
with Madarch.Lights.Point_Lights;
with Madarch.Materials;
with Madarch.Primitives;
with Madarch.Primitives.Boxes;
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
                         2 => (Primitives.Planes.Plane, 10),
                         3 => (Primitives.Boxes.Box, 20)),
      All_Lights     => (1 => (Lights.Point_Lights.Point_Light, 4)));

   Window : Windows.Window := Windows.Open (1000, 1000, "Simple_Scene");

   Renderer : Renderers.Renderer := Renderers.Create
     (Window      => Window,
      Scene       => Scene,
      Volumetrics => Renderers.No_Volumetrics);

   Sphere_Instance : Entities.Entity :=
      Primitives.Spheres.Create ((3.5, 3.0, 3.0), 1.0, 3);

   Point_Light_Instance : Entities.Entity :=
      Lights.Point_Lights.Create ((0.0, 3.0, 0.0), (0.9, 0.9, 0.9));

   Time : Single := 0.0;

   FPS_Clock : Ada.Calendar.Time;

   Planes : Entities.Entity_Array :=
     (Primitives.Planes.Create ((0.0,  1.0, 0.0), 1.0, 0),
      Primitives.Planes.Create ((0.0, -1.0, 0.0), 7.0, 0),
      Primitives.Planes.Create (( 1.0, 0.0, 0.0), 1.0, 1),
      Primitives.Planes.Create ((-1.0, 0.0, 0.0), 7.0, 2),
      Primitives.Planes.Create ((0.0, 0.0,  1.0), 6.0, 0),
      Primitives.Planes.Create ((0.0, 0.0, -1.0), 7.0, 0));

   Spheres : Entities.Entity_Array :=
     (Primitives.Spheres.Create ((0.5, 3.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((1.5, 3.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((2.5, 3.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((3.5, 3.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((4.5, 3.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((5.5, 3.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((0.5, 0.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((1.5, 0.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((2.5, 0.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((3.5, 0.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((4.5, 0.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((5.5, 0.5, 2.0), 0.5, 3),
      Primitives.Spheres.Create ((0.5, 3.5, 5.0), 0.5, 3),
      Primitives.Spheres.Create ((1.5, 3.5, 5.0), 0.5, 3),
      Primitives.Spheres.Create ((2.5, 3.5, 5.0), 0.5, 3),
      Primitives.Spheres.Create ((3.5, 3.5, 5.0), 0.5, 3),
      Primitives.Spheres.Create ((4.5, 3.5, 5.0), 0.5, 3),
      Primitives.Spheres.Create ((5.5, 3.5, 5.0), 0.5, 3),
      Primitives.Spheres.Create ((0.5, 0.5, 5.0), 0.5, 3),
      Primitives.Spheres.Create ((1.5, 0.5, 5.0), 0.5, 3));

   Boxes : Entities.Entity_Array :=
     (Primitives.Boxes.Create ((3.0, 1.0, 2.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((0.0, 1.0, 2.0), (0.3, 0.3, 0.5), 2),
      Primitives.Boxes.Create ((3.0, 1.0, 4.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((4.0, 2.0, 2.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((2.0, 2.0, 2.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((1.0, 1.0, 6.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((3.0, 1.0, 6.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((3.0, 1.0, -2.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((1.0, 1.0, -2.0), (0.3, 0.3, 0.5), 2),
      Primitives.Boxes.Create ((3.0, 1.0, -4.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((4.0, 2.0, -2.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((2.0, 2.0, -2.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((1.0, 1.0, -6.0), (0.5, 0.5, 0.5), 2),
      Primitives.Boxes.Create ((3.0, 1.0, -6.0), (0.5, 0.5, 0.5), 2));
begin
   for Plane of Planes loop
      Renderers.Add_Primitive (Renderer, Primitives.Planes.Plane, Plane);
   end loop;

   for Sphere of Spheres loop
      Renderers.Add_Primitive (Renderer, Primitives.Spheres.Sphere, Sphere);
   end loop;

   for Box of Boxes loop
      Renderers.Add_Primitive (Renderer, Primitives.Boxes.Box, Box);
   end loop;

   Renderers.Set_Material
     (Renderer, 0, Materials.Create ((0.0, 0.0, 0.0), 0.0, 0.6));
   Renderers.Set_Material
     (Renderer, 1, Materials.Create ((1.0, 0.0, 0.0), 0.0, 0.6));
   Renderers.Set_Material
     (Renderer, 2, Materials.Create ((0.0, 0.0, 1.0), 0.0, 0.6));
   Renderers.Set_Material
     (Renderer, 3, Materials.Create ((0.1, 0.1, 0.1), 0.9, 0.1));

   Renderers.Set_Light (Renderer, 1, Lights.Point_Lights.Point_Light, Point_Light_Instance);

   Renderers.Set_Camera_Position (Renderer, (2.0, 2.0, 0.0));

   Renderers.Update_Partitioning (Renderer);

   while Window.Is_Opened loop
      FPS_Clock := Ada.Calendar.Clock;
      Renderers.Render (Renderer);
      Ada.Text_IO.Put_Line
        (Ada.Calendar."-"(Ada.Calendar.Clock, FPS_Clock)'Image);
      Window.Poll_Events;
      Time := Time + 0.01;
   end loop;
end Main;
