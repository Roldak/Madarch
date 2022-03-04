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
with Glfw.Input.Keys;

procedure Main is
   use Madarch;
   use type Madarch.Exprs.Expr;
   use GL.Types;

   Scene : Scenes.Scene := Scenes.Compile
     (All_Primitives => (1 => (Primitives.Spheres.Sphere, 20),
                         2 => (Primitives.Planes.Plane, 10),
                         3 => (Primitives.Boxes.Box, 10)),
      All_Lights     => (1 => (Lights.Point_Lights.Point_Light, 4)),
      Partitioning   => (Enable => False));

   Window : Windows.Window := Windows.Open (1000, 1000, "Light_Shafts");

   Renderer : Renderers.Renderer := Renderers.Create (Window, Scene);

   Point_Light_Instance : Entities.Entity :=
      Lights.Point_Lights.Create ((5.0, 3.0, 6.0), (0.9, 0.9, 0.9));

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
     (1 => Primitives.Spheres.Create ((3.0, 4.0, 3.0), 1.0, 3));

   Boxes : Entities.Entity_Array :=
     (1 => Primitives.Boxes.Create ((3.0, 0.0, 4.0), (1.5, 1.5, 1.5), 2));

   Camera_Position    : Singles.Vector3 := (2.0, 2.0, 0.0);
   Camera_Orientation : Singles.Matrix3 := Singles.Identity3;
   Camera_Rot_X, Camera_Rot_Y : Single := 0.0;

   procedure Move_Camera (Offset : Singles.Vector3) is
      use type Singles.Vector3;
   begin
      Camera_Position := Camera_Position + Camera_Orientation * Offset;
      Renderers.Set_Camera_Position (Renderer, Camera_Position);
   end Move_Camera;

   procedure Rotate_Camera (X, Y : Single) is
      use type Singles.Matrix3;

      Rot_Mat : Singles.Matrix3;
   begin
      Camera_Rot_X := Camera_Rot_X + X;
      Camera_Rot_Y := Camera_Rot_Y + Y;
      declare
         CX : Single := Cos (Camera_Rot_X);
         CY : Single := Cos (Camera_Rot_Y);
         SX : Single := Sin (Camera_Rot_X);
         SY : Single := Sin (Camera_Rot_Y);
      begin
         Rot_Mat := Singles.Identity3;
         Rot_Mat (GL.Y, GL.Y) := CY;
         Rot_Mat (GL.Z, GL.Z) := CY;
         Rot_Mat (GL.Z, GL.Y) := -SY;
         Rot_Mat (GL.Y, GL.Z) := SY;

         Camera_Orientation := Rot_Mat;

         Rot_Mat := Singles.Identity3;
         Rot_Mat (GL.X, GL.X) := CX;
         Rot_Mat (GL.Z, GL.Z) := CX;
         Rot_Mat (GL.Z, GL.X) := SX;
         Rot_Mat (GL.X, GL.Z) := -SX;

         Camera_Orientation := Rot_Mat * Camera_Orientation;
         Renderers.Set_Camera_Orientation (Renderer, Camera_Orientation);
      end;
   end Rotate_Camera;

   function Handle_Events return Boolean is
      use GL;

      MV : Singles.Vector3 := (0.0, 0.0, 0.0);
      DX, DY : Single;
   begin
      if Window.Key_Pressed (Glfw.Input.Keys.Escape) then
         return True;
      end if;

      if Window.Key_Pressed (Glfw.Input.Keys.S) then
         MV (Z) := MV (Z) - 0.1;
      elsif Window.Key_Pressed (Glfw.Input.Keys.W) then
         MV (Z) := MV (Z) + 0.1;
      end if;

      if Window.Key_Pressed (Glfw.Input.Keys.A) then
         MV (X) := MV (X) - 0.1;
      elsif Window.Key_Pressed (Glfw.Input.Keys.D) then
         MV (X) := MV (X) + 0.1;
      end if;

      Window.Center_Cursor (DX, DY);

      Move_Camera (MV);
      Rotate_Camera (Single (DX / 1000.0), Single (DY / 1000.0));

      return False;
   end Handle_Events;
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
     (Renderer, 0, Materials.Create ((0.0, 0.0, 0.0), 0.0, 1.0));
   Renderers.Set_Material
     (Renderer, 1, Materials.Create ((1.0, 0.0, 0.0), 0.0, 1.0));
   Renderers.Set_Material
     (Renderer, 2, Materials.Create ((0.0, 1.0, 0.0), 0.0, 1.0));
   Renderers.Set_Material
     (Renderer, 3, Materials.Create ((0.0, 0.0, 1.0), 0.0, 1.0));

   Renderers.Set_Light (Renderer, 1, Lights.Point_Lights.Point_Light, Point_Light_Instance);

   Window.Show_Cursor (False);
   while Window.Is_Opened loop
      Window.Poll_Events;
      exit when Handle_Events;

      FPS_Clock := Ada.Calendar.Clock;
      Renderers.Render (Renderer);
      Ada.Text_IO.Put_Line
        (Ada.Calendar."-"(Ada.Calendar.Clock, FPS_Clock)'Image);
      Window.Poll_Events;
      Time := Time + 0.01;
      Point_Light_Instance.Set
        (Lights.Point_Lights.Position,
         Values.Vector3
           ((4.0 + 2.0 * Cos (Time), 3.0, 4.0 + 2.0 * Sin (Time))));
      Renderers.Set_Light (Renderer, 1, Lights.Point_Lights.Point_Light, Point_Light_Instance);
   end loop;
end Main;
