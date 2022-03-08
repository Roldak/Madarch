with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;

with Madarch.Components;
with Madarch.Entities;
with Madarch.Exprs;
with Madarch.Lights;
with Madarch.Lights.Point_Lights;
with Madarch.Materials;
with Madarch.Primitives;
with Madarch.Primitives.Triangles;
with Madarch.Renderers;
with Madarch.Scenes;
with Madarch.Values;
with Madarch.Windows;

with Math_Utils; use Math_Utils;
with Meshes.Obj_Loader;

with GL.Types;
with Glfw.Input.Keys;

procedure Main is
   use Madarch;
   use type Madarch.Exprs.Expr;
   use GL.Types;

   Partitioning_Settings : Scenes.Partitioning_Settings :=
     (Enable          => True,
      Index_Count     => 150,
      Border_Behavior => Scenes.Clamp,
      Grid_Dimensions => (30, 20, 20),
      Grid_Spacing    => (0.1, 0.1, 0.1),
      Grid_Offset     => (0.0, 0.0, 0.0));

   Probe_Settings : Renderers.Probe_Settings :=
     (Radiance_Resolution   => 32,
      Irradiance_Resolution => 8,
      Probe_Count           => (6, 6),
      Grid_Dimensions       => (3, 3, 4),
      Grid_Spacing          => (2.0, 3.0, 3.0));

   Scene : Scenes.Scene := Scenes.Compile
     (All_Primitives => (1 => (Primitives.Triangles.Triangle, 1000)),
      All_Lights     => (1 => (Lights.Point_Lights.Point_Light, 4)),
      Partitioning   => Partitioning_Settings,
      Print_GLSL     => True);

   Window : Windows.Window := Windows.Open (1000, 1000, "Obj_Mesh");

   Renderer : Renderers.Renderer := Renderers.Create
     (Window, Scene, Volumetrics => Renderers.No_Volumetrics);

   Point_Light_Instance : Entities.Entity :=
      Lights.Point_Lights.Create ((0.0, 1.0, -5.0), (0.9, 0.9, 0.9));

   Time : Single := 0.0;

   FPS_Clock : Ada.Calendar.Time;

   Mesh_Mat : Materials.Id := Renderers.Add_Material
     (Renderer, Materials.Create ((0.8, 0.2, 0.1), 0.0, 1.0));

   Camera_Position    : Singles.Vector3 := (0.0, 1.0, -5.0);
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

   Suzanne_Offset : constant Singles.Vector3 :=
     (1.5, 1.0, 1.0);

   procedure Add_Triangle (A, B, C : Singles.Vector3) is
      use GL.Types.Singles;
   begin
      Renderers.Add_Primitive
        (Renderer,
         Primitives.Triangles.Triangle,
         Primitives.Triangles.Create
           (A + Suzanne_Offset,
            B + Suzanne_Offset,
            C + Suzanne_Offset,
            Mesh_Mat));
   end Add_Triangle;

   function Image (V : Singles.Vector3) return String is
     ("("    & Single'Image (V (GL.X))
      & ", " & Single'Image (V (GL.Y))
      & ", " & Single'Image (V (GL.Z)) & ")");

   Suzanne_Mesh : Meshes.Mesh :=
      Meshes.Obj_Loader.Load_Obj_File ("media/suzanne.obj");

   Suzanne_BB : Meshes.Bounding_Box :=
      Meshes.Compute_Bounding_Box (Suzanne_Mesh);
begin
   Meshes.Dump_Info (Suzanne_Mesh);
   Meshes.Iterate_Triangles (Suzanne_Mesh, Add_Triangle'Access);

   Put_Line (Image (Suzanne_BB.From));
   Put_Line (Image (Suzanne_BB.To));

   Renderers.Update_Partitioning (Renderer, Method => Renderers.GPU_Fast);
   Renderers.Set_Light
     (Renderer, 1, Lights.Point_Lights.Point_Light, Point_Light_Instance);

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
   end loop;
end Main;
