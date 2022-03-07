with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;

with Madarch.Components;
with Madarch.Entities;
with Madarch.Exprs;
with Madarch.Lights;
with Madarch.Lights.Spot_Lights;
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
      All_Lights     => (1 => (Lights.Spot_Lights.Spot_Light, 4)),
      Print_GLSL     => True);

   Window : Windows.Window := Windows.Open (1000, 1000, "Ball_Game");

   Renderer : Renderers.Renderer := Renderers.Create
     (Window, Scene, Volumetrics => Renderers.No_Volumetrics);

   Spot_Light_Instance : Entities.Entity := Lights.Spot_Lights.Create
     ((3.5, 6.0, 2.0),
      (0.0, -1.0, 0.0),
      3.1415 / 2.0,
      (1.0, 1.0, 1.0));

   Time : Single := 0.0;

   FPS_Clock : Ada.Calendar.Time;

   Wall_Mat_1 : Materials.Id := Renderers.Add_Material
     (Renderer, Materials.Create ((0.0, 0.0, 0.0), 0.0, 1.0));
   Wall_Mat_2 : Materials.Id := Renderers.Add_Material
     (Renderer, Materials.Create ((1.0, 0.0, 0.0), 0.0, 1.0));
   Wall_Mat_3 : Materials.Id := Renderers.Add_Material
     (Renderer, Materials.Create ((0.0, 0.0, 1.0), 0.0, 1.0));
   Sphere_Mat : Materials.Id := Renderers.Add_Material
     (Renderer, Materials.Create ((0.1, 0.1, 0.1), 0.9, 0.1));
   Box_Mat    : Materials.Id := Renderers.Add_Material
     (Renderer, Materials.Create ((0.0, 1.0, 0.0), 0.8, 0.3));

   Planes : Entities.Entity_Array :=
     (Primitives.Planes.Create ((0.0,  1.0, 0.0), 1.0, Wall_Mat_1),
      Primitives.Planes.Create ((0.0, -1.0, 0.0), 7.0, Wall_Mat_1),
      Primitives.Planes.Create (( 1.0, 0.0, 0.0), 1.0, Wall_Mat_2),
      Primitives.Planes.Create ((-1.0, 0.0, 0.0), 7.0, Wall_Mat_3),
      Primitives.Planes.Create ((0.0, 0.0,  1.0), 6.0, Wall_Mat_1),
      Primitives.Planes.Create ((0.0, 0.0, -1.0), 7.0, Wall_Mat_1));

   Spheres : Entities.Entity_Array :=
     (1 => Primitives.Spheres.Create ((3.0, 4.0, 3.0), 1.0, Sphere_Mat));

   Boxes : Entities.Entity_Array :=
     (1 => Primitives.Boxes.Create ((3.0, 0.0, 4.0), (1.5, 1.5, 1.5), Box_Mat));

   Camera_Position    : Singles.Vector3 := (2.0, 2.0, 0.0);
   Camera_Orientation : Singles.Matrix3 := Singles.Identity3;
   Camera_Rot_X, Camera_Rot_Y : Single := 0.0;

   type Ball_Rigid_Body is record
      Index       : Positive;
      Position    : Singles.Vector3;
      Velocity    : Singles.Vector3;
   end record;

   package Ball_RB_Vectors is new Ada.Containers.Vectors
     (Positive, Ball_Rigid_Body);

   Ball_Bodies : Ball_RB_Vectors.Vector;
   Ball_Radius : constant Single := 0.2;

   function Ball_Entity (Position : Singles.Vector3) return Entities.Entity is
     (Primitives.Spheres.Create (Position, Ball_Radius, Box_Mat));

   procedure Throw_Ball is
      use Singles;

      Ball : Entities.Entity := Ball_Entity (Camera_Position);
      Vel : Singles.Vector3 := Camera_Orientation * (0.0, 0.0, 1.0) * 10.0;
   begin
      Renderers.Add_Primitive (Renderer, Primitives.Spheres.Sphere, Ball);
      Ball_Bodies.Append ((Index    => Natural (Ball_Bodies.Length) + 2,
                           Position => Camera_Position,
                           Velocity => Vel));
   end Throw_Ball;

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

   Space_Pressed : Boolean := False;

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

      if Window.Key_Pressed (Glfw.Input.Keys.Space) then
         Space_Pressed := True;
      else
         if Space_Pressed then
            Throw_Ball;
         end if;
         Space_Pressed := False;
      end if;

      Window.Center_Cursor (DX, DY);

      Move_Camera (MV);
      Rotate_Camera (Single (DX / 1000.0), Single (DY / 1000.0));

      return False;
   end Handle_Events;

   Gravity : constant Singles.Vector3 := (0.0, -9.81, 0.0);

   procedure Step_Physics (Dt : Single) is
      use GL.Types;
      use type GL.Types.Singles.Vector3;
   begin
      for Ball_Body of Ball_Bodies loop
         declare
            New_Vel  : Singles.Vector3 := Ball_Body.Velocity + Gravity * Dt;
            New_Pos  : Singles.Vector3 := Ball_Body.Position + New_Vel * Dt;

            Normal : Singles.Vector3;
            Dist : Single := Renderers.Eval_Distance_To
              (Renderer,
               New_Pos,
               (Primitives.Planes.Plane, Primitives.Boxes.Box),
               Normal);
         begin
            if Dist <= Ball_Radius then
               New_Vel := Reflect (New_Vel, Normal);
               New_Pos := Ball_Body.Position;
            end if;

            Renderers.Set_Primitive
              (Renderer,
               Primitives.Spheres.Sphere,
               Ball_Body.Index,
               Ball_Entity (New_Pos));

            Ball_Body.Velocity := New_Vel;
            Ball_Body.Position := New_Pos;
         end;
      end loop;
   end Step_Physics;
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

   Renderers.Set_Light (Renderer, 1, Lights.Spot_Lights.Spot_Light, Spot_Light_Instance);

   Window.Show_Cursor (False);
   while Window.Is_Opened loop
      Window.Poll_Events;
      exit when Handle_Events;

      Step_Physics (0.01);

      FPS_Clock := Ada.Calendar.Clock;
      Renderers.Update_Partitioning (Renderer);
      Renderers.Render (Renderer);
      Ada.Text_IO.Put_Line
        (Ada.Calendar."-"(Ada.Calendar.Clock, FPS_Clock)'Image);
      Window.Poll_Events;
      Time := Time + 0.01;
   end loop;
end Main;
