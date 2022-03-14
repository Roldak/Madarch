with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;

with Madarch.Components;
with Madarch.Entities;
with Madarch.Exprs;
with Madarch.Exprs.Derivatives;
with Madarch.Lights;
with Madarch.Lights.Point_Lights;
with Madarch.Materials;
with Madarch.Primitives;
with Madarch.Primitives.Spheres;
with Madarch.Primitives.Materials;
with Madarch.Renderers;
with Madarch.Scenes;
with Madarch.Values;
with Madarch.Windows;

with Math_Utils; use Math_Utils;
with Bounding_Boxes;
with Bounding_Volume_Hierarchies;
with Meshes.Obj_Loader;

with GL.Types;
with Glfw.Input.Keys;

procedure Main is
   use Madarch;
   use type Madarch.Exprs.Expr;
   use GL.Types;

   generic
      with package BVHs is new Bounding_Volume_Hierarchies (<>);

      with function Primitive_Distance_Expr
        (Primitive : BVHs.Primitive_Type;
         Point     : Exprs.Expr) return Exprs.Expr;

      Scene : BVHs.Primitive_Array;
   package BVH_Primitives is
      use Madarch.Primitives;
      use Madarch.Primitives.Materials;
      use Madarch.Values;
      use Madarch.Components;
      use Madarch.Exprs;

      Position : constant Component := Create ("position", Vector3_Kind);

      function Get_Distance
        (S : Struct_Expr; P : Expr'Class) return Expr'Class;

      function Get_Normal
        (S : Struct_Expr; P : Expr'Class) return Expr'Class;

      function Computed_BVH return BVHs.BVH;

      BVH : constant Primitive := Create
        ("BVH",
         (Position, Material_ID),
         Get_Distance'Unrestricted_Access,
         Get_Normal'Unrestricted_Access,
         Get_Material_Id'Access);
   end BVH_Primitives;

   package body BVH_Primitives is
      use Bounding_Boxes;
      use GL;

      package Expr_Visitors is new BVHs.Visitors (Expr);

      Zero_Vector : constant Expr := Literal (Vector3 ((0.0, 0.0, 0.0)));
      Zero_Float  : constant Expr := Literal (Values.Float (0.0));
      Threshold   : constant Expr := Literal (Values.Float (0.2));
      Q           : constant Expr := Value_Identifier ("q");

      BB_Dist_Expr : constant Expr := Abs_Value
        (Value_Identifier ("BB_Center") - Value_Identifier ("x"))."-"
           (Value_Identifier ("BB_Side")).Let_In
             (Vector3_Kind,
              "q",
              Q.Max (Zero_Vector).Length
              + Q.Get (X).Max (Q.Get (Y).Max (Q.Get (Z))).Min (Zero_Float));

      function Get_Distance
        (S : Struct_Expr; P : Expr'Class) return Expr'Class
      is
         function BB_Distance (BB : Bounding_Box) return Expr is
            use type Singles.Vector3;
            use Ada.Strings.Unbounded;

            Center : constant Expr :=
               Literal (Values.Vector3 ((BB.From + BB.To) / 2.0));

            Side   : constant Expr :=
               Literal (Values.Vector3 (BB.To - BB.From));
         begin
            return Let_In
              ((Create (Vector3_Kind, "BB_Center", Center),
                Create (Vector3_Kind, "BB_Side", Side),
                Create (Vector3_Kind, "x", P)),
               BB_Dist_Expr);
         end BB_Distance;

         function Visit_Leaf
           (BB      : Bounding_Box;
            Indices : BVHs.Index_Array) return Expr
         is
            Dist_Name   : String := Fresh_Name ("BB_Dist");
            BB_Dist     : Expr   := BB_Distance (BB);
            BB_Dist_Ref : Expr   := Value_Identifier (Dist_Name);

            Prim : constant BVHs.Primitive_Type :=
               Scene (Indices (Indices'First));
         begin
            return BB_Dist.Let_In
              (Values.Float_Kind,
               Dist_Name,
               If_Then_Else
                 (BB_Dist_Ref < Threshold,
                  Primitive_Distance_Expr (Prim, Expr (P)),
                  BB_Dist_Ref));
         end Visit_Leaf;

         function Visit_Node
           (BB    : Bounding_Box;
            Axis  : GL.Index_3D;
            Left  : BVHs.BVH;
            Right : BVHs.BVH) return Expr
         is
            L_Expr : constant Expr := Expr_Visitors.Visit
              (Left,
               Visit_Leaf'Unrestricted_Access,
               Visit_Node'Unrestricted_Access);
            R_Expr : constant Expr := Expr_Visitors.Visit
              (Right,
               Visit_Leaf'Unrestricted_Access,
               Visit_Node'Unrestricted_Access);

            Dist_Name   : String := Fresh_Name ("BB_Dist");
            BB_Dist     : Expr := BB_Distance (BB);
            BB_Dist_Ref : Expr := Value_Identifier (Dist_Name);
         begin
            return BB_Dist.Let_In
              (Values.Float_Kind,
               Dist_Name,
               If_Then_Else
                 (BB_Dist_Ref < Threshold,
                  L_Expr.Min (R_Expr),
                  BB_Dist_Ref));
         end Visit_Node;
      begin
         return Expr_Visitors.Visit
           (Computed_BVH,
            Visit_Leaf'Unrestricted_Access,
            Visit_Node'Unrestricted_Access);
      end Get_Distance;

      function Get_Normal
        (S : Struct_Expr; P : Expr'Class) return Expr'Class
      is
         Dist_Expr : constant Expr'Class :=
           Get_Distance (S, Value_Identifier ("DX"));
      begin
         return Exprs.Derivatives.Forward_Difference
           (Dist_Expr, "DX", P).Normalize;
      end Get_Normal;

      Scene_BVH : constant BVHs.BVH := BVHs.Compute_BVH (Scene);

      function Computed_BVH return BVHs.BVH is (Scene_BVH);
   end BVH_Primitives;

   function Sphere_Centroid (T : Entities.Entity) return Singles.Vector3 is
     (T.Get (Primitives.Spheres.Center).Vector3_Value);

   function Sphere_Bounding_Box
     (T : Entities.Entity) return Bounding_Boxes.Bounding_Box
   is
      use type Singles.Vector3;

      Center : constant Singles.Vector3 := Sphere_Centroid (T);
      Radius : constant Single :=
         T.Get (Primitives.Spheres.Radius).Float_Value;
      Diag   : constant Singles.Vector3 := (Radius, Radius, Radius);
   begin
      return (Center - Diag, Center + Diag);
   end Sphere_Bounding_Box;

   package Spheres_BVH is new Bounding_Volume_Hierarchies
     (Primitive_Type      => Entities.Entity,
      Primitive_Array     => Entities.Entity_Array,
      Get_Centroid        => Sphere_Centroid,
      Get_Bounding_Box    => Sphere_Bounding_Box,
      Max_Volume_Elements => 1);

   All_Spheres : Entities.Entity_Array :=
     (Primitives.Spheres.Create ((1.0, 1.0, 1.0), 0.5, 0),
      Primitives.Spheres.Create ((3.0, 1.0, 1.0), 0.5, 0),
      Primitives.Spheres.Create ((1.0, 3.0, 1.0), 0.5, 0),
      Primitives.Spheres.Create ((3.0, 3.0, 1.0), 0.5, 0),
      Primitives.Spheres.Create ((1.0, 1.0, 3.0), 0.5, 0),
      Primitives.Spheres.Create ((3.0, 1.0, 3.0), 0.5, 0),
      Primitives.Spheres.Create ((1.0, 3.0, 3.0), 0.5, 0),
      Primitives.Spheres.Create ((3.0, 3.0, 3.0), 0.5, 0),
      Primitives.Spheres.Create ((5.0, 1.0, 1.0), 0.5, 0),
      Primitives.Spheres.Create ((7.0, 1.0, 1.0), 0.5, 0),
      Primitives.Spheres.Create ((5.0, 3.0, 1.0), 0.5, 0),
      Primitives.Spheres.Create ((7.0, 3.0, 1.0), 0.5, 0),
      Primitives.Spheres.Create ((5.0, 1.0, 3.0), 0.5, 0),
      Primitives.Spheres.Create ((7.0, 1.0, 3.0), 0.5, 0),
      Primitives.Spheres.Create ((5.0, 3.0, 3.0), 0.5, 0),
      Primitives.Spheres.Create ((7.0, 3.0, 3.0), 0.5, 0));

   Sphere_Dist_Expr : constant Exprs.Expr := Exprs.Length
     (Exprs.Value_Identifier ("center") - Exprs.Value_Identifier ("x"))
     - Exprs.Value_Identifier ("radius");

   function Sphere_Distance
     (Ent : Entities.Entity; P : Exprs.Expr) return Exprs.Expr
   is
      use Values;
      use Exprs;
      use Primitives.Spheres;
   begin
      return Let_In
        ((Create (Vector3_Kind, "center", Literal (Ent.Get (Center))),
          Create (Float_Kind, "radius", Literal (Ent.Get (Radius))),
          Create (Vector3_Kind, "x", P)),
         Sphere_Dist_Expr);
   end Sphere_Distance;

   package Scene_BVH_Primitive is new BVH_Primitives
     (Spheres_BVH, Sphere_Distance, All_Spheres);

   Scene : Scenes.Scene := Scenes.Compile
     (All_Primitives => (1 => (Scene_BVH_Primitive.BVH, 1)),
      All_Lights     => (1 => (Lights.Point_Lights.Point_Light, 4)),
      Partitioning   => (Enable => False),
      Print_GLSL     => True);

   Window : Windows.Window := Windows.Open (1000, 1000, "BVH_Test");

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
begin
   Spheres_BVH.Dump (Scene_BVH_Primitive.Computed_BVH);
   Renderers.Add_Primitive
     (Renderer,
      Scene_BVH_Primitive.BVH,
      Entities.Create
        (((Scene_BVH_Primitive.Position, Values.Vector3 ((0.0, 0.0, 0.0))),
          (Primitives.Materials.Material_Id, Values.Int (0)))));

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
