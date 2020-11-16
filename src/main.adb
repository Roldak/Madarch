with Ada.Text_IO;
with Ada.Calendar;

with Interfaces.C.Pointers;

with GNATCOLL.Strings;

with GL.Buffers;
with GL.Files;
with GL.Fixed.Matrix;
with GL.Immediate;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Shaders.Lists;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;
with GLFW_Utils;

with Glfw.Input.Keys;

with Materials;
with Math_Utils; use Math_Utils;
with Lights;
with Primitives;
with Shader_Loader; use Shader_Loader;
with UBOs;
with GPU_Types.Base;
with GPU_Types.Structs;
with GPU_Types.Fixed_Arrays;
with Meshes.Obj_Loader;
with Meshes.Voxels;
with Meshes.Distance_Maps;

procedure Main is
   use GNATCOLL;

   use GL;
   use GL.Buffers;
   use GL.Types;
   use GL.Fixed.Matrix;
   use GL.Immediate;

   use GPU_Types;

   procedure Draw_Fullscreen_Quad is
      Token : Input_Token := Start (Quads);
   begin
      Set_Color (Colors.Color'(1.0, 0.0, 0.0, 0.0));
      Token.Add_Vertex (Doubles.Vector4'(1.0, 1.0, 0.0, 1.0));
      Set_Color (Colors.Color'(0.0, 1.0, 0.0, 0.0));
      Token.Add_Vertex (Doubles.Vector4'(1.0, -1.0, 0.0, 1.0));
      Set_Color (Colors.Color'(0.0, 0.0, 1.0, 0.0));
      Token.Add_Vertex (Doubles.Vector4'(-1.0, -1.0, 0.0, 1.0));
      Set_Color (Colors.Color'(1.0, 0.0, 1.0, 0.0));
      Token.Add_Vertex (Doubles.Vector4'(-1.0, 1.0, 0.0, 1.0));
   end Draw_Fullscreen_Quad;

   Time            : GL.Types.Single := 0.0;
   Cam_Pos         : Singles.Vector3 := (1.0, 2.0, -4.0);

   All_Lights : Lights.Light_Array :=
     ((Lights.Point, (0.9, 0.9, 0.8), (4.0, 2.0, 0.0)),
      (Kind     => Lights.Spot,
       Light_Color => (0.9, 0.9, 0.8),
       Spot_Light_Pos => (3.5, 5.0, 2.0),
       Spot_Light_Dir => (1.0, 0.0, 0.0),
       Spot_Light_Aperture => 3.1415 / 4.0));

   Controlled_Light : Lights.Light renames All_Lights (1);
   Spot_Light : Lights.Light renames All_Lights (2);

   function Handle_Events return Boolean is
   begin
      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.Escape) then
         return True;
      end if;

      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.S) then
         Cam_Pos (Z) := Cam_Pos (Z) - 0.1;
      elsif GLFW_Utils.Key_Pressed (Glfw.Input.Keys.W) then
         Cam_Pos (Z) := Cam_Pos (Z) + 0.1;
      end if;

      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.A) then
         Cam_Pos (X) := Cam_Pos (X) - 0.1;
      elsif GLFW_Utils.Key_Pressed (Glfw.Input.Keys.D) then
         Cam_Pos (X) := Cam_Pos (X) + 0.1;
      end if;

      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.K) then
         Controlled_Light.Point_Light_Pos (Z) :=
            Controlled_Light.Point_Light_Pos (Z) - 0.1;
      elsif GLFW_Utils.Key_Pressed (Glfw.Input.Keys.I) then
         Controlled_Light.Point_Light_Pos (Z) :=
            Controlled_Light.Point_Light_Pos (Z) + 0.1;
      end if;

      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.J) then
         Controlled_Light.Point_Light_Pos (X) :=
            Controlled_Light.Point_Light_Pos (X) - 0.1;
      elsif GLFW_Utils.Key_Pressed (Glfw.Input.Keys.L) then
         Controlled_Light.Point_Light_Pos (X) :=
            Controlled_Light.Point_Light_Pos (X) + 0.1;
      end if;

      return False;
   end Handle_Events;

   Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Image_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Radiance_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Irradiance_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);

   Radiance_Program : GL.Objects.Programs.Program;
   Rnd_Uniform      : GL.Uniforms.Uniform;

   Radiance_Data : GL.Objects.Textures.Texture;
   Radiance_FB   : GL.Objects.Framebuffers.Framebuffer;

   Probe_Radiance_Resolution   : constant GL.Types.Int := 64;

   Probe_Count_X    : constant GL.Types.Int := 6;
   Probe_Count_Y    : constant GL.Types.Int := 6;

   Irradiance_Data : GL.Objects.Textures.Texture;

   procedure Prepare_Radiance is
      use GL.Objects.Textures.Targets;
      use type GL.Objects.Framebuffers.Framebuffer_Status;
   begin
      Radiance_Data.Initialize_Id;
      Radiance_Fb.Initialize_Id;

      Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (Radiance_Data);
      Texture_2D.Set_Lowest_Mipmap_Level (0);
      Texture_2D.Set_Highest_Mipmap_Level
        (Int (Log_2 (Interfaces.Unsigned_64 (Probe_Radiance_Resolution))));
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear_Mipmap_Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Load_Empty_Texture
        (0, GL.Pixels.RGB8,
         Probe_Radiance_Resolution * Probe_Count_X,
         Probe_Radiance_Resolution * Probe_Count_Y);
      Texture_2D.Generate_Mipmap;

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Radiance_FB);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Attach_Texture
        (GL.Objects.Framebuffers.Color_Attachment_0, Radiance_Data, 0);

      if GL.Objects.Framebuffers.Read_And_Draw_Target.Status
            /= GL.Objects.Framebuffers.Complete
      then
         Ada.Text_IO.Put_Line
           ("Error: Framebuffer status is " &
              GL.Objects.Framebuffers.Read_And_Draw_Target.Status'Img);
         raise Program_Error;
      end if;

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);

      -- set up program
      Radiance_Program.Initialize_Id;
      Radiance_Program.Attach (Vertex_Shader);
      Radiance_Program.Attach (Radiance_Shader);
      Radiance_Program.Link;
      if not Radiance_Program.Link_Status then
         Ada.Text_IO.Put_Line ("Radiance program linking failed. Log:");
         Ada.Text_IO.Put_Line (Radiance_Program.Info_Log);
         return;
      end if;

      Rnd_Uniform := GL.Objects.Programs.Uniform_Location (Radiance_Program, "time");
   end Prepare_Radiance;

   procedure Compute_Radiance is
      use GL.Objects.Textures.Targets;
   begin
      Radiance_Program.Use_Program;
      GL.Uniforms.Set_Single (Rnd_Uniform, Time);

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Radiance_FB);

      GL.Window.Set_Viewport
        (0, 0,
         Probe_Radiance_Resolution * Probe_Count_X,
         Probe_Radiance_Resolution * Probe_Count_Y);

      Clear (Buffer_Bits'(Color => True, others => False));
      Draw_Fullscreen_Quad;

      Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (Radiance_Data);
      Texture_2D.Generate_Mipmap;

   end Compute_Radiance;

   Irradiance_Program : GL.Objects.Programs.Program;
   Irradiance_FB   : GL.Objects.Framebuffers.Framebuffer;

   Probe_Irradiance_Resolution : constant GL.Types.Int := 8;

   procedure Prepare_Irradiance is
      use GL.Objects.Textures.Targets;
      use type GL.Objects.Framebuffers.Framebuffer_Status;
   begin
      Irradiance_Data.Initialize_Id;
      Irradiance_Fb.Initialize_Id;

      Objects.Textures.Set_Active_Unit (1);
      Texture_2D.Bind (Irradiance_Data);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Load_Empty_Texture
        (0, GL.Pixels.RGB8,
         Probe_Irradiance_Resolution * Probe_Count_X,
         Probe_Irradiance_Resolution * Probe_Count_Y);

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Irradiance_FB);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Attach_Texture
        (GL.Objects.Framebuffers.Color_Attachment_0, Irradiance_Data, 0);

      if GL.Objects.Framebuffers.Read_And_Draw_Target.Status
            /= GL.Objects.Framebuffers.Complete
      then
         Ada.Text_IO.Put_Line
           ("Error: Framebuffer status is " &
              GL.Objects.Framebuffers.Read_And_Draw_Target.Status'Img);
         raise Program_Error;
         return;
      end if;

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);

      -- set up program
      Irradiance_Program.Initialize_Id;
      Irradiance_Program.Attach (Vertex_Shader);
      Irradiance_Program.Attach (Irradiance_Shader);
      Irradiance_Program.Link;
      if not Irradiance_Program.Link_Status then
         Ada.Text_IO.Put_Line ("Irradiance program linking failed. Log:");
         Ada.Text_IO.Put_Line (Irradiance_Program.Info_Log);
         return;
      end if;
   end Prepare_Irradiance;

   procedure Update_Irradiance is
      use GL.Objects.Textures.Targets;
   begin
      Irradiance_Program.Use_Program;

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Irradiance_FB);

      GL.Window.Set_Viewport
        (0, 0,
         Probe_Irradiance_Resolution * Probe_Count_X,
         Probe_Irradiance_Resolution * Probe_Count_Y);

      Clear (Buffer_Bits'(Color => True, others => False));
      Draw_Fullscreen_Quad;
   end Update_Irradiance;

   Image_Program   : GL.Objects.Programs.Program;
   Time_Uniform    : GL.Uniforms.Uniform;
   Cam_Pos_Uniform : GL.Uniforms.Uniform;

   procedure Prepare_Image is
   begin
      -- set up program
      Image_Program.Initialize_Id;
      Image_Program.Attach (Vertex_Shader);
      Image_Program.Attach (Image_Shader);
      Image_Program.Link;
      if not Image_Program.Link_Status then
         Ada.Text_IO.Put_Line ("Image program linking failed. Log:");
         Ada.Text_IO.Put_Line (Image_Program.Info_Log);
         return;
      end if;

      Time_Uniform :=
         GL.Objects.Programs.Uniform_Location (Image_Program, "time");

      Cam_Pos_Uniform :=
         GL.Objects.Programs.Uniform_Location (Image_Program, "camera_position");
   end Prepare_Image;

   procedure Draw_Image is
      use GL.Objects.Textures.Targets;
   begin
      Image_Program.Use_Program;
      GL.Uniforms.Set_Single (Time_Uniform, Time);
      GL.Uniforms.Set_Single (Cam_Pos_Uniform, Cam_Pos);

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);

      GL.Window.Set_Viewport (0, 0, 1000, 1000);
      Clear (Buffer_Bits'(Color => True, others => False));
      Draw_Fullscreen_Quad;

      GL.Flush;
      GLFW_Utils.Swap_Buffers;
   end Draw_Image;

   Probes_UBO : UBOs.UBO;

   Probes_Layout_Type : Structs.Struct := Structs.Create
     ((Base.IVec_2.Named ("probe_count"),
       Base.IVec_3.Named ("grid_dimensions"),
       Base.Vec_3.Named ("grid_spacing")));

   procedure Setup_Probe_Layout (X, Y, Z : Int; SX, SY, SZ : Single) is
      W : UBOs.Writer := UBOs.Start (Probes_UBO);

      L : Locations.Location := Probes_Layout_Type.Address;
   begin
      if X * Y * Z /= Probe_Count_X * Probe_Count_Y then
         raise Program_Error with "Probe_Count should match grid dimensions.";
      end if;

      L.Component ("probe_count").Adjust (W);
      W.Write_Int (Probe_Count_X);
      W.Write_Int (Probe_Count_Y);

      L.Component ("grid_dimensions").Adjust (W);
      W.Write_Int (X);
      W.Write_Int (Y);
      W.Write_Int (Z);

      L.Component ("grid_spacing").Adjust (W);
      W.Write_Float (SX);
      W.Write_Float (SY);
      W.Write_Float (SZ);
   end Setup_Probe_Layout;

   Scene_UBO  : UBOs.UBO;

   Max_Sphere_Count      : constant Types.Size := 40;
   Max_Plane_Count       : constant Types.Size := 40;
   Max_Cube_Count        : constant Types.Size := 40;
   Max_Point_Light_Count : constant Types.Size := 4;
   Max_Spot_Light_Count  : constant Types.Size := 4;

   Primitive_Type : aliased constant Structs.Struct :=
      Structs.Create
        ((Base.Vec_3.Named ("f_vec3"), Base.Float.Named ("f_float"),
          Base.Int.Named ("material_id")));

   Point_Light_Type : aliased constant Structs.Struct :=
      Structs.Create
        ((Base.Vec_3.Named ("position"), Base.Vec_3.Named ("color")));

   Spot_Light_Type : aliased constant Structs.Struct :=
      Structs.Create
        ((Base.Vec_3.Named ("position"), Base.Vec_3.Named ("direction"),
          Base.Float.Named ("aperture"), Base.Vec_3.Named ("color")));

   Primitive_Sphere_Array_Type : aliased constant Fixed_Arrays.Fixed_Array :=
      Fixed_Arrays.Create
        (Max_Sphere_Count, Primitive_Type'Unchecked_Access);

   Primitive_Plane_Array_Type : aliased constant Fixed_Arrays.Fixed_Array :=
      Fixed_Arrays.Create
        (Max_Plane_Count, Primitive_Type'Unchecked_Access);

   Primitive_Cube_Array_Type : aliased constant Fixed_Arrays.Fixed_Array :=
      Fixed_Arrays.Create
        (Max_Cube_Count, Primitive_Type'Unchecked_Access);

   Point_Light_Array_Type : aliased constant Fixed_Arrays.Fixed_Array :=
      Fixed_Arrays.Create
        (Max_Point_Light_Count, Point_Light_Type'Unchecked_Access);

   Spot_Light_Array_Type : aliased constant Fixed_Arrays.Fixed_Array :=
      Fixed_Arrays.Create
        (Max_Spot_Light_Count, Spot_Light_Type'Unchecked_Access);

   Scene_Description_Type : Structs.Struct := Structs.Create
     ((Base.Int.Named ("prim_sphere_count"),
       Primitive_Sphere_Array_Type.Named ("prim_spheres"),

       Base.Int.Named ("prim_plane_count"),
       Primitive_Plane_Array_Type.Named ("prim_planes"),

       Base.Int.Named ("prim_cube_count"),
       Primitive_Cube_Array_Type.Named ("prim_cubes"),

       Base.Int.Named ("point_light_count"),
       Point_Light_Array_Type.Named ("point_lights"),

       Base.Int.Named ("spot_light_count"),
       Spot_Light_Array_Type.Named ("spot_lights"),

       Base.Int.Named ("light_count")));

   procedure Update_Scene_Primitives
     (Prims : Primitives.Primitive_Array)
   is
      W : UBOs.Writer := UBOs.Start (Scene_UBO);

      Sphere_Index : Positive := 1;
      Plane_Index  : Positive := 1;
      Cube_Index   : Positive := 1;

      L : Locations.Location := Scene_Description_Type.Address;

      procedure Write_Sphere (X : Primitives.Primitive) is
      begin
         L.Component ("prim_spheres").Component (Sphere_Index).Adjust (W);

         W.Write_Vec3 (X.Sphere_Center);
         W.Write_Float (X.Sphere_Radius);
         W.Write_Int (X.Material);
         Sphere_Index := Sphere_Index + 1;
      end Write_Sphere;

      procedure Write_Plane (X : Primitives.Primitive) is
      begin
         L.Component ("prim_planes").Component (Plane_Index).Adjust (W);

         W.Write_Vec3 (X.Plane_Normal);
         W.Write_Float (X.Plane_Offset);
         W.Write_Int (X.Material);
         Plane_Index := Plane_Index + 1;
      end Write_Plane;

      procedure Write_Cube (X : Primitives.Primitive) is
      begin
         L.Component ("prim_cubes").Component (Cube_Index).Adjust (W);

         W.Write_Vec3 (X.Cube_Center);
         W.Write_Float (X.Cube_Side);
         W.Write_Int (X.Material);
         Cube_Index := Cube_Index + 1;
      end Write_Cube;
   begin
      for Prim of Prims loop
         case Prim.Kind is
            when Primitives.Sphere =>
               Write_Sphere (Prim);
            when Primitives.Plane =>
               Write_Plane (Prim);
            when Primitives.Cube =>
               Write_Cube (Prim);
         end case;
      end loop;

      L.Component ("prim_sphere_count").Adjust (W);
      W.Write_Int (Int (Sphere_Index) - 1);

      L.Component ("prim_plane_count").Adjust (W);
      W.Write_Int (Int (Plane_Index) - 1);

      L.Component ("prim_cube_count").Adjust (W);
      W.Write_Int (Int (Cube_Index) - 1);
   end Update_Scene_Primitives;

   procedure Update_Scene_Lights (Lits : Lights.Light_Array) is
      W : UBOs.Writer := UBOs.Start (Scene_UBO);

      Point_Light_Index : Positive := 1;
      Spot_Light_Index  : Positive := 1;

      L : Locations.Location := Scene_Description_Type.Address;

      procedure Write_Point_Light (X : Lights.Light) is
      begin
         L.Component ("point_lights").Component (Point_Light_Index).Adjust (W);

         W.Write_Vec3 (X.Point_Light_Pos);
         W.Write_Vec3 (X.Light_Color);

         Point_Light_Index := Point_Light_Index + 1;
      end Write_Point_Light;

      procedure Write_Spot_Light (X : Lights.Light) is
      begin
         L.Component ("spot_lights").Component (Spot_Light_Index).Adjust (W);

         W.Write_Vec3  (X.Spot_Light_Pos);
         W.Write_Vec3  (X.Spot_Light_Dir);
         W.Write_Float (X.Spot_Light_Aperture);
         W.Write_Vec3  (X.Light_Color);

         Spot_Light_Index := Spot_Light_Index + 1;
      end Write_Spot_Light;
   begin
      for L of Lits loop
         case L.Kind is
            when Lights.Point =>
               Write_Point_Light (L);
            when Lights.Spot =>
               Write_Spot_Light (L);
         end case;
      end loop;

      L.Component ("point_light_count").Adjust (W);
      W.Write_Int (Int (Point_Light_Index) - 1);

      L.Component ("spot_light_count").Adjust (W);
      W.Write_Int (Int (Spot_Light_Index) - 1);

      L.Component ("light_count").Adjust (W);
      W.Write_Int (Int (Lits'Length));
   end Update_Scene_Lights;

   Max_Material_Count : constant Types.Size := 20;

   Materials_UBO : UBOs.UBO;

   Material_Type : aliased constant Structs.Struct :=
      Structs.Create
        ((Base.Vec_3.Named ("albedo"),
          Base.Float.Named ("metallic"),
          Base.Float.Named ("roughness")));

   Material_Array_Type : aliased constant Fixed_Arrays.Fixed_Array :=
      Fixed_Arrays.Create
        (Max_Material_Count, Material_Type'Unchecked_Access);

   Materials_Description_Type : Structs.Struct :=
      Structs.Create
        ((Base.Int.Named ("material_count"),
          Material_Array_Type.Named ("materials")));

   procedure Update_Materials_Description
     (Mats : Materials.Material_Array)
   is
      W : UBOs.Writer := UBOs.Start (Materials_UBO);

      L : Locations.Location := Materials_Description_Type.Address;
   begin
      L.Component ("material_count").Adjust (W);
      W.Write_Int (Int (Mats'Length));

      for I in Mats'Range loop
         L.Component ("materials").Component (I).Adjust (W);
         W.Write_Vec3 (Mats (I).Albedo);
         W.Write_Float (Mats (I).Metallic);
         W.Write_Float (Mats (I).Roughness);
      end loop;
   end Update_Materials_Description;

   Probe_Layout_Macros : Macro_Definition_Array :=
     (Create_Macro_Definition ("M_RADIANCE_RESOLUTION", "30"),
      Create_Macro_Definition ("M_IRRADIANCE_RESOLUTION", "8"));

   Scene_Macros : Macro_Definition_Array :=
     (Create_Macro_Definition ("M_MAX_SPHERE_COUNT", Max_Sphere_Count'Image),
      Create_Macro_Definition ("M_MAX_PLANE_COUNT", Max_Plane_Count'Image),
      Create_Macro_Definition ("M_MAX_CUBE_COUNT", Max_Cube_Count'Image),
      Create_Macro_Definition
        ("M_MAX_POINT_LIGHT_COUNT", Max_Point_Light_Count'Image),
      Create_Macro_Definition
        ("M_MAX_SPOT_LIGHT_COUNT", Max_Spot_Light_Count'Image));

   Probe_Render_Macros : Macro_Definition_Array :=
     (Create_Macro_Definition ("M_COMPUTE_DIRECT_SPECULAR", "0"),
      Create_Macro_Definition ("M_COMPUTE_INDIRECT_SPECULAR", "0"));

   Render_Macros : Macro_Definition_Array :=
     (Create_Macro_Definition ("M_COMPUTE_DIRECT_SPECULAR", "1"),
      Create_Macro_Definition ("M_COMPUTE_INDIRECT_SPECULAR", "2"),
      Create_Macro_Definition ("M_ADD_INDIRECT_SPECULAR", "1"));

   FPS_Clock : Ada.Calendar.Time;

   Scene_Descr : Primitives.Primitive_Array_Access :=
      new Primitives.Primitive_Array'
        ((Primitives.Sphere, 0, ( 3.5,  3.0,  3.0), 1.0),
         (Primitives.Cube,   1, ( 3.0,  0.0,  4.0), 1.5),
         (Primitives.Plane,  2, ( 0.0,  1.0,  0.0), 1.0),
         (Primitives.Plane,  2, ( 0.0, -1.0,  0.0), 7.0),
         (Primitives.Plane,  3, ( 1.0,  0.0,  0.0), 1.0),
         (Primitives.Plane,  4, (-1.0,  0.0,  0.0), 7.0),
         (Primitives.Plane,  2, ( 0.0,  0.0,  1.0), 6.0),
         (Primitives.Plane,  2, ( 0.0,  0.0, -1.0), 7.0));

   procedure Add_Sphere is
      use type Primitives.Primitive_Array;
   begin
      Scene_Descr := new Primitives.Primitive_Array'
        (Scene_Descr.all &
           (Primitives.Primitive'(Primitives.Sphere, 2, Cam_Pos, 0.5)));
      Update_Scene_Primitives (Scene_Descr.all);
   end Add_Sphere;

   Mat_Descr : Materials.Material_Array :=
      (((0.1, 0.1, 0.1), 0.9, 0.1),
       ((0.0, 1.0, 0.0), 0.8, 0.3),
       ((0.0, 0.0, 0.0), 0.0, 0.6),
       ((1.0, 0.0, 0.0), 0.0, 0.6),
       ((0.0, 0.0, 1.0), 0.0, 0.6));

   Q_Pressed : Boolean := False;

   Suzanne_Type : Structs.Struct := Structs.Create
     ((Base.Vec_3.Named ("position"), Base.Vec_3.Named ("extent")));

   Suzanne_UBO : UBOs.UBO;

   procedure Prepare_Suzanne is
      use GL.Objects.Textures.Targets;
      use type Singles.Vector3;

      Suzanne_Mesh : Meshes.Mesh :=
         Meshes.Obj_Loader.Load_Obj_File ("media/suzanne.obj");
      Suzanne_BB   : Meshes.Bounding_Box :=
         Meshes.Compute_Bounding_Box (Suzanne_Mesh);
      Suzanne_DT : aliased Meshes.Distance_Maps.Distance_Map :=
         Meshes.Distance_Maps.Build_From_Mesh
           (Suzanne_Mesh, Suzanne_BB, 20, 20, 20);
      Suzanne_Tex : GL.Objects.Textures.Texture;

      Extent : Singles.Vector3 := (Suzanne_BB.To - Suzanne_BB.From) / 2.0;

      W : UBOs.Writer := UBOs.Start (Suzanne_UBO);
   begin
      Suzanne_Tex.Initialize_Id;
      Objects.Textures.Set_Active_Unit (2);
      Texture_3D.Bind (Suzanne_Tex);
      Texture_3D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_3D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_3D.Set_Z_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_3D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_3D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Meshes.Distance_Maps.Load_To_Texture (Suzanne_DT);

      W.Write_Vec3 ((1.0, 1.0, 1.0));
      W.Write_Vec3 (Extent);
      Ada.Text_IO.Put_Line (Extent (X)'Image);
      Ada.Text_IO.Put_Line (Extent (Y)'Image);
      Ada.Text_IO.Put_Line (Extent (Z)'Image);
   end Prepare_Suzanne;
begin
   GLFW_Utils.Init;
   GLFW_Utils.Open_Window (Width => 1000, Height => 1000, Title => "Madarch");

   -- set up matrices
   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
   Modelview.Load_Identity;

   -- load shader sources and compile shaders
   Load_Shader (Vertex_Shader,
                "src/glsl/identity.glsl",
                No_Macro_Definition_Array, "120");

   Load_Shader (Image_Shader,
                "src/glsl/draw_screen.glsl",
                Probe_Layout_Macros & Scene_Macros & Render_Macros, "420");

   Load_Shader (Radiance_Shader,
                "src/glsl/compute_probe_radiance.glsl",
                Probe_Layout_Macros & Scene_Macros & Probe_Render_Macros, "420");

   Load_Shader (Irradiance_Shader,
                "src/glsl/update_probe_irradiance.glsl",
                Probe_Layout_Macros, "420");

   -- setup probes
   Probes_UBO := Probes_Layout_Type.Allocate (Binding => 0);
   Setup_Probe_Layout (X  => 4,   Y  => 3,   Z  => 3,
                       SX => 2.0, SY => 3.0, SZ => 3.0);

   -- setup scene
   Scene_UBO := Scene_Description_Type.Allocate (Binding => 1);

   Update_Scene_Primitives (Scene_Descr.all);
   Update_Scene_Lights (All_Lights);

   -- setup materials
   Materials_UBO := Materials_Description_Type.Allocate (Binding => 2);
   Update_Materials_Description (Mat_Descr);

   -- setup suzanne
   Suzanne_UBO := Suzanne_Type.Allocate (Binding => 3);

   -- prepare data structures
   Prepare_Radiance;
   Prepare_Irradiance;
   Prepare_Image;
   Prepare_Suzanne;

   while GLFW_Utils.Window_Opened loop
      exit when Handle_Events;
      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.Space) then
         FPS_Clock := Ada.Calendar.Clock;

         Time := Time + 0.01;
         Spot_Light.Spot_Light_Dir :=
           (Single_Elementaries.Cos (Time),
            Single_Elementaries.Sin (Time),
            0.0);

         Update_Scene_Lights (All_Lights);
         Compute_Radiance;
         Update_Irradiance;
         Draw_Image;

         Ada.Text_IO.Put_Line
           (Ada.Calendar."-"(Ada.Calendar.Clock, FPS_Clock)'Image);
      end if;

      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.Q) then
         if not Q_Pressed then
            Mat_Descr (4).Albedo (X) := 1.0 - Mat_Descr (4).Albedo (X);
            -- Mat_Descr (1).Roughness := Mat_Descr (1).Roughness + 0.1;
            Update_Materials_Description (Mat_Descr);
         end if;
         Q_Pressed := True;
      else
         Q_Pressed := False;
      end if;
      GLFW_Utils.Poll_Events;
   end loop;

   GLFW_Utils.Shutdown;
end Main;
