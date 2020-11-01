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
with Primitives;
with Shader_Loader; use Shader_Loader;
with UBOs;

procedure Main is
   use GNATCOLL;

   use GL;
   use GL.Buffers;
   use GL.Types;
   use GL.Fixed.Matrix;
   use GL.Immediate;

   procedure Set_Uniform_Int_Data is
      new Objects.Buffers.Set_Sub_Data (Int_Pointers);
   procedure Set_Uniform_Float_Data is
      new Objects.Buffers.Set_Sub_Data (Single_Pointers);

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

   Probe_Radiance_Resolution   : constant GL.Types.Int := 30;

   Probe_Count_X    : constant GL.Types.Int := 6;
   Probe_Count_Y    : constant GL.Types.Int := 6;

   Irradiance_Data : GL.Objects.Textures.Texture;

   procedure Prepare_Radiance is
      use GL.Objects.Textures.Targets;
      use type GL.Objects.Framebuffers.Framebuffer_Status;
   begin
      Radiance_Data.Initialize_Id;
      Radiance_Fb.Initialize_Id;

      Texture_2D.Bind (Radiance_Data);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Load_Empty_Texture
        (0, GL.Pixels.RGB8,
         Probe_Radiance_Resolution * Probe_Count_X,
         Probe_Radiance_Resolution * Probe_Count_Y);

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Radiance_FB);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Attach_Texture
        (GL.Objects.Framebuffers.Color_Attachment_0, Radiance_Data, 0);

      if GL.Objects.Framebuffers.Read_And_Draw_Target.Status
            /= GL.Objects.Framebuffers.Complete
      then
         Ada.Text_IO.Put_Line
           ("Error: Framebuffer status is " &
              GL.Objects.Framebuffers.Read_And_Draw_Target.Status'Img);
         return;
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

      Texture_2D.Bind (Irradiance_Data);

      GL.Window.Set_Viewport
        (0, 0,
         Probe_Radiance_Resolution * Probe_Count_X,
         Probe_Radiance_Resolution * Probe_Count_Y);

      Clear (Buffer_Bits'(Color => True, others => False));
      Draw_Fullscreen_Quad;

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

      Texture_2D.Bind (Radiance_Data);

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
      Time := Time + 0.01;

      Image_Program.Use_Program;
      GL.Uniforms.Set_Single (Time_Uniform, Time);
      GL.Uniforms.Set_Single (Cam_Pos_Uniform, Cam_Pos);

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);

      Texture_2D.Bind (Irradiance_Data);

      GL.Window.Set_Viewport (0, 0, 1000, 1000);
      Clear (Buffer_Bits'(Color => True, others => False));
      Draw_Fullscreen_Quad;

      GL.Flush;
      GLFW_Utils.Swap_Buffers;
   end Draw_Image;

   Probes_UBO : UBOs.UBO;

   procedure Setup_Probe_Layout (X, Y, Z : Int; SX, SY, SZ : Single) is
      W : UBOs.Writer := UBOs.Start (Probes_UBO);
   begin
      if X * Y * Z /= Probe_Count_X * Probe_Count_Y then
         raise Program_Error with "Probe_Count should match grid dimensions.";
      end if;

      W.Write_Int (Probe_Count_X);
      W.Write_Int (Probe_Count_Y);

      W.Pad (16);
      W.Write_Int (X);
      W.Write_Int (Y);
      W.Write_Int (Z);

      W.Pad (32);
      W.Write_Float (SX);
      W.Write_Float (SY);
      W.Write_Float (SZ);
   end Setup_Probe_Layout;

   Scene_UBO  : UBOs.UBO;

   procedure Update_Scene_Description
     (Prims : Primitives.Primitive_Array)
   is
      W : UBOs.Writer := UBOs.Start (Scene_UBO);
   begin
      W.Write_Int (Int (Prims'Length));

      for Prim of Prims loop
         W.Pad (16);
         W.Write_Int (Int (Prim.Kind'Enum_Rep));
         case Prim.Kind is
            when Primitives.Sphere =>
               W.Write_Vec3 (Prim.Sphere_Center);
               W.Write_Float (Prim.Sphere_Radius);
            when Primitives.Cube =>
               W.Write_Vec3 (Prim.Cube_Center);
               W.Write_Float (Prim.Cube_Side);
            when Primitives.Plane =>
               W.Write_Vec3 (Prim.Normal);
               W.Write_Float (Prim.Offset);
         end case;
         W.Write_Int (Prim.Material);
      end loop;
   end Update_Scene_Description;

   Materials_UBO : UBOs.UBO;

   procedure Update_Materials_Description
     (Mats : Materials.Material_Array)
   is
      W : UBOs.Writer := UBOs.Start (Materials_UBO);
   begin
      W.Write_Int (Int (Mats'Length));

      for M of Mats loop
         W.Pad (16);
         W.Write_Vec3 (M.Albedo);
         W.Write_Float (M.Metallic);
         W.Write_Float (M.Roughness);
      end loop;
   end Update_Materials_Description;

   Probe_Layout_Macros : Macro_Definition_Array :=
     (Create_Macro_Definition ("M_RADIANCE_RESOLUTION", "30"),
      Create_Macro_Definition ("M_IRRADIANCE_RESOLUTION", "8"));

   FPS_Clock : Ada.Calendar.Time;

   Scene_Descr : Primitives.Primitive_Array :=
     ((Primitives.Sphere, 0, ( 3.5,  3.0,  3.0), 1.0),
      (Primitives.Cube,   1, ( 3.0,  0.0,  4.0), 1.5),
      (Primitives.Plane,  2, ( 0.0,  1.0,  0.0), 1.0),
      (Primitives.Plane,  2, ( 0.0, -1.0,  0.0), 7.0),
      (Primitives.Plane,  3, ( 1.0,  0.0,  0.0), 1.0),
      (Primitives.Plane,  4, (-1.0,  0.0,  0.0), 7.0),
      (Primitives.Plane,  2, ( 0.0,  0.0,  1.0), 6.0),
      (Primitives.Plane,  2, ( 0.0,  0.0, -1.0), 7.0));

   Mat_Descr : Materials.Material_Array :=
      (((1.0, 0.0, 0.0), 0.5, 0.4),
       ((0.0, 1.0, 0.0), 0.8, 0.8),
       ((0.0, 0.0, 0.0), 0.0, 0.6),
       ((1.0, 0.0, 0.0), 0.0, 0.6),
       ((0.0, 0.0, 1.0), 0.0, 0.6));

   Q_Pressed : Boolean := False;
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
                "src/glsl/render_image.glsl",
                Probe_Layout_Macros, "420");

   Load_Shader (Radiance_Shader,
                "src/glsl/compute_probe_radiance.glsl",
                Probe_Layout_Macros, "420");

   Load_Shader (Irradiance_Shader,
                "src/glsl/update_probe_irradiance.glsl",
                Probe_Layout_Macros, "420");

   -- setup probes
   Probes_UBO := UBOs.Create (0, 48);
   Setup_Probe_Layout (X  => 4,   Y  => 3,   Z  => 3,
                       SX => 2.0, SY => 3.0, SZ => 3.0);

   -- setup scene
   Scene_UBO := UBOs.Create (1, 16 + 48 * 20);
   Update_Scene_Description (Scene_Descr);

   -- setup materials
   Materials_UBO := UBOs.Create (2, 16 + 32 * 20);
   Update_Materials_Description (Mat_Descr);

   -- prepare data structures
   Prepare_Radiance;
   Prepare_Irradiance;
   Prepare_Image;

   while GLFW_Utils.Window_Opened loop
      exit when Handle_Events;
      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.Space) then
         FPS_Clock := Ada.Calendar.Clock;

         Compute_Radiance;
         Update_Irradiance;
         Draw_Image;

         Ada.Text_IO.Put_Line
           (Ada.Calendar."-"(Ada.Calendar.Clock, FPS_Clock)'Image);
      end if;

      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.Q) then
         if not Q_Pressed then
            Mat_Descr (4).Albedo (X) := 1.0 - Mat_Descr (4).Albedo (X);
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
