with Ada.Text_IO;

with GL.Buffers;
with GL.Files;
with GL.Fixed.Matrix;
with GL.Immediate;
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

with Load_Shader;

procedure Main is
   use GL;
   use GL.Buffers;
   use GL.Types;
   use GL.Fixed.Matrix;
   use GL.Immediate;

   procedure List_Shaders (Program : GL.Objects.Programs.Program) is
   begin
      Ada.Text_IO.Put_Line ("Listing shaders attached to program...");
      declare
         use type GL.Objects.Shaders.Lists.Cursor;

         List : constant GL.Objects.Shaders.Lists.List
           := Program.Attached_Shaders;
         Cursor : GL.Objects.Shaders.Lists.Cursor := List.First;
      begin
         while Cursor /= GL.Objects.Shaders.Lists.No_Element loop
            declare
               Shader : constant GL.Objects.Shaders.Shader
                 := GL.Objects.Shaders.Lists.Element (Cursor);
            begin
               Ada.Text_IO.Put_Line ("----------------------------");
               Ada.Text_IO.Put_Line ("Kind: " & Shader.Kind'Img);
               Ada.Text_IO.Put_Line ("Status: " & Shader.Compile_Status'Img);
            end;
            Cursor := GL.Objects.Shaders.Lists.Next (Cursor);
         end loop;
      end;
      Ada.Text_IO.Put_Line ("-----------[Done.]----------");
   end List_Shaders;

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
begin
   GLFW_Utils.Init;
   GLFW_Utils.Open_Window (Width => 1000, Height => 1000, Title => "Madarch");

   -- set up matrices
   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
   Modelview.Load_Identity;

   -- load shader sources and compile shaders
   Load_Shader (Vertex_Shader,     "src/glsl/identity.glsl");
   Load_Shader (Image_Shader,      "src/glsl/render_image.glsl");
   Load_Shader (Radiance_Shader,   "src/glsl/compute_probe_radiance.glsl");
   Load_Shader (Irradiance_Shader, "src/glsl/update_probe_irradiance.glsl");

   -- prepare data structures
   Prepare_Radiance;
   Prepare_Irradiance;
   Prepare_Image;

   while GLFW_Utils.Window_Opened loop
      exit when Handle_Events;
      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.Space) then
         Compute_Radiance;
         Update_Irradiance;
         Draw_Image;
      end if;
      GLFW_Utils.Poll_Events;
   end loop;

   GLFW_Utils.Shutdown;
end Main;
