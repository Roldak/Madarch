--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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

with GNATCOLL.VFS;
with GNATCOLL.Strings;

procedure Main is
   use GL;
   use GL.Buffers;
   use GL.Types;
   use GL.Fixed.Matrix;
   use GL.Immediate;

   procedure Load_Shader
     (Shader : in out GL.Objects.Shaders.Shader;
      Source_File : String)
   is
      use GNATCOLL;

      Source_Path : VFS.Virtual_File :=
         VFS.Create (VFS.Filesystem_String (Source_File));

      Content : Strings.XString := VFS.Read_File (Source_Path);
   begin
      Shader.Initialize_Id;
      GL.Objects.Shaders.Set_Source (Shader, Content.To_String);
      Shader.Compile;
      if not Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of " & Source_File & " failed. log:");
         Ada.Text_IO.Put_Line (Shader.Info_Log);
      end if;
   end Load_Shader;

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
   Cam_Pos         : Singles.Vector3 := (-2.0, 0.0, -3.0);

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
   Irradiance_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);

   Irradiance_Program : GL.Objects.Programs.Program;
   Rnd_Uniform        : GL.Uniforms.Uniform;

   Irradiance_Data_1 : GL.Objects.Textures.Texture;
   Irradiance_Data_2 : GL.Objects.Textures.Texture;
   Irradiance_FB     : GL.Objects.Framebuffers.Framebuffer;

   Probe_Resolution : constant GL.Types.Int := 10;
   Probe_Count_X    : constant GL.Types.Int := 4;
   Probe_Count_Y    : constant GL.Types.Int := 4;

   procedure Prepare_Irradiance is
      use GL.Objects.Textures.Targets;
      use type GL.Objects.Framebuffers.Framebuffer_Status;

      procedure Prepare_Irradiance_Texture is
      begin
         Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Repeat);
         Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Repeat);
         Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Nearest);
         Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Nearest);
         Texture_2D.Load_Empty_Texture
           (0, GL.Pixels.RGB8,
            Probe_Resolution * Probe_Count_X,
            Probe_Resolution * Probe_Count_Y);
      end Prepare_Irradiance_Texture;
   begin
      Irradiance_Data_1.Initialize_Id;
      Irradiance_Data_2.Initialize_Id;
      Irradiance_Fb.Initialize_Id;

      Texture_2D.Bind (Irradiance_Data_1);
      Prepare_Irradiance_Texture;

      Texture_2D.Bind (Irradiance_Data_2);
      Prepare_Irradiance_Texture;

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Irradiance_FB);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Attach_Texture
        (GL.Objects.Framebuffers.Color_Attachment_0, Irradiance_Data_1, 0);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Attach_Texture
        (GL.Objects.Framebuffers.Color_Attachment_1, Irradiance_Data_2, 0);

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

      Rnd_Uniform := GL.Objects.Programs.Uniform_Location (Irradiance_Program, "time");
   end Prepare_Irradiance;

   Swap : GL.Types.Int := 0;

   procedure Update_Irradiance is
      use GL.Objects.Textures.Targets;
   begin
      Irradiance_Program.Use_Program;
      GL.Uniforms.Set_Single (Rnd_Uniform, Time);

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Irradiance_FB);

      if Swap = 0 then
         GL.Buffers.Set_Active_Buffer (GL.Buffers.Color_Attachment0);
      else
         GL.Buffers.Set_Active_Buffer (GL.Buffers.Color_Attachment1);
      end if;

      GL.Objects.Textures.Set_Active_Unit (Swap);
      Texture_2D.Bind (Irradiance_Data_2);
      GL.Objects.Textures.Set_Active_Unit (1 - Swap);
      Texture_2D.Bind (Irradiance_Data_1);

      Swap := 1 - Swap;

      GL.Window.Set_Viewport
        (0, 0,
         Probe_Resolution * Probe_Count_X,
         Probe_Resolution * Probe_Count_Y);

      Clear (Buffer_Bits'(Color => True, others => False));
      Draw_Fullscreen_Quad;

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);
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
   begin
      Time := Time + 1.0;

      Image_Program.Use_Program;
      GL.Uniforms.Set_Single (Time_Uniform, Time);
      GL.Uniforms.Set_Single (Cam_Pos_Uniform, Cam_Pos);

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
   Load_Shader (Irradiance_Shader, "src/glsl/compute_irradiance.glsl");

   -- prepare data structures
   Prepare_Irradiance;
   Prepare_Image;

   Cam_Pos (Y) := 2.0;

   while GLFW_Utils.Window_Opened loop
      exit when Handle_Events;
      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.Space) then
         Update_Irradiance;
         Draw_Image;
      end if;
      GLFW_Utils.Poll_Events;
   end loop;

   GLFW_Utils.Shutdown;
end Main;
