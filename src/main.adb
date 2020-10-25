--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Text_IO;

with GL.Buffers;
with GL.Files;
with GL.Fixed.Matrix;
with GL.Immediate;
with GL.Objects.Programs;
with GL.Objects.Shaders.Lists;
with GL.Types.Colors;
with GL.Uniforms;
with GLFW_Utils;

with Glfw.Input.Keys;

procedure Main is
   use GL;
   use GL.Buffers;
   use GL.Types;
   use GL.Fixed.Matrix;
   use GL.Immediate;

   Program : GL.Objects.Programs.Program;

   procedure List_Shaders is
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

   Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Fragment_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);

   Time_Uniform    : GL.Uniforms.Uniform;
   Time            : GL.Types.Single := 0.0;
   Cam_Pos_Uniform : GL.Uniforms.Uniform;
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

   procedure Draw is
   begin
      Time := Time + 1.0;

      GL.Uniforms.Set_Single (Time_Uniform, Time);
      GL.Uniforms.Set_Single (Cam_Pos_Uniform, Cam_Pos);

      Clear (Buffer_Bits'(Color => True, others => False));
      declare
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
      end;

      GL.Flush;
      GLFW_Utils.Swap_Buffers;
   end Draw;
begin
   GLFW_Utils.Init;
   GLFW_Utils.Open_Window (Width => 1000, Height => 1000, Title => "Madarch");

   Vertex_Shader.Initialize_Id;
   Fragment_Shader.Initialize_Id;
   Program.Initialize_Id;

   -- load shader sources and compile shaders

   GL.Files.Load_Shader_Source_From_File
     (Vertex_Shader, "src/glsl/vert.glsl");
   GL.Files.Load_Shader_Source_From_File
     (Fragment_Shader, "src/glsl/frag.glsl");

   Vertex_Shader.Compile;
   Fragment_Shader.Compile;

   if not Vertex_Shader.Compile_Status then
      Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. log:");
      Ada.Text_IO.Put_Line (Vertex_Shader.Info_Log);
   end if;
   if not Fragment_Shader.Compile_Status then
      Ada.Text_IO.Put_Line ("Compilation of fragment shader failed. log:");
      Ada.Text_IO.Put_Line (Fragment_Shader.Info_Log);
   end if;

   -- set up program
   Program.Attach (Vertex_Shader);
   Program.Attach (Fragment_Shader);
   Program.Link;
   if not Program.Link_Status then
      Ada.Text_IO.Put_Line ("Program linking failed. Log:");
      Ada.Text_IO.Put_Line (Program.Info_Log);
      return;
   end if;
   Program.Use_Program;

   Time_Uniform    := GL.Objects.Programs.Uniform_Location (Program, "time");
   Cam_Pos_Uniform := GL.Objects.Programs.Uniform_Location (Program, "camera_position");
   Cam_Pos (Y) := 2.0;

   -- test iteration over program shaders
   List_Shaders;

   -- set up matrices
   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
   Modelview.Load_Identity;

   while GLFW_Utils.Window_Opened loop
      exit when Handle_Events;
      if GLFW_Utils.Key_Pressed (Glfw.Input.Keys.Space) then
         Draw;
      end if;
      GLFW_Utils.Poll_Events;
   end loop;

   GLFW_Utils.Shutdown;
end Main;
