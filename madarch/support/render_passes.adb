with Ada.Text_IO;

with GL.Buffers;
with GL.Immediate;
with GL.Pixels;
with GL.Types.Colors;
with GL.Window;

package body Render_Passes is
   procedure Draw_Fullscreen_Quad is
      use GL;
      use GL.Immediate;
      use GL.Types;

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

   procedure Render_Base (Pass : Render_Pass'Class) is
      use GL.Buffers;
      use GL.Objects.Textures.Targets;
   begin
      Pass.Program.Use_Program;

      GL.Window.Set_Viewport
        (0, 0, Pass.Frame_Width, Pass.Frame_Height);

      Clear (Buffer_Bits'(Color => True, others => False));
      Draw_Fullscreen_Quad;
   end Render_Base;

   procedure Init_Program
     (Program         : in out Objects.Programs.Program;
      Vertex_Shader   : Objects.Shaders.Shader;
      Fragment_Shader : Objects.Shaders.Shader)
   is
   begin
      Program.Initialize_Id;
      Program.Attach (Vertex_Shader);
      Program.Attach (Fragment_Shader);
      Program.Link;

      if not Program.Link_Status then
         Ada.Text_IO.Put_Line ("Program linking failed. Log:");
         Ada.Text_IO.Put_Line (Program.Info_Log);
         raise Program_Error;
      end if;
   end Init_Program;

   function Create
     (Vertex_Shader   : Objects.Shaders.Shader;
      Fragment_Shader : Objects.Shaders.Shader;
      Frame_Width     : Types.Size;
      Frame_Height    : Types.Size)
      return Screen_Render_Pass
   is
      Program : Objects.Programs.Program;
   begin
      Init_Program (Program, Vertex_Shader, Fragment_Shader);

      return (Program      => Program,
              Frame_Width  => Frame_Width,
              Frame_Height => Frame_Height);
   end Create;

   overriding procedure Render (Pass : Screen_Render_Pass) is
   begin
      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);
      Render_Base (Pass);
   end Render;

   function Create
     (Vertex_Shader   : Objects.Shaders.Shader;
      Fragment_Shader : Objects.Shaders.Shader;
      Frame_Width     : Types.Size;
      Frame_Height    : Types.Size;
      Target          : Objects.Textures.Texture_Unit)
      return Framebuffer_Render_Pass
   is
      use GL.Objects.Textures.Targets;
      use type GL.Objects.Framebuffers.Framebuffer_Status;

      Program     : Objects.Programs.Program;
      Framebuffer : Objects.Framebuffers.Framebuffer;
      Texture     : Objects.Textures.Texture;
   begin
      Init_Program (Program, Vertex_Shader, Fragment_Shader);

      Framebuffer.Initialize_Id;
      Texture.Initialize_Id;

      Objects.Textures.Set_Active_Unit (Target);
      Texture_2D.Bind (Texture);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Load_Empty_Texture
        (0, GL.Pixels.RGB8, Frame_Width, Frame_Height);

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Framebuffer);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Attach_Texture
        (GL.Objects.Framebuffers.Color_Attachment_0, Texture, 0);

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

      return (Program      => Program,
              Frame_Width  => Frame_Width,
              Frame_Height => Frame_Height,
              Framebuffer  => Framebuffer,
              Texture      => Texture);
   end Create;

   overriding procedure Render (Pass : Framebuffer_Render_Pass) is
   begin
      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind
        (Pass.Framebuffer);
      Render_Base (Pass);
   end Render;
end Render_Passes;
