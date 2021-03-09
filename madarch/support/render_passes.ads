with GL.Objects.Framebuffers;
with GL.Objects.Shaders;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Uniforms;

with GL.Types;

package Render_Passes is
   use GL;

   type Render_Pass is abstract tagged private;

   procedure Render (Pass : Render_Pass) is abstract;
   function Uniform
     (Self : Render_Pass; Name : String) return Uniforms.Uniform;

   type Screen_Render_Pass is new Render_Pass with private;

   function Create
     (Vertex_Shader   : Objects.Shaders.Shader;
      Fragment_Shader : Objects.Shaders.Shader;
      Frame_Width     : Types.Size;
      Frame_Height    : Types.Size)
      return Screen_Render_Pass;

   overriding procedure Render (Pass : Screen_Render_Pass);

   type Framebuffer_Render_Pass is new Render_Pass with private;

   function Create
     (Vertex_Shader   : Objects.Shaders.Shader;
      Fragment_Shader : Objects.Shaders.Shader;
      Frame_Width     : Types.Size;
      Frame_Height    : Types.Size;
      Target          : Objects.Textures.Texture_Unit;
      Texture_Format  : Pixels.Internal_Format := Pixels.RGB8)
      return Framebuffer_Render_Pass;

   overriding procedure Render (Pass : Framebuffer_Render_Pass);

private
   type Render_Pass is abstract tagged record
      Program      : Objects.Programs.Program;
      Frame_Width  : Types.Size;
      Frame_Height : Types.Size;
   end record;

   type Screen_Render_Pass is new Render_Pass with null record;

   type Framebuffer_Render_Pass is new Render_Pass with record
      Framebuffer : Objects.Framebuffers.Framebuffer;
      Texture     : Objects.Textures.Texture;
   end record;
end Render_Passes;
