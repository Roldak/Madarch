with GL;
with GL.Objects.Programs;
with GL.Objects.Shaders;

with Shader_Loader;
with GPU_Types;

with Glfw.Windows.Context;

package body Madarch.Renderers is
   function Create
     (Window : Windows.Window;
      Scene  : Scenes.Scene;
      Probes : Probe_Settings := Default_Probe_Settings) return Renderer
   is
      use Shader_Loader;

      R : Renderer;

      Scene_Description_Type : GPU_Types.GPU_Type :=
         Scenes.Get_GPU_Type (Scene);

      Probe_Layout_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition
           ("M_RADIANCE_RESOLUTION", Probes.Radiance_Resolution'Image),
         Create_Macro_Definition
           ("M_IRRADIANCE_RESOLUTION", Probes.Irradiance_Resolution'Image));

      Probe_Render_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition ("M_COMPUTE_DIRECT_SPECULAR", "0"),
         Create_Macro_Definition ("M_COMPUTE_INDIRECT_SPECULAR", "0"));

      Render_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition ("M_COMPUTE_DIRECT_SPECULAR", "1"),
         Create_Macro_Definition ("M_COMPUTE_INDIRECT_SPECULAR", "2"),
         Create_Macro_Definition ("M_ADD_INDIRECT_SPECULAR", "1"),
         Create_Macro_Definition ("M_AMBIENT_OCCLUSION_STEPS", "3"));

      Vertex_Shader   : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Vertex_Shader);

      Screen_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);

      Radiance_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);

      Irradiance_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);

      Radiance_Program   : GL.Objects.Programs.Program;
      Irradiance_Program : GL.Objects.Programs.Program;
      Screen_Program     : GL.Objects.Programs.Program;
   begin
      Glfw.Windows.Context.Make_Current (Window);

      Load_Shader (Vertex_Shader,
                   "madarch/glsl/identity.glsl",
                   No_Macro_Definition_Array, "120");

      Load_Shader (Screen_Shader,
                   "madarch/glsl/draw_screen.glsl",
                   Probe_Layout_Macros & Render_Macros, "420");

      Load_Shader (Radiance_Shader,
                   "madarch/glsl/compute_probe_radiance.glsl",
                   Probe_Layout_Macros & Probe_Render_Macros, "420");

      Load_Shader (Irradiance_Shader,
                   "madarch/glsl/update_probe_irradiance.glsl",
                   Probe_Layout_Macros, "420");

      Radiance_Program.Initialize_Id;
      Irradiance_Program.Initialize_Id;
      Screen_Program.Initialize_Id;

      R := new Renderer_Internal'
        (Window       => Window,
         Scene        => Scene,
         Scene_Buffer => Scene_Description_Type.Allocate
           (Kind => GPU_Buffers.Uniform_Buffer, Binding => 1));
      return R;
   end Create;

   procedure Render (Self : Renderer) is
   begin
      Glfw.Windows.Context.Make_Current (Self.Window);
   end Render;
end Madarch.Renderers;
