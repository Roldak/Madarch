with GL;
with GL.Objects.Programs;
with GL.Objects.Shaders;

with Shader_Loader;
with GPU_Types;
with GPU_Types.Base;
with GPU_Types.Fixed_Arrays;
with GPU_Types.Structs;

with Glfw.Windows.Context;

package body Madarch.Renderers is
   Probes_Layout_Type : GPU_Types.GPU_Type := GPU_Types.Structs.Create
     ((GPU_Types.Base.IVec_2.Named ("probe_count"),
       GPU_Types.Base.IVec_3.Named ("grid_dimensions"),
       GPU_Types.Base.Vec_3.Named  ("grid_spacing")));

   procedure Setup_Probe_Layout
     (Self   : Renderer;
      Probes : Probe_Settings)
   is
      use GL;
      use GPU_Types;

      W : GPU_Buffers.Writer := GPU_Buffers.Start (Self.Probes_Buffer);

      L : Locations.Location := Probes_Layout_Type.Address;

      Total_On_Grid : Int :=
         Probes.Grid_Dimensions (X) *
         Probes.Grid_Dimensions (Y) *
         Probes.Grid_Dimensions (Z);

      Total_Count : Int :=
         Probes.Probe_Count (X) *
         Probes.Probe_Count (Y);
   begin
      if Total_On_Grid /= Total_Count then
         raise Program_Error with "Probe_Count should match grid dimensions.";
      end if;

      L.Component ("probe_count").Adjust (W);
      W.Write_IVec2 (Probes.Probe_Count);

      L.Component ("grid_dimensions").Adjust (W);
      W.Write_IVec3 (Probes.Grid_Dimensions);

      L.Component ("grid_spacing").Adjust (W);
      W.Write_Vec3 (Probes.Grid_Spacing);
   end Setup_Probe_Layout;

   Material_Type : GPU_Types.GPU_Type :=
      GPU_Types.Structs.Create
        ((GPU_Types.Base.Vec_3.Named ("albedo"),
          GPU_Types.Base.Float.Named ("metallic"),
          GPU_Types.Base.Float.Named ("roughness")));

   Material_Array_Type : GPU_Types.GPU_Type :=
      GPU_Types.Fixed_Arrays.Create (20, Material_Type);

   Materials_Description_Type : GPU_Types.GPU_Type :=
      GPU_Types.Structs.Create
        ((GPU_Types.Base.Int.Named ("material_count"),
          Material_Array_Type.Named ("materials")));

   function Create
     (Window : Windows.Window;
      Scene  : Scenes.Scene;
      Probes : Probe_Settings := Default_Probe_Settings) return Renderer
   is
      use Shader_Loader;

      Probe_Count_X : constant Int := Probes.Probe_Count (GL.X);
      Probe_Count_Y : constant Int := Probes.Probe_Count (GL.Y);

      Scene_Description_Type : GPU_Types.GPU_Type :=
         Scenes.Get_GPU_Type (Scene);

      Probe_Layout_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition
           ("M_RADIANCE_RESOLUTION", Probes.Radiance_Resolution'Image),
         Create_Macro_Definition
           ("M_IRRADIANCE_RESOLUTION", Probes.Irradiance_Resolution'Image));

      Scene_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition ("M_MAX_SPHERE_COUNT", "20"),
         Create_Macro_Definition ("M_MAX_PLANE_COUNT", "20"),
         Create_Macro_Definition ("M_MAX_CUBE_COUNT", "20"),
         Create_Macro_Definition ("M_MAX_POINT_LIGHT_COUNT", "4"),
         Create_Macro_Definition ("M_MAX_SPOT_LIGHT_COUNT", "4"));

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
   begin
      Glfw.Windows.Context.Make_Current (Window);

      Load_Shader
        (Vertex_Shader,
         "madarch/glsl/identity.glsl",
         No_Macro_Definition_Array, "120");

      Load_Shader
        (Screen_Shader,
         "madarch/glsl/draw_screen.glsl",
         Probe_Layout_Macros & Scene_Macros & Render_Macros, "420");

      Load_Shader
        (Radiance_Shader,
         "madarch/glsl/compute_probe_radiance.glsl",
         Probe_Layout_Macros & Scene_Macros & Probe_Render_Macros, "420");

      Load_Shader
        (Irradiance_Shader,
         "madarch/glsl/update_probe_irradiance.glsl",
         Probe_Layout_Macros, "420");

      return R : Renderer := new Renderer_Internal'
        (Window       => Window,
         Scene        => Scene,

         Probes_Buffer => Probes_Layout_Type.Allocate
           (Kind => GPU_Buffers.Uniform_Buffer, Binding => 0),

         Scene_Buffer => Scene_Description_Type.Allocate
           (Kind => GPU_Buffers.Uniform_Buffer, Binding => 1),

         Materials_Buffer => Materials_Description_Type.Allocate
           (Kind => GPU_Buffers.Uniform_Buffer, Binding => 2),

         Radiance_Pass => Render_Passes.Create
           (Vertex_Shader   => Vertex_Shader,
            Fragment_Shader => Radiance_Shader,

            Frame_Width  => Probes.Radiance_Resolution * Probe_Count_X,
            Frame_Height => Probes.Radiance_Resolution * Probe_Count_Y,

            Target => 0),

         Irradiance_Pass => Render_Passes.Create
           (Vertex_Shader   => Vertex_Shader,
            Fragment_Shader => Irradiance_Shader,

            Frame_Width  => Probes.Irradiance_Resolution * Probe_Count_X,
            Frame_Height => Probes.Irradiance_Resolution * Probe_Count_Y,

            Target => 1),

         Screen_Pass => Render_Passes.Create
           (Vertex_Shader   => Vertex_Shader,
            Fragment_Shader => Screen_Shader,

            Frame_Width  => Int (Window.Width),
            Frame_Height => Int (Window.Height)))
      do
         Setup_Probe_Layout (R, Probes);
      end return;
   end Create;

   procedure Render (Self : Renderer) is
   begin
      Glfw.Windows.Context.Make_Current (Self.Window);
      Self.Radiance_Pass.Render;
      Self.Irradiance_Pass.Render;
      Self.Screen_Pass.Render;
      Glfw.Windows.Context.Swap_Buffers (Self.Window);
   end Render;

   procedure Set_Material
     (Self  : in out Renderer;
      Index : Positive;
      Mat   : Materials.Material)
   is
      use GPU_Types;

      W : GPU_Buffers.Writer := GPU_Buffers.Start (Self.Materials_Buffer);

      L : Locations.Location := Materials_Description_Type.Address;
   begin
      L.Component ("materials").Component (Index).Adjust (W);
      W.Write_Vec3 (Mat.Albedo);
      W.Write_Float (Mat.Metallic);
      W.Write_Float (Mat.Roughness);
   end Set_Material;
end Madarch.Renderers;
