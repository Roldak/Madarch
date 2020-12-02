with Madarch.Scenes;

with GPU_Buffers;

package Madarch.Renderers is
   type Renderer is private;

   type Probe_Settings is record
      Radiance_Resolution   : Natural := 32;
      Irradiance_Resolution : Natural := 8;
   end record;

   Default_Probe_Settings : constant Probe_Settings;

   function Create
     (Scene  : Scenes.Scene;
      Probes : Probe_Settings := Default_Probe_Settings) return Renderer;
private
   type Renderer_Internal is record
      Scene : Scenes.Scene;

      Scene_Buffer : GPU_Buffers.GPU_Buffer;
   end record;

   Default_Probe_Settings : constant Probe_Settings := (others => <>);

   type Renderer is access Renderer_Internal;
end Madarch.Renderers;
