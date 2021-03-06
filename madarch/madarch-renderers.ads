with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with GL.Types;

with Madarch.Entities;
with Madarch.Materials;
with Madarch.Lights;
with Madarch.Primitives;
with Madarch.Scenes;
with Madarch.Values;
with Madarch.Windows;

with GPU_Buffers;
with Render_Passes;

package Madarch.Renderers is
   use GL.Types;

   type Renderer is private;

   type Probe_Settings is record
      Radiance_Resolution   : Int := 32;
      Irradiance_Resolution : Int := 8;
      Probe_Count           : Ints.Vector2 := (6, 6);
      Grid_Dimensions       : Ints.Vector3 := (4, 3, 3);
      Grid_Spacing          : Singles.Vector3 := (2.0, 3.0, 3.0);
   end record;

   Default_Probe_Settings : constant Probe_Settings;

   function Create
     (Window : Windows.Window;
      Scene  : Scenes.Scene;
      Probes : Probe_Settings := Default_Probe_Settings) return Renderer;

   procedure Render (Self : Renderer);

   procedure Set_Material
     (Self   : in out Renderer;
      Index  : Positive;
      Entity : Entities.Entity);

   procedure Add_Primitive
     (Self   : in out Renderer;
      Prim   : Primitives.Primitive;
      Entity : Entities.Entity);

   procedure Set_Light
     (Self   : in out Renderer;
      Index  : Positive;
      Lit    : Lights.Light;
      Entity : Entities.Entity);

   procedure Update_Partionning (Self : in out Renderer);
private
   package Entity_Vectors is new Ada.Containers.Vectors
     (Positive, Entities.Entity, Entities."=");

   package Primitive_Entity_Maps is new Ada.Containers.Hashed_Maps
     (Primitives.Primitive,
      Entity_Vectors.Vector,
      Primitives.Hash,
      Primitives."=",
      Entity_Vectors."=");

   type Renderer_Internal is record
      Window : Windows.Window;
      Scene  : Scenes.Scene;

      Probes_Buffer    : GPU_Buffers.GPU_Buffer;
      Scene_Buffer     : GPU_Buffers.GPU_Buffer;
      Materials_Buffer : GPU_Buffers.GPU_Buffer;

      Radiance_Pass   : Render_Passes.Framebuffer_Render_Pass;
      Irradiance_Pass : Render_Passes.Framebuffer_Render_Pass;
      Screen_Pass     : Render_Passes.Screen_Render_Pass;

      All_Primitives  : Primitive_Entity_Maps.Map;

      Partitioning_Buffer : GPU_Buffers.GPU_Buffer;
   end record;

   Default_Probe_Settings : constant Probe_Settings := (others => <>);

   type Renderer is access Renderer_Internal;
end Madarch.Renderers;
