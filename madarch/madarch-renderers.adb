with Ada.Containers.Hashed_Sets;
with Ada.Text_IO;

with GL;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Pixels;
with GL.Uniforms;

with Math_Utils;
with Shader_Loader;
with GPU_Types;
with GPU_Types.Base;
with GPU_Types.Fixed_Arrays;
with GPU_Types.Structs;

with Glfw.Windows.Context;

with Madarch.Components;

package body Madarch.Renderers is
   procedure Setup_Camera
     (Self : Renderer;
      Pass : Render_Passes.Render_Pass'Class)
   is
      use GL;

      Camera_Position : Uniforms.Uniform :=
         Pass.Uniform ("camera_position");

      Camera_Orientation : Uniforms.Uniform :=
         Pass.Uniform ("camera_orientation");
   begin
      Uniforms.Set_Single (Camera_Position, Self.Camera_Position);
      Uniforms.Set_Single (Camera_Orientation, Self.Camera_Orientation);
   end Setup_Camera;

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
     (Window      : Windows.Window;
      Scene       : Scenes.Scene;
      Probes      : Probe_Settings := Default_Probe_Settings;
      Volumetrics : Volumetrics_Settings := Default_Volumetrics_Settings)
      return Renderer
   is
      use Shader_Loader;

      Probe_Count_X : constant Int := Probes.Probe_Count (GL.X);
      Probe_Count_Y : constant Int := Probes.Probe_Count (GL.Y);

      Scene_Description_Type : GPU_Types.GPU_Type :=
         Scenes.Get_GPU_Type (Scene);

      Scene_Partitioning_Type : GPU_Types.GPU_Type :=
         Scenes.Get_Partitioning_GPU_Type (Scene);

      Probe_Layout_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition
           ("M_RADIANCE_RESOLUTION", Probes.Radiance_Resolution'Image),
         Create_Macro_Definition
           ("M_IRRADIANCE_RESOLUTION", Probes.Irradiance_Resolution'Image));

      Probe_Render_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition ("M_COMPUTE_DIRECT_SPECULAR", "0"),
         Create_Macro_Definition ("M_COMPUTE_INDIRECT_SPECULAR", "0"));

      Volumetrics_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition
           ("M_VISIBILITY_RESOLUTION_Z",
            Volumetrics.Visibility_Resolution (GL.Z)'Image),
         Create_Macro_Definition
           ("M_VISIBILITY_STEP_SIZE",
            Volumetrics.Visibility_Step_Size'Image),
         Create_Macro_Definition
           ("M_SCATTERING_RESOLUTION_X",
            Volumetrics.Scattering_Resolution (GL.X)'Image),
         Create_Macro_Definition
           ("M_SCATTERING_RESOLUTION_Y",
            Volumetrics.Scattering_Resolution (GL.Y)'Image),
         Create_Macro_Definition
           ("M_SCATTERING_STEP_SIZE",
            Volumetrics.Scattering_Step_Size'Image));

      Render_Macros : Macro_Definition_Array :=
        (Create_Macro_Definition ("M_COMPUTE_DIRECT_SPECULAR", "1"),
         Create_Macro_Definition ("M_COMPUTE_INDIRECT_SPECULAR", "2"),
         Create_Macro_Definition ("M_ADD_INDIRECT_SPECULAR", "1"),
         Create_Macro_Definition ("M_AMBIENT_OCCLUSION_STEPS", "3"),
         Create_Macro_Definition
           ("M_RENDER_VOLUMETRICS",
            (if Volumetrics.Enabled then "1" else "0")));

      File_Substs : File_Substitution_Array :=
        (1 => Create_File_Substitution
           ("scene.glsl", Scenes.Get_GLSL (Scene)));

      Vertex_Shader   : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Vertex_Shader);

      Screen_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);

      Radiance_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);

      Visibility_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);

      Scattering_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);

      Irradiance_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);
   begin
      Glfw.Windows.Context.Make_Current (Window);

      Load_Shader
        (Vertex_Shader,
         "madarch/glsl/identity.glsl",
         No_Macro_Definition_Array,
         No_File_Substitution_Array,
         "120");

      Load_Shader
        (Screen_Shader,
         "madarch/glsl/draw_screen.glsl",
         Probe_Layout_Macros & Render_Macros & Volumetrics_Macros,
         File_Substs,
         "430");

      Load_Shader
        (Radiance_Shader,
         "madarch/glsl/compute_probe_radiance.glsl",
         Probe_Layout_Macros & Probe_Render_Macros & Volumetrics_Macros,
         File_Substs,
         "430");

      Load_Shader
        (Visibility_Shader,
         "madarch/glsl/compute_frustrum_visibility.glsl",
         Probe_Layout_Macros & Volumetrics_Macros,
         File_Substs,
         "430");

      Load_Shader
        (Scattering_Shader,
         "madarch/glsl/accumulate_scattering.glsl",
         Volumetrics_Macros,
         File_Substs,
         "430");

      Load_Shader
        (Irradiance_Shader,
         "madarch/glsl/update_probe_irradiance.glsl",
         Probe_Layout_Macros,
         No_File_Substitution_Array,
         "430");

      return R : Renderer := new Renderer_Internal'
        (Window       => Window,
         Scene        => Scene,

         Camera_Position => (0.0, 0.0, 0.0),
         Camera_Orientation => Singles.Identity3,

         Volumetrics => Volumetrics,

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

         Visibility_Pass => Render_Passes.Create
           (Vertex_Shader   => Vertex_Shader,
            Fragment_Shader => Visibility_Shader,

            Frame_Width  => Volumetrics.Visibility_Resolution (GL.X),
            Frame_Height =>
               Volumetrics.Visibility_Resolution (GL.Y)
               * Volumetrics.Visibility_Resolution (GL.Z),

            Target => 2,

            Texture_Format => GL.Pixels.RGB32F),

         Scattering_Pass => Render_Passes.Create
           (Vertex_Shader   => Vertex_Shader,
            Fragment_Shader => Scattering_Shader,

            Frame_Width  => Volumetrics.Scattering_Resolution (GL.X),
            Frame_Height => Volumetrics.Scattering_Resolution (GL.Y),

            Target => 3,

            Texture_Format => GL.Pixels.RGBA32F),

         Screen_Pass => Render_Passes.Create
           (Vertex_Shader   => Vertex_Shader,
            Fragment_Shader => Screen_Shader,

            Frame_Width  => Int (Window.Width),
            Frame_Height => Int (Window.Height)),

         All_Primitives => <>,

         Last_Material_Index => 0,

         Partitioning_Buffer => Scene_Partitioning_Type.Allocate
           (Kind => GPU_Buffers.Shader_Storage_Buffer, Binding => 0))
      do
         Setup_Probe_Layout (R, Probes);
      end return;
   end Create;

   procedure Render (Self : Renderer) is
   begin
      Glfw.Windows.Context.Make_Current (Self.Window);

      Setup_Camera (Self, Self.Screen_Pass);
      Setup_Camera (Self, Self.Visibility_Pass);
      Setup_Camera (Self, Self.Scattering_Pass);

      Self.Radiance_Pass.Render;
      Self.Irradiance_Pass.Render;

      if Self.Volumetrics.Enabled then
         Self.Visibility_Pass.Render;
         Self.Scattering_Pass.Render;
      end if;

      Self.Screen_Pass.Render;

      Glfw.Windows.Context.Swap_Buffers (Self.Window);
   end Render;

   procedure Write_Value (W : in out GPU_Buffers.Writer; V : Values.Value) is
   begin
      case V.Kind is
         when Values.Vector3_Kind =>
            W.Write_Vec3 (V.Vector3_Value);
         when Values.Float_Kind =>
            W.Write_Float (V.Float_Value);
         when Values.Int_Kind =>
            W.Write_Int (V.Int_Value);
      end case;
   end Write_Value;

   procedure Write_Entity
     (W   : in out GPU_Buffers.Writer;
      Loc : GPU_Types.Locations.Location;
      Ent : Entities.Entity)
   is
      procedure Write_Component (C : Components.Component; V : Values.Value) is
      begin
         Loc.Component (Components.Get_Name (C)).Adjust (W);
         Write_Value (W, V);
      end Write_Component;
   begin
      Entities.Foreach (Ent, Write_Component'Unrestricted_Access);
   end Write_Entity;

   procedure Set_Material
     (Self   : in out Renderer;
      Index  : Materials.Id;
      Entity : Entities.Entity)
   is
      use GPU_Types;

      W : GPU_Buffers.Writer := GPU_Buffers.Start (Self.Materials_Buffer);

      L : Locations.Location := Materials_Description_Type.Address;
   begin
      Write_Entity
        (W,
         L.Component ("materials").Component (Positive (Index + 1)), Entity);

      if Index >= Self.Last_Material_Index then
         Self.Last_Material_Index := Index + 1;
      end if;
   end Set_Material;

   function Add_Material
     (Self   : in out Renderer;
      Entity : Entities.Entity) return Materials.Id
   is
      Index : Materials.Id := Self.Last_Material_Index;
   begin
      Set_Material (Self, Index, Entity);
      return Index;
   end Add_Material;

   procedure Set_Primitive
     (Self   : in out Renderer;
      Prim   : Primitives.Primitive;
      Index  : Positive;
      Entity : Entities.Entity)
   is
      use GPU_Types;

      W : GPU_Buffers.Writer := GPU_Buffers.Start (Self.Scene_Buffer);

      Array_Loc : Locations.Location;
      Count_Loc : Locations.Location;
   begin
      Self.All_Primitives (Prim) (Index) := Entity;

      Scenes.Get_Primitives_Location (Self.Scene, Prim, Array_Loc, Count_Loc);

      Write_Entity
        (W, Array_Loc.Component (Index), Entity);
   end Set_Primitive;

   generic
      with package Vectors is new Ada.Containers.Vectors (<>);
      with package Maps is new Ada.Containers.Hashed_Maps
        (Element_Type => Vectors.Vector, others => <>);
   function Append_Element_G
     (M : in out Maps.Map;
      K : Maps.Key_Type;
      V : Vectors.Element_Type) return Natural;

   function Append_Element_G
     (M : in out Maps.Map;
      K : Maps.Key_Type;
      V : Vectors.Element_Type) return Natural
   is
      Res : Natural;

      procedure Add_It
        (K : Maps.Key_Type; E : in out Vectors.Vector)
      is
      begin
         E.Append (V);
         Res := Natural (E.Length);
      end Add_It;

      Cursor   : Maps.Cursor;
      Inserted : Boolean;
   begin
      M.Insert (K, Cursor, Inserted);
      M.Update_Element (Cursor, Add_It'Access);
      return Res;
   end Append_Element_G;

   function Append_Entity is new Append_Element_G
     (Entity_Vectors, Primitive_Entity_Maps);

   procedure Add_Primitive
     (Self   : in out Renderer;
      Prim   : Primitives.Primitive;
      Entity : Entities.Entity)
   is
      use GPU_Types;

      Count : Natural := Append_Entity (Self.All_Primitives, Prim, Entity);

      W : GPU_Buffers.Writer := GPU_Buffers.Start (Self.Scene_Buffer);

      Array_Loc : Locations.Location;
      Count_Loc : Locations.Location;
   begin
      Scenes.Get_Primitives_Location (Self.Scene, Prim, Array_Loc, Count_Loc);

      Write_Entity
        (W, Array_Loc.Component (Count), Entity);

      Count_Loc.Adjust (W);
      W.Write_Int (Int (Count));
   end Add_Primitive;

   procedure Set_Light
     (Self   : in out Renderer;
      Index  : Positive;
      Lit    : Lights.Light;
      Entity : Entities.Entity)
   is
      use GPU_Types;

      W : GPU_Buffers.Writer := GPU_Buffers.Start (Self.Scene_Buffer);

      Array_Loc : Locations.Location;
      Count_Loc : Locations.Location;
      Total_Loc : Locations.Location;
   begin
      Scenes.Get_Lights_Location
        (Self.Scene, Lit, Array_Loc, Count_Loc, Total_Loc);

      Write_Entity
        (W, Array_Loc.Component (Index), Entity);

      Count_Loc.Adjust (W);
      W.Write_Int (Int (Index));

      Total_Loc.Adjust (W);
      W.Write_Int (Int (Index));
   end Set_Light;

   procedure Set_Camera_Position
     (Self : in out Renderer; Position : Singles.Vector3)
   is
   begin
      Self.Camera_Position := Position;
   end Set_Camera_Position;

   procedure Set_Camera_Orientation
     (Self : in out Renderer; Orientation : Singles.Matrix3)
   is
   begin
      Self.Camera_Orientation := Orientation;
   end Set_Camera_Orientation;

   package Natural_Vectors is new Ada.Containers.Vectors (Positive, Natural);
   package Primitive_Natural_Maps is new Ada.Containers.Hashed_Maps
     (Primitives.Primitive,
      Natural_Vectors.Vector,
      Primitives.Hash,
      Primitives."=",
      Natural_Vectors."=");

   function Append_Natural is new Append_Element_G
     (Natural_Vectors, Primitive_Natural_Maps);

   procedure Update_Partionning (Self : in out Renderer) is
      function To_Vec (X, Y, Z : Integer) return Singles.Vector3 is
        ((Single (X), Single (Y), Single (Z)));

      W : GPU_Buffers.Writer := GPU_Buffers.Start (Self.Partitioning_Buffer);

      Settings : constant Scenes.Partitioning_Settings :=
         Scenes.Get_Partitioning_Settings (Self.Scene);

      procedure Process_Point (X, Y, Z : Integer) is
         Candidates : Primitive_Natural_Maps.Map;

         Point_Loc : GPU_Types.Locations.Location :=
            Scenes.Get_Partitioning_GPU_Type (Self.Scene).Address.Component
              (  X * Integer (Settings.Grid_Dimensions (GL.Y)
                              * Settings.Grid_Dimensions (GL.Z))
               + Y * Integer (Settings.Grid_Dimensions (GL.Z))
               + Z + 1);

         Array_Loc : GPU_Types.Locations.Location :=
            Point_Loc.Component ("indices");

         Index : Positive := 1;

         procedure Write_Partitioning_Info (Prim : Primitives.Primitive) is
            Count_Loc : GPU_Types.Locations.Location :=
               Point_Loc.Component (Primitives.Get_Name (Prim) & "_count");

            Cursor : Primitive_Natural_Maps.Cursor :=
               Candidates.Find (Prim);

            Vec : Natural_Vectors.Vector;
         begin
            if Primitive_Natural_Maps.Has_Element (Cursor) then
               Vec := Primitive_Natural_Maps.Element (Cursor);
            end if;
            Count_Loc.Adjust (W);
            W.Write_Int (Int (Vec.Length));
            for E of Vec loop
               Array_Loc.Component (Index).Adjust (W);
               W.Write_Int (Int (E));
               Index := Index + 1;
            end loop;
         end Write_Partitioning_Info;

         use type Singles.Vector3;

         Grid_Pos : Singles.Vector3 :=
            Math_Utils.Mul (To_Vec (X, Y, Z), Settings.Grid_Spacing)
            + Settings.Grid_Offset;

         Cell_Diag : Single := Math_Utils.Length (Settings.Grid_Spacing);

         Center  : Singles.Vector3 :=
            Grid_Pos + Settings.Grid_Spacing * 0.5;

         Closest : Single := 1.0e10;

         procedure Find_Closest (Cursor : Primitive_Entity_Maps.Cursor) is
            Prim : Primitives.Primitive := Primitive_Entity_Maps.Key (Cursor);
            Dist : Single;
         begin
            for Ent of Primitive_Entity_Maps.Element (Cursor) loop
               Dist := Primitives.Eval_Dist (Prim, Ent, Center);
               if Dist < Closest then
                  Closest := Dist;
               end if;
            end loop;
         end Find_Closest;

         type Precandidate is record
            Prim  : Primitives.Primitive;
            Ent   : Entities.Entity;
            Index : Natural;
         end record;

         use type Ada.Containers.Hash_Type;

         function Hash (X : Precandidate) return Ada.Containers.Hash_Type is
           (Primitives.Hash (X.Prim) + Ada.Containers.Hash_Type (X.Index));

         package Precandidate_Vectors is new Ada.Containers.Vectors
           (Positive, Precandidate);

         package Precandidate_Sets is new Ada.Containers.Hashed_Sets
           (Precandidate, Hash, "=");

         Precandidates : Precandidate_Vectors.Vector;

         procedure Find_Precandidates (Cursor : Primitive_Entity_Maps.Cursor) is
            Prim : Primitives.Primitive := Primitive_Entity_Maps.Key (Cursor);
            Dist : Single;

            Dummy_Count : Natural;
            Index : Natural := 0;
         begin
            for Ent of Primitive_Entity_Maps.Element (Cursor) loop
               Dist := Primitives.Eval_Dist (Prim, Ent, Center);
               if Dist < Closest + Cell_Diag then
                  Precandidates.Append ((Prim, Ent, Index));
               end if;
               Index := Index + 1;
            end loop;
         end Find_Precandidates;

         procedure Find_Candidates is
            use type Ada.Containers.Count_Type;

            Accepted : Precandidate_Sets.Set;

            procedure Test_Point (P : Singles.Vector3) is
               Closest       : Single  := 1.0e10;
               Closest_Index : Natural := 0;

               Dist          : Single   := 0.0;
               Index         : Positive := 1;
            begin
               for C of Precandidates loop
                  Dist := Primitives.Eval_Dist (C.Prim, C.Ent, P);
                  if Dist < Closest then
                     Closest := Dist;
                     Closest_Index := Index;
                  end if;
                  Index := Index + 1;
               end loop;

               declare
                  Candidate : Precandidate := Precandidates (Closest_Index);
                  Inserted  : Boolean;

                  Dummy_Cursor : Precandidate_Sets.Cursor;
                  Dummy_Count  : Natural;
               begin
                  Accepted.Insert (Candidate, Dummy_Cursor, Inserted);
                  if Inserted then
                     Dummy_Count := Append_Natural
                       (Candidates, Candidate.Prim, Candidate.Index);
                  end if;
               end;
            end Test_Point;

            Res : constant Singles.Vector3 := (3.0, 3.0, 3.0);
            One : constant Singles.Vector3 := (1.0, 1.0, 1.0);
         begin
            for SX in 1 .. Integer (Res (GL.X)) loop
               for SY in 1 .. Integer (Res (GL.Y)) loop
                  for SZ in 1 .. Integer (Res (GL.Z)) loop
                     declare
                        Cmp : Singles.Vector3 := To_Vec (SX, SY, SZ);
                        Off : Singles.Vector3 := Math_Utils.Div
                          (Cmp - One, Res - One);
                     begin
                        Test_Point
                          (Math_Utils.Mul (Off, Settings.Grid_Spacing)
                           + Grid_Pos);
                     end;
                  end loop;
               end loop;
            end loop;
         end Find_Candidates;
      begin
         Self.All_Primitives.Iterate (Find_Closest'Access);
         Self.All_Primitives.Iterate (Find_Precandidates'Access);
         Find_Candidates;
         for Prim of Scenes.Get_Primitives (Self.Scene) loop
            Write_Partitioning_Info (Prim);
         end loop;
      end Process_Point;
   begin
      if not Settings.Enable then
         return;
      end if;
      for X in 0 .. Settings.Grid_Dimensions (GL.X) - 1 loop
         for Y in 0 .. Settings.Grid_Dimensions (GL.Y) - 1 loop
            for Z in 0 .. Settings.Grid_Dimensions (GL.Z) - 1 loop
               Process_Point (Integer (X), Integer (Y), Integer (Z));
            end loop;
         end loop;
      end loop;
   end Update_Partionning;
end Madarch.Renderers;
