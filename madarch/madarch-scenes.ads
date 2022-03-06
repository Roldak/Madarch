with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Madarch.Primitives;
with Madarch.Lights;

with GL.Types;

with GPU_Types;

package Madarch.Scenes is
   use GL.Types;

   type Scene is private;

   type Primitive_Count is record
      Prim  : Primitives.Primitive;
      Count : Natural;
   end record;

   type Light_Count is record
      Light : Lights.Light;
      Count : Natural;
   end record;

   type Primitive_Count_Array is array (Positive range <>) of Primitive_Count;
   type Light_Count_Array is array (Positive range <>) of Light_Count;

   type Partitioning_Border_Behavior is (Clamp, Fallback);

   type Partitioning_Settings (Enable : Boolean := True) is record
      case Enable is
         when True =>
            Index_Count     : Natural := 10;
            Border_Behavior : Partitioning_Border_Behavior := Clamp;
            Grid_Dimensions : Ints.Vector3    := (10, 10, 20);
            Grid_Spacing    : Singles.Vector3 := (1.0, 1.0, 1.0);
            Grid_Offset     : Singles.Vector3 := (-1.5, -1.5, -10.0);
         when others =>
            null;
      end case;
   end record;

   Default_Partitioning_Settings : constant Partitioning_Settings;

   type Codegen_Loop_Strategy is (Split, Unify);

   function Compile
     (All_Primitives : Primitive_Count_Array;
      All_Lights     : Light_Count_Array;
      Partitioning   : Partitioning_Settings := Default_Partitioning_Settings;
      Max_Dist       : Single := 20.0;
      Loop_Strategy  : Codegen_Loop_Strategy := Unify;
      Print_GLSL     : Boolean := False) return Scene;

   function Get_GLSL (S : Scene) return String;

   function Get_GPU_Type (S : Scene) return GPU_Types.GPU_Type;

   function Get_Partitioning_Settings (S : Scene) return Partitioning_Settings;

   function Get_Partitioning_GPU_Type (S : Scene) return GPU_Types.GPU_Type;

   procedure Get_Primitives_Location
     (S    : Scene;
      Prim : Primitives.Primitive;
      Array_Location : out GPU_Types.Locations.Location;
      Count_Location : out GPU_Types.Locations.Location);

   procedure Get_Lights_Location
     (S    : Scene;
      Lit  : Lights.Light;
      Array_Location : out GPU_Types.Locations.Location;
      Count_Location : out GPU_Types.Locations.Location;
      Total_Location : out GPU_Types.Locations.Location);

   function Get_Primitives (S : Scene) return Primitives.Primitive_Array;
private
   type Scene_Internal is record
      Prims    : Primitives.Primitive_Array_Access;
      Lits     : Lights.Light_Array_Access;
      GLSL     : Unbounded_String;
      GPU_Type : GPU_Types.GPU_Type;

      Partitioning_Config   : Partitioning_Settings;
      Partitioning_GPU_Type : GPU_Types.GPU_Type;
   end record;

   type Scene is access Scene_Internal;

   Default_Partitioning_Settings : constant Partitioning_Settings :=
     (Enable => True, others => <>);
end Madarch.Scenes;
