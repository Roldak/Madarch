with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Madarch.Primitives;

package Madarch.Scenes is
   type Scene is private;

   type Primitive_Count is record
      Prim  : Primitives.Primitive;
      Count : Natural;
   end record;

   type Primitive_Count_Array is array (Positive range <>) of Primitive_Count;

   function Compile
     (Prims_Count : Primitive_Count_Array) return Scene;

   procedure Print_GLSL (S : Scene);
private
   type Scene_Internal is record
      Prims : Primitives.Primitive_Array_Access;
      GLSL  : Unbounded_String;
   end record;

   type Scene is access Scene_Internal;
end Madarch.Scenes;
