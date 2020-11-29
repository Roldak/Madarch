with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Madarch.Values;

package Madarch.Components is
   type Component is private;
   type Component_Array is array (Positive range <>) of Component;
   type Component_Array_Access is access all Component_Array;

   function Create
     (Name : String;
      Kind : Values.Value_Kind) return Component;

   function Get_Name (C : Component) return String;

   type Component_Value is record
      Comp : Component;
      Val  : Values.Value;
   end record;

   type Component_Value_Array is array (Positive range <>) of Component_Value;
private
   type Component_Internal is record
      Name : Unbounded_String;
      Kind : Values.Value_Kind;
   end record;

   type Component is access all Component_Internal;
end Madarch.Components;
