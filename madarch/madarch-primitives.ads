with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Madarch.Exprs;
with Madarch.Components;

package Madarch.Primitives is
   type Primitive is private;

   type Struct_Point_Function is access function
     (S : Exprs.Struct_Expr;
      P : Exprs.Expr'Class) return Exprs.Expr'Class;

   function Create
     (Name     : String;
      Comps    : Components.Component_Array;
      Distance : Struct_Point_Function;
      Normal   : Struct_Point_Function) return Primitive;
private
   type Primitive_Internal is record
      Name     : Unbounded_String;
      Comps    : Components.Component_Array_Access;
      Distance : Struct_Point_Function;
      Normal   : Struct_Point_Function;
   end record;

   type Primitive is access Primitive_Internal;
end Madarch.Primitives;
