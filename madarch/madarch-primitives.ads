with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Madarch.Exprs;
with Madarch.Components;
with Madarch.Entities;

with GL.Types;

package Madarch.Primitives is
   use GL.Types;

   type Primitive is private;
   type Primitive_Array is array (Positive range <>) of Primitive;
   type Primitive_Array_Access is access all Primitive_Array;

   type Primitive_Point_Function is access function
     (S : Exprs.Struct_Expr;
      P : Exprs.Expr'Class) return Exprs.Expr'Class;

   type Primitive_Function is access function
     (S : Exprs.Struct_Expr) return Exprs.Expr'Class;

   function Create
     (Name     : String;
      Comps    : Components.Component_Array;
      Distance : Primitive_Point_Function;
      Normal   : Primitive_Point_Function;
      Material : Primitive_Function) return Primitive;

   function Get_Name (Prim : Primitive) return String;

   function Get_Components
     (Prim : Primitive) return Components.Component_Array;

   function Get_Dist_Expr
     (Prim  : Primitive;
      Inst  : Exprs.Struct_Expr;
      Point : Exprs.Expr) return Exprs.Expr'Class;

   function Get_Normal_Expr
     (Prim  : Primitive;
      Inst  : Exprs.Struct_Expr;
      Point : Exprs.Expr) return Exprs.Expr'Class;

   function Get_Material_Expr
     (Prim : Primitive;
      Inst : Exprs.Struct_Expr) return Exprs.Expr'Class;

   function Eval_Dist
     (Prim   : Primitive;
      Entity : Entities.Entity;
      Point  : Singles.Vector3) return Single;

   function Hash (Prim : Primitive) return Ada.Containers.Hash_Type;
private
   type Primitive_Internal is record
      Name     : Unbounded_String;
      Comps    : Components.Component_Array_Access;
      Distance : Primitive_Point_Function;
      Normal   : Primitive_Point_Function;
      Material : Primitive_Function;
   end record;

   type Primitive is access Primitive_Internal;
end Madarch.Primitives;
