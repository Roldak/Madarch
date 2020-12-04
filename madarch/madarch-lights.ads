with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Madarch.Exprs;
with Madarch.Components;

package Madarch.Lights is
   type Light is private;
   type Light_Array is array (Positive range <>) of Light;
   type Light_Array_Access is access all Light_Array;

   type Light_Sample_Function is access function
     (L : Exprs.Struct_Expr;
      P : Exprs.Expr'Class;
      N : Exprs.Expr'Class) return Exprs.Expr'Class;

   type Light_Position_Function is access function
     (L : Exprs.Struct_Expr) return Exprs.Expr'Class;

   function Create
     (Name     : String;
      Comps    : Components.Component_Array;
      Sample   : Light_Sample_Function;
      Position : Light_Position_Function) return Light;

   function Get_Name (L : Light) return String;

   function Get_Components
     (L : Light) return Components.Component_Array;

   function Get_Sample_Expr
     (L     : Light;
      Inst  : Exprs.Struct_Expr;
      Point : Exprs.Expr;
      Norm  : Exprs.Expr) return Exprs.Expr'Class;

   function Get_Position_Expr
     (L : Light; Inst : Exprs.Struct_Expr) return Exprs.Expr'Class;
private
   type Light_Internal is record
      Name     : Unbounded_String;
      Comps    : Components.Component_Array_Access;
      Sample   : Light_Sample_Function;
      Position : Light_Position_Function;
   end record;

   type Light is access Light_Internal;
end Madarch.Lights;
