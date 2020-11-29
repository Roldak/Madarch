with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Madarch.Components; use Madarch.Components;
with Madarch.Values; use Madarch.Values;

package Madarch.Exprs is
   type Expr is tagged private;
   type Struct_Expr is tagged private;

   type Eval_Context is private;

   function Eval (E : Expr; Ctx : Eval_Context) return Value;
   function To_GLSL (E : Expr) return String;

   function Literal (V : Value) return Expr;
   function Value_Identifier (N : String) return Expr;
   function Struct_Identifier (N : String) return Struct_Expr;

   function "+" (L, R : Expr) return Expr;
   function "-" (L, R : Expr) return Expr;
   function "*" (L, R : Expr) return Expr;
   function "/" (L, R : Expr) return Expr;

   function Length (E : Expr) return Expr;
   function Normalize (E : Expr) return Expr;

   function Get (E : Struct_Expr; C : Component) return Expr'Class;

private
   type Expr_Node is abstract tagged null record;
   type Expr_Access is access all Expr_Node'Class;

   type Eval_Context is record
      null;
   end record;

   function Eval (E : Expr_Node; Ctx : Eval_Context) return Value is abstract;
   function To_GLSL (E : Expr_Node) return String is abstract;

   type Expr is tagged record
      Value : Expr_Access;
   end record;

   type Ident is new Expr_Node with record
      Name : Unbounded_String;
   end record;

   function Eval (I : Ident; Ctx : Eval_Context) return Value;
   function To_GLSL (I : Ident) return String;

   type Lit is new Expr_Node with record
      V : Value;
   end record;

   function Eval (L : Lit; Ctx : Eval_Context) return Value;
   function To_GLSL (L : Lit) return String;

   type Bin_Op_Kind is (Bin_Add, Bin_Sub, Bin_Mul, Bin_Div);

   type Bin_Op is new Expr_Node with record
      Op : Bin_Op_Kind;
      Lhs, Rhs : Expr;
   end record;

   function Eval (B : Bin_Op; Ctx : Eval_Context) return Value;
   function To_GLSL (B : Bin_Op) return String;

   type Un_Op_Kind is (Un_Min, Un_Length, Un_Normalize);

   type Un_Op is new Expr_Node with record
      Op : Un_Op_Kind;
      E  : Expr;
   end record;

   function Eval (U : Un_Op; Ctx : Eval_Context) return Value;
   function To_GLSL (U : Un_Op) return String;

   type Struct_Expr is tagged record
      Name : Unbounded_String;
   end record;

   type Get_Component is new Expr_Node with record
      Prefix : Struct_Expr;
      Suffix : Component;
   end record;

   function Eval (G : Get_Component; Ctx : Eval_Context) return Value;
   function To_GLSL (G : Get_Component) return String;
end Madarch.Exprs;
