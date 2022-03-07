with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with GL;

with Madarch.Components; use Madarch.Components;
with Madarch.Values; use Madarch.Values;
with Madarch.Entities; use Madarch.Entities;

package Madarch.Exprs is
   type Expr is tagged private;
   type Struct_Expr is tagged private;

   type Eval_Context is private;

   Empty_Context : constant Eval_Context;

   function Append
     (Self : Eval_Context;
      Key  : Unbounded_String;
      Val  : Entity) return Eval_Context;

   function Append
     (Self : Eval_Context;
      Key  : Unbounded_String;
      Val  : Value) return Eval_Context;

   procedure Free (Ctx : in out Eval_Context);

   function Eval (E : Expr; Ctx : Eval_Context) return Value;
   function Pre_GLSL (E : Expr) return String;
   function To_GLSL  (E : Expr) return String;

   function Literal (V : Value) return Expr;

   function Value_Identifier (N : String) return Expr;
   function Value_Identifier (N : Unbounded_String) return Expr;

   function Struct_Identifier (N : String) return Struct_Expr;
   function Struct_Identifier (N : Unbounded_String) return Struct_Expr;

   function Construct_Vector3 (X, Y, Z : Expr) return Expr;

   function "+" (L, R : Expr) return Expr;
   function "-" (L, R : Expr) return Expr;
   function "*" (L, R : Expr) return Expr;
   function "/" (L, R : Expr) return Expr;
   function "**" (L, R : Expr) return Expr;

   function "<"  (L, R : Expr) return Expr;
   function ">"  (L, R : Expr) return Expr;
   function "<=" (L, R : Expr) return Expr;
   function ">=" (L, R : Expr) return Expr;

   function "+" (L : Expr; R : Value) return Expr is (L + Literal (R));
   function "-" (L : Expr; R : Value) return Expr is (L - Literal (R));
   function "*" (L : Expr; R : Value) return Expr is (L * Literal (R));
   function "/" (L : Expr; R : Value) return Expr is (L / Literal (R));
   function "**" (L : Expr; R : Value) return Expr is (L ** Literal (R));

   function Dot (L, R : Expr) return Expr;
   function Cross (L, R : Expr) return Expr;
   function Min (L, R : Expr) return Expr;
   function Max (L, R : Expr) return Expr;
   function Clamp (E, LB, UB : Expr) return Expr;

   function "-" (E : Expr) return Expr;
   function Length (E : Expr) return Expr;
   function Normalize (E : Expr) return Expr;
   function Abs_Value (E : Expr) return Expr;
   function To_Float (E : Expr) return Expr;
   function Sign (E : Expr) return Expr;

   function Sin (E : Expr) return Expr;
   function Cos (E : Expr) return Expr;
   function Tan (E : Expr) return Expr;
   function Asin (E : Expr) return Expr;
   function Acos (E : Expr) return Expr;
   function Atan (E : Expr) return Expr;

   function Get (E : Struct_Expr; C : Component) return Expr'Class;
   function Get (E : Expr; C : GL.Index_3D) return Expr;

   function Let_In
     (Value   : Expr;
      Kind    : Value_Kind;
      Name    : String;
      In_Body : Expr) return Expr;

   type Var_Decl is record
      Kind  : Value_Kind;
      Name  : Unbounded_String;
      Value : Expr;
   end record;

   type Var_Decl_Array is array (Positive range <>) of Var_Decl;

   function Let_In (Vars : Var_Decl_Array; In_Body : Expr) return Expr;

private
   type Expr_Node is abstract tagged null record;
   type Expr_Access is access all Expr_Node'Class;

   type Eval_Context_Node;
   type Eval_Context_Node_Access is access Eval_Context_Node;

   type Eval_Context is record
      Ref : Eval_Context_Node_Access := null;
   end record;

   type Eval_Context_Node (K : Boolean) is record
      Parent : Eval_Context;
      Name   : Unbounded_String;
      case K is
         when True =>
            Ent : Entity;
         when False =>
            Val : Value;
      end case;
   end record;

   Empty_Context : constant Eval_Context := (Ref => null);

   function Eval (E : Expr_Node; Ctx : Eval_Context) return Value is abstract;
   function Pre_GLSL (E : Expr_Node) return String is ("");
   function To_GLSL  (E : Expr_Node) return String is abstract;

   type Expr is tagged record
      Value : Expr_Access;
   end record;

   type Expr_Array is array (Positive range <>) of Expr;
   type Expr_Array_Access is access Expr_Array;

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

   type Bin_Op_Kind is (Bin_Add, Bin_Sub, Bin_Mul, Bin_Div,
                        Bin_Lt, Bin_Gt, Bin_Lte, Bin_Gte);

   type Bin_Op is new Expr_Node with record
      Op : Bin_Op_Kind;
      Lhs, Rhs : Expr;
   end record;

   function Eval (B : Bin_Op; Ctx : Eval_Context) return Value;
   function Pre_GLSL (B : Bin_Op) return String;
   function To_GLSL  (B : Bin_Op) return String;

   type Un_Op_Kind is (Un_Min, Un_Length, Un_Normalize, Un_Abs,
                       Un_Float, Un_Sign);

   type Un_Op is new Expr_Node with record
      Op : Un_Op_Kind;
      E  : Expr;
   end record;

   function Eval (U : Un_Op; Ctx : Eval_Context) return Value;
   function Pre_GLSL (U : Un_Op) return String;
   function To_GLSL  (U : Un_Op) return String;

   type Builtin_Kind is (Builtin_Dot, Builtin_Cross,
                         Builtin_Min, Builtin_Max, Builtin_Clamp,
                         Builtin_Pow,
                         Builtin_Sin, Builtin_Cos, Builtin_Tan,
                         Builtin_Asin, Builtin_Acos, Builtin_Atan,
                         Builtin_Vec3);

   type Builtin_Call is new Expr_Node with record
      Builtin : Builtin_Kind;
      Args    : Expr_Array_Access;
   end record;

   function Eval (B : Builtin_Call; Ctx : Eval_Context) return Value;
   function Pre_GLSL (B : Builtin_Call) return String;
   function To_GLSL  (B : Builtin_Call) return String;

   type Project_Axis is new Expr_Node with record
      E : Expr;
      A : GL.Index_3D;
   end record;

   function Eval (P : Project_Axis; Ctx : Eval_Context) return Value;
   function Pre_GLSL (P : Project_Axis) return String;
   function To_GLSL  (P : Project_Axis) return String;

   type Struct_Expr is tagged record
      Name : Unbounded_String;
   end record;

   type Get_Component is new Expr_Node with record
      Prefix : Struct_Expr;
      Suffix : Component;
   end record;

   function Eval (G : Get_Component; Ctx : Eval_Context) return Value;
   function To_GLSL  (G : Get_Component) return String;

   type Var_Body is new Expr_Node with record
      Value   : Expr;
      Kind    : Value_Kind;
      Name    : Unbounded_String;
      In_Body : Expr;
   end record;

   function Eval (V : Var_Body; Ctx : Eval_Context) return Value;
   function Pre_GLSL (V : Var_Body) return String;
   function To_GLSL  (V : Var_Body) return String;
end Madarch.Exprs;
