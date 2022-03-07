with Ada.Unchecked_Deallocation;

with GL;
with GL.Types;

package body Madarch.Exprs is
   procedure Free is new Ada.Unchecked_Deallocation
     (Eval_Context_Node, Eval_Context_Node_Access);

   function Append
     (Self : Eval_Context;
      Key  : Unbounded_String;
      Val  : Entity) return Eval_Context
   is
   begin
      return (Ref => new Eval_Context_Node'
        (K      => True,
         Parent => Self,
         Name   => Key,
         Ent    => Val));
   end Append;

   function Append
     (Self : Eval_Context;
      Key  : Unbounded_String;
      Val  : Value) return Eval_Context
   is
   begin
      return (Ref => new Eval_Context_Node'
        (K      => False,
         Parent => Self,
         Name   => Key,
         Val    => Val));
   end Append;

   procedure Free (Ctx : in out Eval_Context) is
   begin
      if Ctx.Ref /= null then
         Free (Ctx.Ref.Parent);
         Free (Ctx.Ref);
      end if;
   end Free;

   function Get
     (Ctx : Eval_Context;
      Exp : Struct_Expr) return Entity
   is
      use Ada.Strings.Unbounded;
   begin
      if Ctx.Ref = null then
         raise Program_Error
            with "Key '" & To_String (Exp.Name)
                 & "' not in eval context entities";
      elsif Ctx.Ref.Name = Exp.Name then
         return Ctx.Ref.Ent;
      else
         return Get (Ctx.Ref.Parent, Exp);
      end if;
   end Get;

   function Get
     (Ctx : Eval_Context;
      Exp : Ident) return Value
   is
      use Ada.Strings.Unbounded;
   begin
      if Ctx.Ref = null then
         raise Program_Error
            with "Key '" & To_String (Exp.Name)
                 & "' not in eval context values";
      elsif Ctx.Ref.Name = Exp.Name then
         return Ctx.Ref.Val;
      else
         return Get (Ctx.Ref.Parent, Exp);
      end if;
   end Get;

   function Literal (V : Value) return Expr is
     (Value => new Lit'(V => V));

   function Value_Identifier (N : String) return Expr is
     (Value => new Ident'(Name => To_unbounded_String (N)));

   function Value_Identifier (N : Unbounded_String) return Expr is
     (Value => new Ident'(Name => N));

   function Struct_Identifier (N : String) return Struct_Expr is
     (Struct_Expr'(Name => To_unbounded_String (N)));

   function Struct_Identifier (N : Unbounded_String) return Struct_Expr is
     (Struct_Expr'(Name => N));

   function Construct_Vector3 (X, Y, Z : Expr) return Expr is
     (Value => new Builtin_Call'
        (Builtin_Vec3, new Expr_Array'(X, Y, Z)));

   function Eval (E : Expr; Ctx : Eval_Context) return Value is
     (E.Value.Eval (Ctx));

   function Pre_GLSL (E : Expr) return String is
     (E.Value.Pre_GLSL);

   function To_GLSL (E : Expr) return String is
     (E.Value.To_GLSL);

   function "+" (L, R : Expr) return Expr is
     (Value => new Bin_Op'(Bin_Add, L, R));

   function "-" (L, R : Expr) return Expr is
     (Value => new Bin_Op'(Bin_Sub, L, R));

   function "*" (L, R : Expr) return Expr is
     (Value => new Bin_Op'(Bin_Mul, L, R));

   function "/" (L, R : Expr) return Expr is
     (Value => new Bin_Op'(Bin_Div, L, R));

   function "**" (L, R : Expr) return Expr is
     (Value => new Builtin_Call'(Builtin_Pow,
                                 new Expr_Array'(L, R)));

   function "<"  (L, R : Expr) return Expr is
     (Value => new Bin_Op'(Bin_Lt, L, R));

   function ">"  (L, R : Expr) return Expr is
     (Value => new Bin_Op'(Bin_Gt, L, R));

   function "<=" (L, R : Expr) return Expr is
     (Value => new Bin_Op'(Bin_Lte, L, R));

   function ">=" (L, R : Expr) return Expr is
     (Value => new Bin_Op'(Bin_Gte, L, R));

   function Dot (L, R : Expr) return Expr is
     (Value => new Builtin_Call'(Builtin_Dot,
                                 new Expr_Array'((L, R))));

   function Cross (L, R : Expr) return Expr is
     (Value => new Builtin_Call'(Builtin_Cross,
                                 new Expr_Array'((L, R))));

   function Min (L, R : Expr) return Expr is
     (Value => new Builtin_Call'(Builtin_Min,
                                 new Expr_Array'((L, R))));

   function Max (L, R : Expr) return Expr is
     (Value => new Builtin_Call'(Builtin_Max,
                                 new Expr_Array'((L, R))));

   function Clamp (E, LB, UB : Expr) return Expr is
     (Value => new Builtin_Call'(Builtin_Clamp,
                                 new Expr_Array'((E, LB, UB))));

   function "-" (E : Expr) return Expr is
     (Value => new Un_Op'(Un_Min, E));

   function Length (E : Expr) return Expr is
     (Value => new Un_Op'(Un_Length, E));

   function Normalize (E : Expr) return Expr is
     (Value => new Un_Op'(Un_Normalize, E));

   function Abs_Value (E : Expr) return Expr is
     (Value => new Un_Op'(Un_Abs, E));

   function To_Float (E : Expr) return Expr is
     (Value => new Un_Op'(Un_Float, E));

   function Sign (E : Expr) return Expr is
     (Value => new Un_Op'(Un_Sign, E));

   function Builtin_Call_Single_Arg
     (Kind : Builtin_Kind; E : Expr) return Expr
   is (Value => new Builtin_Call'(Kind,
                                  new Expr_Array'(1 => E)));

   function Sin (E : Expr) return Expr is
     (Builtin_Call_Single_Arg (Builtin_Sin, E));

   function Cos (E : Expr) return Expr is
     (Builtin_Call_Single_Arg (Builtin_Cos, E));

   function Tan (E : Expr) return Expr is
     (Builtin_Call_Single_Arg (Builtin_Tan, E));

   function Asin (E : Expr) return Expr is
     (Builtin_Call_Single_Arg (Builtin_Asin, E));

   function Acos (E : Expr) return Expr is
     (Builtin_Call_Single_Arg (Builtin_Acos, E));

   function Atan (E : Expr) return Expr is
     (Builtin_Call_Single_Arg (Builtin_Atan, E));

   function Get (E : Struct_Expr; C : Component) return Expr'Class is
      R : Expr := (Value => new Get_Component'(E, C));
   begin
      return R;
   end Get;

   function Get (E : Expr; C : GL.Index_3D) return Expr is
     (Value => new Project_Axis'(E, C));

   function Let_In
     (Value   : Expr;
      Kind    : Value_Kind;
      Name    : String;
      In_Body : Expr) return Expr
   is
   begin
      return
        (Value => new Var_Body'
           (Value, Kind, To_Unbounded_String (Name), In_Body));
   end Let_In;

   --  Ident

   function Eval (I : Ident; Ctx : Eval_Context) return Value is
     (Get (Ctx, I));

   function To_GLSL (I : Ident) return String is
     (To_String (I.Name));

   --  Lit

   function GLSL_Single (X : GLT.Single) return String is
     (GLT.Single'Image (X));

   function GLSL_Vector3 (V : GLT.Singles.Vector3) return String is
     ("vec3(" & GLSL_Single (V (GL.X)) &
      ", "    & GLSL_Single (V (GL.Y)) &
      ", "    & GLSL_Single (V (GL.Z)) & ")");

   function Eval (L : Lit; Ctx : Eval_Context) return Value is (L.V);

   function To_GLSL (L : Lit) return String is
   begin
      case L.V.Kind is
         when Vector3_Kind =>
            return GLSL_Vector3 (L.V.Vector3_Value);
         when Float_Kind =>
            return GLSL_Single (L.V.Float_Value);
         when Int_Kind =>
            return GLT.Int'Image (L.V.Int_Value);
      end case;
   end To_GLSL;

   --  Bin_Op

   function Eval (B : Bin_Op; Ctx : Eval_Context) return Value is
      L_Value : Value := B.Lhs.Eval (Ctx);
      R_Value : Value := B.Rhs.Eval (Ctx);
   begin
      case B.Op is
         when Bin_Add =>
            return L_Value + R_Value;
         when Bin_Sub =>
            return L_Value - R_Value;
         when Bin_Mul =>
            return L_Value * R_Value;
         when Bin_Div =>
            return L_Value / R_Value;
         when Bin_Lt =>
            return L_Value < R_Value;
         when Bin_Gt =>
            return L_Value > R_Value;
         when Bin_Lte =>
            return L_Value <= R_Value;
         when Bin_Gte =>
            return L_Value >= R_Value;
      end case;
   end Eval;

   function Pre_GLSL (B : Bin_Op) return String is
      Pre_LHS : String := B.Lhs.Pre_GLSL;
      Pre_RHS : String := B.Rhs.Pre_GLSL;
   begin
      return Pre_LHS & Pre_RHS;
   end Pre_GLSL;

   function To_GLSL (B : Bin_Op) return String is
      L_GLSL : String := B.Lhs.To_GLSL;
      R_GLSL : String := B.Rhs.To_GLSL;
      O_GLSL : String := (case B.Op is
         when Bin_Add => "+",
         when Bin_Sub => "-",
         when Bin_Mul => "*",
         when Bin_Div => "/",
         when Bin_Lt  => "<",
         when Bin_Gt  => ">",
         when Bin_Lte => "<=",
         when Bin_Gte => ">=");
   begin
      return "(" & L_GLSL & ")" & O_GLSL & "(" & R_GLSL & ")";
   end To_GLSL;

   --  Un_Op

   function Eval (U : Un_Op; Ctx : Eval_Context) return Value is
      use type GL.Types.Single;

      E_Value : Value := U.E.Eval (Ctx);
   begin
      case U.Op is
         when Un_Min =>
            return -E_Value;
         when Un_Length =>
            return Length (E_Value);
         when Un_Normalize =>
            return Normalize (E_Value);
         when Un_Abs =>
            return Abs_Value (E_Value);
         when Un_Float =>
            return (case E_Value.Kind is
               when Vector3_Kind =>
                  raise Program_Error with "Invalid cast.",
               when Float_Kind =>
                  E_Value,
               when Int_Kind =>
                  Values.Float (GL.Types.Single (E_Value.Int_Value)));
         when Un_Sign =>
            return (case E_Value.Kind is
               when Float_Kind =>
                  Values.Float
                    (if E_Value.Float_Value < 0.0 then -1.0
                     elsif E_Value.Float_Value = 0.0 then 0.0
                     else 1.0),
               when others =>
                  raise Program_Error with "sign not applicable.");
      end case;
   end Eval;

   function Pre_GLSL (U : Un_Op) return String is
   begin
      return U.E.Pre_GLSL;
   end Pre_GLSL;

   function To_GLSL (U : Un_Op) return String is
      E_GLSL : String := U.E.To_GLSL;
      O_GLSL : String := (case U.Op is
         when Un_Min => "-",
         when Un_Length => "length",
         when Un_Normalize => "normalize",
         when Un_Abs => "abs",
         when Un_Float => "float",
         when Un_Sign => "sign");
   begin
      return O_GLSL & "(" & E_GLSL & ")";
   end To_GLSL;

   --  Builtin_Call

   function Eval (B : Builtin_Call; Ctx : Eval_Context) return Value is
      Arg_Values : Value_Array (B.Args'Range);
   begin
      for I in Arg_Values'Range loop
         Arg_Values (I) := B.Args.all (I).Eval (Ctx);
      end loop;

      case B.Builtin is
         when Builtin_Dot =>
            return Dot (Arg_Values (1), Arg_Values (2));
         when Builtin_Cross =>
            return Cross (Arg_Values (1), Arg_Values (2));
         when Builtin_Min =>
            return Min (Arg_Values (1), Arg_Values (2));
         when Builtin_Max =>
            return Max (Arg_Values (1), Arg_Values (2));
         when Builtin_Clamp =>
            return Clamp (Arg_Values (1), Arg_Values (2), Arg_Values (3));
         when Builtin_Pow =>
            return Arg_Values (1) ** Arg_Values (2);
         when Builtin_Sin =>
            return Sin (Arg_Values (1));
         when Builtin_Cos =>
            return Cos (Arg_Values (1));
         when Builtin_Tan =>
            return Tan (Arg_Values (1));
         when Builtin_Asin =>
            return Asin (Arg_Values (1));
         when Builtin_Acos =>
            return Acos (Arg_Values (1));
         when Builtin_Atan =>
            return Atan (Arg_Values (1));
         when Builtin_Vec3 =>
            return Values.Vector3
              ((Arg_Values (1).Float_Value,
                Arg_Values (2).Float_Value,
                Arg_Values (3).Float_Value));
      end case;
   end Eval;

   function Pre_GLSL (B : Builtin_Call) return String is
      Res : Unbounded_String;
   begin
      for A of B.Args.all loop
         Append (Res, A.Pre_GLSL);
      end loop;
      return To_String (Res);
   end Pre_GLSL;

   function To_GLSL (B : Builtin_Call) return String is
      Result : Unbounded_String;

      Builtin_Name : String :=
        (case B.Builtin is
           when Builtin_Dot   => "dot",
           when Builtin_Cross => "cross",
           when Builtin_Min   => "min",
           when Builtin_Max   => "max",
           when Builtin_Clamp => "clamp",
           when Builtin_Pow   => "pow",
           when Builtin_Sin   => "sin",
           when Builtin_Cos   => "cos",
           when Builtin_Tan   => "tan",
           when Builtin_Asin  => "asin",
           when Builtin_Acos  => "acos",
           when Builtin_Atan  => "atan",
           when Builtin_Vec3  => "vec3");
   begin
      Append (Result, Builtin_Name);
      Append (Result, "(");
      for I in B.Args.all'Range loop
         Append (Result, B.Args (I).To_GLSL);
         if I /= B.Args.all'Last then
            Append (Result, ", ");
         end if;
      end loop;
      Append (Result, ")");
      return To_String (Result);
   end To_GLSL;

   -- Project_Axis

   function Eval (P : Project_Axis; Ctx : Eval_Context) return Value is
      V : Value := P.E.Eval (Ctx);
   begin
      case V.Kind is
         when Vector3_Kind =>
            return Values.Float (V.Vector3_Value (P.A));
         when others =>
            raise Program_Error with "Cannot project component.";
      end case;
   end Eval;

   function Pre_GLSL (P : Project_Axis) return String is
     (P.E.Pre_GLSL);

   function To_GLSL  (P : Project_Axis) return String is
      E_GLSL : String := P.E.To_GLSL;
      A_GLSL : String := (case P.A is
         when GL.X => "x",
         when GL.Y => "y",
         when GL.Z => "z");
   begin
      return E_GLSL & "." & A_GLSL;
   end To_GLSL;

   --  Get_Component

   function Eval (G : Get_Component; Ctx : Eval_Context) return Value is
      Ent : Entity := Get (Ctx, G.Prefix);
   begin
      return Entities.Get (Ent, G.Suffix);
   end Eval;

   function To_GLSL (G : Get_Component) return String is
      P_GLSL : String := To_String (G.Prefix.Name);
      S_GLSL : String := Get_Name (G.Suffix);
   begin
      return P_GLSL & "." & S_GLSL;
   end To_GLSL;

   -- Var_Body

   function Eval (V : Var_Body; Ctx : Eval_Context) return Value is
      Var_Val : Value := V.Value.Eval (Ctx);
      New_Ctx : Eval_Context := Append (Ctx, V.Name, Var_Val);
   begin
      return R : Value := V.In_Body.Eval (New_Ctx) do
         Free (New_Ctx.Ref);
      end return;
   end Eval;

   function Pre_GLSL (V : Var_Body) return String is
      Pre_Val  : String := V.Value.Pre_GLSL;
      Var_Typ  : String := To_GLSL (V.Kind);
      Var_Val  : String := V.Value.To_GLSL;
      Var_Name : String := To_String (V.Name);
      Pre_Body : String := V.In_Body.Pre_GLSL;
   begin
      return
         Pre_Val & Var_Typ & " " & Var_Name & " = " & Var_Val & ";" & Pre_Body;
   end Pre_GLSL;

   function To_GLSL  (V : Var_Body) return String is
   begin
      return V.In_Body.To_GLSL;
   end To_GLSL;
end Madarch.Exprs;
