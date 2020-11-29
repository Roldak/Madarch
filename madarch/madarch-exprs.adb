with GL;

package body Madarch.Exprs is
   function Literal (V : Value) return Expr is
     (Value => new Lit'(V => V));

   function Eval (E : Expr; Ctx : Eval_Context) return Value is
     (E.Value.Eval (Ctx));

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

   function Length (E : Expr) return Expr is
     (Value => new Un_Op'(Un_Length, E));

   function Normalize (E : Expr) return Expr is
     (Value => new Un_Op'(Un_Normalize, E));

   function Get (E : Struct_Expr; C : Component) return Expr'Class is
      R : Expr := (Value => new Get_Component'(E, C));
   begin
      return R;
   end Get;

   --  Ident

   function Eval (I : Ident; Ctx : Eval_Context) return Value is
     (Values.Float (0.0));

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
      end case;
   end Eval;

   function To_GLSL (B : Bin_Op) return String is
      L_GLSL : String := B.Lhs.To_GLSL;
      R_GLSL : String := B.Rhs.To_GLSL;
      O_GLSL : String := (case B.Op is
         when Bin_Add => "+",
         when Bin_Sub => "-",
         when Bin_Mul => "*",
         when Bin_Div => "/");
   begin
      return "(" & L_GLSL & ")" & O_GLSL & "(" & R_GLSL & ")";
   end To_GLSL;

   --  Un_Op

   function Eval (U : Un_Op; Ctx : Eval_Context) return Value is
      E_Value : Value := U.E.Eval (Ctx);
   begin
      case U.Op is
         when Un_Min =>
            return -E_Value;
         when Un_Length =>
            return Length (E_Value);
         when Un_Normalize =>
            return Normalize (E_Value);
      end case;
   end Eval;

   function To_GLSL (U : Un_Op) return String is
      E_GLSL : String := U.E.To_GLSL;
      O_GLSL : String := (case U.Op is
         when Un_Min => "-",
         when Un_Length => "length",
         when Un_Normalize => "normalize");
   begin
      return O_GLSL & "(" & E_GLSL & ")";
   end To_GLSL;

   --  Get_Component

   function Eval (G : Get_Component; Ctx : Eval_Context) return Value is
   begin
      return Values.Float (0.0);
   end Eval;

   function To_GLSL (G : Get_Component) return String is
      P_GLSL : String := To_String (G.Prefix.Name);
      S_GLSL : String := Get_Name (G.Suffix);
   begin
      return P_GLSL & "." & S_GLSL;
   end To_GLSL;
end Madarch.Exprs;
