package Madarch.Exprs.Transformers is
   type Transformer is abstract tagged null record;

   function Transform_Let
     (Self    : in out Transformer;
      Orig    : Expr;
      Decls   : in out Var_Decl_Array;
      In_Body : in out Expr) return Expr is (Orig);

   procedure Transform
     (T : in out Transformers.Transformer'Class; E : in out Expr);
end Madarch.Exprs.Transformers;
