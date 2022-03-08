package Madarch.Exprs.Derivatives is
   function Forward_Difference
     (Exp   : Expr'Class;
      Param : String;
      Point : Expr'Class) return Expr'Class;
end Madarch.Exprs.Derivatives;
