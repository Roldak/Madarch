with GL.Types;

package Madarch.Exprs.Derivatives is
   function Forward_Difference
     (Exp     : Expr'Class;
      Param   : String;
      Point   : Expr'Class;
      Epsilon : GL.Types.Single := 0.000001) return Expr'Class;
end Madarch.Exprs.Derivatives;
