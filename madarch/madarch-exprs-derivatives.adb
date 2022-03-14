with Madarch.Values;

package body Madarch.Exprs.Derivatives is
   use GL.Types;
   use Values;

   F_P_Name : constant Unbounded_String := To_Unbounded_String ("f_p");
   F_X_Name : constant Unbounded_String := To_Unbounded_String ("f_x");
   F_Y_Name : constant Unbounded_String := To_Unbounded_String ("f_y");
   F_Z_Name : constant Unbounded_String := To_Unbounded_String ("f_z");

   function Forward_Difference
     (Exp     : Expr'Class;
      Param   : String;
      Point   : Expr'Class;
      Epsilon : Single := 0.000001) return Expr'Class
   is
      H_X : constant Expr'Class := Literal (Vector3 ((Epsilon, 0.0, 0.0)));
      H_Y : constant Expr'Class := Literal (Vector3 ((0.0, Epsilon, 0.0)));
      H_Z : constant Expr'Class := Literal (Vector3 ((0.0, 0.0, Epsilon)));

      F_P : constant Expr'Class := Value_Identifier (F_P_Name);
      F_X : constant Expr'Class := Value_Identifier (F_X_Name);
      F_Y : constant Expr'Class := Value_Identifier (F_Y_Name);
      F_Z : constant Expr'Class := Value_Identifier (F_Z_Name);
   begin
      return Let_In
        (((Float_Kind,
           F_P_Name,
           Expr (Let_In (Point, Vector3_Kind, Param, Exp))),

          (Float_Kind,
           F_X_Name,
           Expr (Let_In (Point + H_X, Vector3_Kind, Param, Exp) - F_P)),

          (Float_Kind,
           F_Y_Name,
           Expr (Let_In (Point + H_Y, Vector3_Kind, Param, Exp) - F_P)),

          (Float_Kind,
           F_Z_Name,
           Expr (Let_In (Point + H_Z, Vector3_Kind, Param, Exp) - F_P))),

         Construct_Vector3 (F_X, F_Y, F_Z));
   end Forward_Difference;
end Madarch.Exprs.Derivatives;

