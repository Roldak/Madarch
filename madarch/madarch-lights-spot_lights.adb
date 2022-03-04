package body Madarch.Lights.Spot_Lights is
   Zero_Float : constant Expr'Class := Literal (Values.Float (0.0));
   One_Float  : constant Expr'Class := Literal (Values.Float (1.0));

   function Get_Color
     (L : Struct_Expr; Pos, Normal, Dir, Dist : Expr'Class) return Expr'Class
   is
      Attenuation : constant Expr'Class :=
         One_Float / (Dist * Dist * Values.Float (0.03));

      Theta : constant Expr'Class :=
         Acos (Max (Dot (-Dir, L.Get (Direction)), Zero_Float));

      Ratio : constant Expr'Class :=
         Clamp (Theta / L.Get (Aperture), Zero_Float, One_Float);

      Visible : constant Expr'Class :=
         One_Float - Ratio ** Values.Float (8.0);
   begin
      return
         L.Get (Color)
         * Min (Attenuation, Literal (Values.Float (1.5)))
         * Visible;
   end Get_Color;
end Madarch.Lights.Spot_Lights;

