with Madarch.Exprs.Derivatives;

package body Madarch.Primitives.Triangles is
   V21_Name : constant Unbounded_String := To_Unbounded_String ("V21");
   V32_Name : constant Unbounded_String := To_Unbounded_String ("V32");
   V13_Name : constant Unbounded_String := To_Unbounded_String ("V13");
   P1_Name  : constant Unbounded_String := To_Unbounded_String ("P1");
   P2_Name  : constant Unbounded_String := To_Unbounded_String ("P2");
   P3_Name  : constant Unbounded_String := To_Unbounded_String ("P3");
   Nor_Name : constant Unbounded_String := To_Unbounded_String ("Nor");

   F_0 : constant Expr := Literal (Values.Float (0.0));
   F_1 : constant Expr := Literal (Values.Float (1.0));
   F_2 : constant Expr := Literal (Values.Float (2.0));

   function Get_Distance (S : Struct_Expr; P : Expr'Class) return Expr'Class is
      V21 : constant Expr := Value_Identifier (V21_Name);
      V32 : constant Expr := Value_Identifier (V32_Name);
      V13 : constant Expr := Value_Identifier (V13_Name);
      P1  : constant Expr := Value_Identifier (P1_Name);
      P2  : constant Expr := Value_Identifier (P2_Name);
      P3  : constant Expr := Value_Identifier (P3_Name);
      Nor : constant Expr := Value_Identifier (Nor_Name);

      Cond : constant Expr :=
        (V21.Cross (Nor).Dot (P1).Sign +
         V32.Cross (Nor).Dot (P2).Sign +
         V13.Cross (Nor).Dot (P3).Sign) < F_2;

      Thn : constant Expr := Min
        (Dot2 (V21 * Clamp (V21.Dot (P1) / V21.Dot2, F_0, F_1) - P1),
         Dot2 (V32 * Clamp (V32.Dot (P2) / V32.Dot2, F_0, F_1) - P2),
         Dot2 (V13 * Clamp (V13.Dot (P3) / V13.Dot2, F_0, F_1) - P3));

      Els : constant Expr :=
         Nor.Dot (P1) * Nor.Dot (P1) / Nor.Dot2;

   begin
      return Let_In
        (((Vector3_Kind, V21_Name, Expr (S.Get (V2) - S.Get (V1))),
          (Vector3_Kind, V32_Name, Expr (S.Get (V3) - S.Get (V2))),
          (Vector3_Kind, V13_Name, Expr (S.Get (V1) - S.Get (V3))),
          (Vector3_Kind, P1_Name,  Expr (P - S.Get (V1))),
          (Vector3_Kind, P2_Name,  Expr (P - S.Get (V2))),
          (Vector3_Kind, P3_Name,  Expr (P - S.Get (V3))),
          (Vector3_Kind, Nor_Name, Expr (V21.Cross (V13)))),
         If_Then_Else (Cond, Thn, Els).Sqrt);
   end Get_Distance;

   function Get_Normal (S : Struct_Expr; P : Expr'Class) return Expr'Class is
      Dist_Expr : constant Expr'Class :=
        Get_Distance (S, Value_Identifier ("DX"));
   begin
      return Exprs.Derivatives.Forward_Difference
        (Dist_Expr, "DX", P).Normalize;
   end Get_Normal;
end Madarch.Primitives.Triangles;

