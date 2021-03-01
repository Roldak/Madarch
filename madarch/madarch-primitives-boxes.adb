package body Madarch.Primitives.Boxes is
   Zero_Vector : constant Expr'Class := Literal (Vector3 ((0.0, 0.0, 0.0)));
   Zero_Float  : constant Expr'Class := Literal (Values.Float (0.0));

   function Get_Distance (S : Struct_Expr; P : Expr'Class) return Expr'Class is
      Q : Expr'Class := Value_Identifier ("q");
   begin
      return (S.Get (Center)."-" (P).Abs_Value."-" (S.Get (Side)).Let_In
        (Vector3_Kind,
         "q",
         Q.Max (Zero_Vector).Length
         + Q.Get (GL.X).Max (Q.Get (GL.Y).Max (Q.Get (GL.Z))).Min (Zero_Float)));
   end Get_Distance;

   function Get_Normal (S : Struct_Expr; P : Expr'Class) return Expr'Class is
      D : Expr'Class := Value_Identifier ("d");
      RX : Expr'Class := Value_Identifier ("rx");
      RY : Expr'Class := Value_Identifier ("ry");
      RZ : Expr'Class := Value_Identifier ("rz");

      E : Expr'Class := Value_Identifier ("epsilon");

      Normal_Dir : Expr'Class := Construct_Vector3
        (To_Float (RX > RY - E) * To_Float (RX > RZ - E) * D.Get (GL.X).Sign,
         To_Float (RY > RX - E) * To_Float (RY > RZ - E) * D.Get (GL.Y).Sign,
         To_Float (RZ > RX - E) * To_Float (RZ > RY - E) * D.Get (GL.Z).Sign);
   begin
      return P."-" (S.Get (Center))."/" (S.Get (Side)).Let_In
        (Vector3_Kind,
         "d",
         D.Get (GL.X).Abs_Value.Let_In
           (Float_Kind,
            "rx",
            D.Get (GL.Y).Abs_Value.Let_In
              (Float_Kind,
               "ry",
               D.Get (GL.Z).Abs_Value.Let_In
                 (Float_Kind,
                  "rz",
                  Normal_Dir.Normalize))));
   end Get_Normal;
end Madarch.Primitives.Boxes;
