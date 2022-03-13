package body Madarch.Exprs.Transformers is
   procedure Transform
     (T : in out Transformers.Transformer'Class; E : in out Expr)
   is
   begin
      if E.Value.all in Var_Body then
         declare
            B : Var_Body renames Var_Body (E.Value.all);
         begin
            E := T.Transform_Let (E, B.Decls, B.In_Body);
         end;
      else
         E.Value.Transform (T);
      end if;
   end Transform;
end Madarch.Exprs.Transformers;

