package body Madarch.Lights is
   function Create
     (Name     : String;
      Comps    : Components.Component_Array;
      Sample   : Light_Sample_Function;
      Position : Light_Position_Function) return Light
   is
   begin
      return new Light_Internal'
        (Name     => To_Unbounded_String (Name),
         Comps    => new Components.Component_Array'(Comps),
         Sample   => Sample,
         Position => Position);
   end Create;

   function Get_Name (L : Light) return String is
     (To_String (L.Name));

   function Get_Components
     (L : Light) return Components.Component_Array
   is (L.Comps.all);

   function Get_Sample_Expr
     (L     : Light;
      Inst  : Exprs.Struct_Expr;
      Point : Exprs.Expr;
      Norm  : Exprs.Expr) return Exprs.Expr'Class
   is (L.Sample (Inst, Point, Norm));

   function Get_Position_Expr
     (L : Light; Inst : Exprs.Struct_Expr) return Exprs.Expr'Class
   is (L.Position (Inst));
end Madarch.Lights;
