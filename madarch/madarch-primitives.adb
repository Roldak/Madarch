package body Madarch.Primitives is
   function Create
     (Name     : String;
      Comps    : Components.Component_Array;
      Distance : Struct_Point_Function;
      Normal   : Struct_Point_Function) return Primitive
   is
   begin
      return new Primitive_Internal'
        (Name     => To_Unbounded_String (Name),
         Comps    => new Components.Component_Array'(Comps),
         Distance => Distance,
         Normal   => Normal);
   end Create;

   function Get_Name (Prim : Primitive) return String is
     (To_String (Prim.Name));

   function Get_Components
     (Prim : Primitive) return Components.Component_Array
   is (Prim.Comps.all);

   function Get_Dist_Expr
     (Prim  : Primitive;
      Inst  : Exprs.Struct_Expr;
      Point : Exprs.Expr) return Exprs.Expr'Class
   is (Prim.Distance (Inst, Point));

   function Get_Normal_Expr
     (Prim  : Primitive;
      Inst  : Exprs.Struct_Expr;
      Point : Exprs.Expr) return Exprs.Expr'Class
   is (Prim.Normal (Inst, Point));
end Madarch.Primitives;
