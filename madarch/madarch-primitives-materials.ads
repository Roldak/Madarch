with Madarch.Values;

package Madarch.Primitives.Materials is
   use Components;
   use Exprs;
   use Values;

   Material_Id : constant Component := Create ("material_id", Int_Kind);

   function Get_Material_Id (Inst : Struct_Expr) return Expr'Class is
     (Inst.Get (Material_Id));
end Madarch.Primitives.Materials;
