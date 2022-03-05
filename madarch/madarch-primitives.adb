with Ada.Unchecked_Conversion;

with System;

with Madarch.Values;

package body Madarch.Primitives is
   function Create
     (Name     : String;
      Comps    : Components.Component_Array;
      Distance : Primitive_Point_Function;
      Normal   : Primitive_Point_Function;
      Material : Primitive_Function) return Primitive
   is
   begin
      return new Primitive_Internal'
        (Name     => To_Unbounded_String (Name),
         Comps    => new Components.Component_Array'(Comps),
         Distance => Distance,
         Normal   => Normal,
         Material => Material);
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

   function Get_Material_Expr
     (Prim : Primitive;
      Inst : Exprs.Struct_Expr) return Exprs.Expr'Class
   is (Prim.Material (Inst));

   type Point_Expr_Fun is access function
     (Prim  : Primitive;
      Inst  : Exprs.Struct_Expr;
      Point : Exprs.Expr) return Exprs.Expr'Class;

   function Eval_Expr_From_Point
     (Func   : Point_Expr_Fun;
      Prim   : Primitive;
      Entity : Entities.Entity;
      Point  : Singles.Vector3) return Values.Value
   is
      Entity_Expr  : Exprs.Struct_Expr := Exprs.Struct_Identifier ("prim");
      Point_Expr   : Exprs.Expr        := Exprs.Value_Identifier ("x");
      Compute_Expr : Exprs.Expr'Class  := Func (Prim, Entity_Expr, Point_Expr);
      Ctx : Exprs.Eval_Context := Exprs.Create;
   begin
      Ctx := Exprs.Append (Ctx, "prim", Entity);
      Ctx := Exprs.Append (Ctx, "x",    Values.Vector3 (Point));
      return Compute_Expr.Eval (Ctx);
   end Eval_Expr_From_Point;

   function Eval_Dist
     (Prim   : Primitive;
      Entity : Entities.Entity;
      Point  : Singles.Vector3) return Single
   is
      Res : Values.Value := Eval_Expr_From_Point
        (Get_Dist_Expr'Access, Prim, Entity, Point);
   begin
      case Res.Kind is
         when Values.Float_Kind =>
            return Res.Float_Value;
         when others =>
            raise Program_Error with "Unexpected value kind.";
      end case;
   end Eval_Dist;

   function Eval_Normal
     (Prim   : Primitive;
      Entity : Entities.Entity;
      Point  : Singles.Vector3) return Singles.Vector3
   is
      Res : Values.Value := Eval_Expr_From_Point
        (Get_Normal_Expr'Access, Prim, Entity, Point);
   begin
      case Res.Kind is
         when Values.Vector3_Kind =>
            return Res.Vector3_Value;
         when others =>
            raise Program_Error with "Unexpected value kind.";
      end case;
   end Eval_Normal;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Long_Integer) with Inline;

   function Hash (Prim : Primitive) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Convert (Prim.all'Address)));
end Madarch.Primitives;
