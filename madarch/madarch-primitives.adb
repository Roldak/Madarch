with Ada.Unchecked_Conversion;

with System;

with Madarch.Values;

package body Madarch.Primitives is
   Entity_Var_Name : constant Unbounded_String :=
      To_Unbounded_String ("prim");

   Point_Var_Name : constant Unbounded_String :=
      To_Unbounded_String ("x");

   Entity_Expr : constant Exprs.Struct_Expr :=
      Exprs.Struct_Identifier (To_String (Entity_Var_Name));

   Point_Expr  : constant Exprs.Expr :=
      Exprs.Value_Identifier (To_String (Point_Var_Name));

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
         Material => Material,
         Cached_Dist_Expr => null,
         Cached_Norm_Expr => null);
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
     (Exp    : Exprs.Expr'Class;
      Entity : Entities.Entity;
      Point  : Singles.Vector3) return Values.Value
   is
      Ctx : Exprs.Eval_Context := Exprs.Empty_Context;
   begin
      Ctx := Exprs.Append (Ctx, Entity_Var_Name, Entity);
      Ctx := Exprs.Append (Ctx, Point_Var_Name, Values.Vector3 (Point));
      return V : Values.Value := Exp.Eval (Ctx) do
         Exprs.Free (Ctx);
      end return;
   end Eval_Expr_From_Point;

   function Cache_Distance
     (Prim : Primitive;
      Expr : Exprs.Expr'Class) return Exprs.Expr'Class
   is
   begin
      Prim.Cached_Dist_Expr := new Exprs.Expr'Class'(Expr);
      return Expr;
   end Cache_Distance;

   function Eval_Dist
     (Prim   : Primitive;
      Entity : Entities.Entity;
      Point  : Singles.Vector3) return Single
   is
      Expr : Exprs.Expr'Class :=
        (if Prim.Cached_Dist_Expr /= null
         then Prim.Cached_Dist_Expr.all
         else Cache_Distance (Prim, Prim.Distance (Entity_Expr, Point_Expr)));

      Res : Values.Value := Eval_Expr_From_Point (Expr, Entity, Point);
   begin
      case Res.Kind is
         when Values.Float_Kind =>
            return Res.Float_Value;
         when others =>
            raise Program_Error with "Unexpected value kind.";
      end case;
   end Eval_Dist;

   function Cache_Normal
     (Prim : Primitive;
      Expr : Exprs.Expr'Class) return Exprs.Expr'Class
   is
   begin
      Prim.Cached_Norm_Expr := new Exprs.Expr'Class'(Expr);
      return Expr;
   end Cache_Normal;

   function Eval_Normal
     (Prim   : Primitive;
      Entity : Entities.Entity;
      Point  : Singles.Vector3) return Singles.Vector3
   is
      Expr : Exprs.Expr'Class :=
        (if Prim.Cached_Norm_Expr /= null
         then Prim.Cached_Norm_Expr.all
         else Cache_Normal (Prim, Prim.Normal (Entity_Expr, Point_Expr)));

      Res : Values.Value := Eval_Expr_From_Point (Expr, Entity, Point);
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
