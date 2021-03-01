with Madarch.Primitives.Materials;
with Madarch.Values;

package Madarch.Primitives.Boxes is
   use Components;
   use Exprs;
   use Materials;
   use Values;

   Center : constant Component := Create ("center", Vector3_Kind);
   Side   : constant Component := Create ("side",   Vector3_Kind);

   function Get_Distance (S : Struct_Expr; P : Expr'Class) return Expr'Class;

   function Get_Normal (S : Struct_Expr; P : Expr'Class) return Expr'Class;

   Box : constant Primitive := Create
     ("Box",
      (Center, Side, Material_Id),
      Get_Distance'Access,
      Get_Normal'Access,
      Get_Material_Id'Access);

   function Create
     (Instance_Center      : Singles.Vector3;
      Instance_Side        : Singles.Vector3;
      Instance_Material_Id : GL.Types.Int) return Entities.Entity
   is (Entities.Create
        (((Center,      Values.Vector3 (Instance_Center)),
          (Side,        Values.Vector3 (Instance_Side)),
          (Material_Id, Values.Int (Instance_Material_Id)))));
end Madarch.Primitives.Boxes;
