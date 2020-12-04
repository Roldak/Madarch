with Madarch.Primitives.Materials;
with Madarch.Values;

package Madarch.Primitives.Planes is
   use Components;
   use Exprs;
   use Materials;
   use Values;

   Normal : constant Component := Create ("normal", Vector3_Kind);
   Offset : constant Component := Create ("offset", Float_Kind);

   function Distance (S : Struct_Expr; P : Expr'Class) return Expr'Class is
     (Dot (S.Get (Normal), P) + S.Get (Offset));

   function Get_Normal (S : Struct_Expr; P : Expr'Class) return Expr'Class is
     (S.Get (Normal));

   Plane : constant Primitive := Create
     ("Plane",
      (Normal, Offset, Material_Id),
      Distance'Access,
      Get_Normal'Access,
      Get_Material_Id'Access);

   function Create
     (Instance_Normal      : Singles.Vector3;
      Instance_Offset      : Single;
      Instance_Material_Id : GL.Types.Int) return Entities.Entity
   is (Entities.Create
        (((Normal,      Values.Vector3 (Instance_Normal)),
          (Offset,      Values.Float (Instance_Offset)),
          (Material_Id, Values.Int (Instance_Material_Id)))));
end Madarch.Primitives.Planes;
