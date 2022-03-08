with Madarch.Primitives.Materials;
with Madarch.Values;

package Madarch.Primitives.Triangles is
   use Components;
   use Exprs;
   use Materials;
   use Values;

   V1 : constant Component := Create ("v1", Vector3_Kind);
   V2 : constant Component := Create ("v2", Vector3_Kind);
   V3 : constant Component := Create ("v3", Vector3_Kind);

   function Get_Distance (S : Struct_Expr; P : Expr'Class) return Expr'Class;

   function Get_Normal (S : Struct_Expr; P : Expr'Class) return Expr'Class;

   Triangle : constant Primitive := Create
     ("Triangle",
      (V1, V2, V3, Material_Id),
      Get_Distance'Access,
      Get_Normal'Access,
      Get_Material_Id'Access);

   function Create
     (Instance_V1          : Singles.Vector3;
      Instance_V2          : Singles.Vector3;
      Instance_V3          : Singles.Vector3;
      Instance_Material_Id : GL.Types.Int) return Entities.Entity
   is (Entities.Create
        (((V1,          Values.Vector3 (Instance_V1)),
          (V2,          Values.Vector3 (Instance_V2)),
          (V3,          Values.Vector3 (Instance_V3)),
          (Material_Id, Values.Int (Instance_Material_Id)))));
end Madarch.Primitives.Triangles;

