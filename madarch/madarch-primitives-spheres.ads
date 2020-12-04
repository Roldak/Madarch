with Madarch.Primitives.Materials;
with Madarch.Values;

package Madarch.Primitives.Spheres is
   use Components;
   use Exprs;
   use Materials;
   use Values;

   Center : constant Component := Create ("center", Vector3_Kind);
   Radius : constant Component := Create ("radius", Float_Kind);

   function Distance (S : Struct_Expr; P : Expr'Class) return Expr'Class is
     (S.Get (Center)."-" (P).Length - S.Get (Radius));

   function Normal (S : Struct_Expr; P : Expr'Class) return Expr'Class is
     (P."-" (S.Get (Center)).Normalize);

   Sphere : Primitive := Create
     ("Sphere",
      (Center, Radius, Material_Id),
      Distance'Access,
      Normal'Access,
      Get_Material_Id'Access);

   function Create
     (Instance_Center      : Singles.Vector3;
      Instance_Radius      : Single;
      Instance_Material_Id : GL.Types.Int) return Entities.Entity
   is (Entities.Create
        (((Center,      Values.Vector3 (Instance_Center)),
          (Radius,      Values.Float (Instance_Radius)),
          (Material_Id, Values.Int (Instance_Material_Id)))));
end Madarch.Primitives.Spheres;
