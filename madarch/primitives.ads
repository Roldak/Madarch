with GL.Types;

package Primitives is
   use GL.Types;

   type Primitive_Kind is (Sphere, Plane, Cube);

   type Primitive (Kind : Primitive_Kind := Sphere) is record
      Material : Int;

      case Kind is
         when Sphere =>
            Sphere_Center : Singles.Vector3;
            Sphere_Radius : Single;
         when Plane =>
            Plane_Normal : Singles.Vector3;
            Plane_Offset : Single;
         when Cube =>
            Cube_Center : Singles.Vector3;
            Cube_Side   : Single;
      end case;
   end record;

   type Primitive_Array is array (Positive range <>) of Primitive;
   type Primitive_Array_Access is access all Primitive_Array;
end Primitives;
