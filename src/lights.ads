with GL.Types; use GL.Types;

package Lights is
   type Light_Kind is (Point);

   type Light (Kind : Light_Kind := Point) is record
      Light_Color : Singles.Vector3;

      case Kind is
         when Point =>
            Point_Light_Pos : Singles.Vector3;
      end case;
   end record;
end Lights;
