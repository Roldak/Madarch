with GL.Types; use GL.Types;

package Lights is
   type Light_Kind is (Point, Spot);

   type Light (Kind : Light_Kind := Point) is record
      Light_Color : Singles.Vector3;

      case Kind is
         when Point =>
            Point_Light_Pos : Singles.Vector3;
         when Spot =>
            Spot_Light_Pos      : Singles.Vector3;
            Spot_Light_Dir      : Singles.Vector3;
            Spot_Light_Aperture : Single;
      end case;
   end record;

   type Light_Array is array (Positive range <>) of Light;
end Lights;
