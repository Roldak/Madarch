with GL.Types;

package Bounding_Boxes is
   use GL.Types;

   type Bounding_Box is record
      From, To : Singles.Vector3;
   end record;

   function Surface_Area (B : Bounding_Box) return Single;
end Bounding_Boxes;
