with GL.Types;

package Bounding_Boxes is
   use GL.Types;

   type Bounding_Box is record
      From, To : Singles.Vector3;
   end record;

   procedure Extend
     (BB : in out Bounding_Box; Point : Singles.Vector3);

   procedure Extend
     (BB : in out Bounding_Box; Other : Bounding_Box);

   function Surface_Area (B : Bounding_Box) return Single;

   function Contains (Self, Other : Bounding_Box) return Boolean;

   function Image (B : Bounding_Box) return String;
end Bounding_Boxes;
