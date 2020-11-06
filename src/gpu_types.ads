with GL.Types;
with UBOs;

package GPU_Types is
   use GL;

   type GPU_Type;
   type GPU_Type_Access is access constant GPU_Type'Class;

   type GPU_Type is abstract tagged private;

   function Alignment (X : GPU_Type) return Types.Size is abstract;
   function Size (X : GPU_Type) return Types.Size is abstract;

   type GPU_Type_Array is array (Positive range <>) of GPU_Type_Access;
   type GPU_Type_Array_Access is access all GPU_Type_Array;

   function Allocate
     (X : GPU_Type'Class; Binding : Types.UInt) return UBOs.UBO;

   package Locations is
      type Location is tagged record
         Offset : Types.Size;
         Typ    : GPU_Type_Access;
      end record;

      function Component (Self : Location; Index : Positive) return Location;
      procedure Adjust (Self : Location; W : in out UBOs.Writer);
   end Locations;

   function Address (Self : aliased GPU_Type) return Locations.Location
      is ((Offset => 0, Typ => Self'Unchecked_Access));

   function Component_Location
     (Self : GPU_Type; Component_Index : Positive) return Locations.Location;
private
   type GPU_Type is abstract tagged null record;

   procedure Pad (X : in out Types.Size; Amount : Types.Size);
end GPU_Types;
