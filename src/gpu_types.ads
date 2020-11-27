with GNATCOLL.Strings;

with GL.Types;
with GPU_Buffers;

package GPU_Types is
   use GL;
   use GNATCOLL.Strings;

   type GPU_Type;
   type GPU_Type_Access is access constant GPU_Type'Class;

   type GPU_Type is abstract tagged private;

   function Alignment (X : GPU_Type) return Types.Size is abstract;
   function Size (X : GPU_Type) return Types.Size is abstract;

   type GPU_Type_Array is array (Positive range <>) of GPU_Type_Access;
   type GPU_Type_Array_Access is access all GPU_Type_Array;

   function Allocate
     (Typ     : GPU_Type'Class;
      Kind    : GPU_Buffers.GPU_Buffer_Kind;
      Binding : Types.UInt) return GPU_Buffers.GPU_Buffer;

   type Named_Component is record
      Name : XString;
      Typ  : GPU_Type_Access;
   end record;

   type Named_Component_Array is array (Positive range <>) of Named_Component;

   function Named
     (Self : aliased GPU_Type; Name : String) return Named_Component;

   package Locations is
      type Location is tagged record
         Offset : Types.Size;
         Typ    : GPU_Type_Access;
      end record;

      function Component (Self : Location; Index : Positive) return Location;
      function Component (Self : Location; Name  : String) return Location;

      procedure Adjust (Self : Location; W : in out GPU_Buffers.Writer);
   end Locations;

   function Address (Self : aliased GPU_Type) return Locations.Location
      is ((Offset => 0, Typ => Self'Unchecked_Access));

   function Component_Location
     (Self : GPU_Type; Component_Index : Positive) return Locations.Location;

   function Component_Location
     (Self : GPU_Type; Component_Name : String) return Locations.Location;
private
   type GPU_Type is abstract tagged null record;

   procedure Pad (X : in out Types.Size; Amount : Types.Size);
end GPU_Types;
