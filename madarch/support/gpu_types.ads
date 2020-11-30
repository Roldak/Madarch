with GNATCOLL.Strings;

with GL.Types;
with GPU_Buffers;

package GPU_Types is
   use GL;
   use GNATCOLL.Strings;

   type GPU_Type is tagged private;

   function Alignment (X : GPU_Type) return Types.Size;
   function Size (X : GPU_Type) return Types.Size;

   type GPU_Type_Array is array (Positive range <>) of GPU_Type;
   type GPU_Type_Array_Access is access all GPU_Type_Array;

   function Allocate
     (Typ     : GPU_Type;
      Kind    : GPU_Buffers.GPU_Buffer_Kind;
      Binding : Types.UInt) return GPU_Buffers.GPU_Buffer;

   type Named_Component is record
      Name : XString;
      Typ  : GPU_Type;
   end record;

   type Named_Component_Array is array (Positive range <>) of Named_Component;

   function Named
     (Self : GPU_Type; Name : String) return Named_Component;

   package Locations is
      type Location is tagged record
         Offset : Types.Size;
         Typ    : GPU_Type;
      end record;

      function Component (Self : Location; Index : Positive) return Location;
      function Component (Self : Location; Name  : String) return Location;

      procedure Adjust (Self : Location; W : in out GPU_Buffers.Writer);
   end Locations;

   function Address (Self : GPU_Type) return Locations.Location
      is ((Offset => 0, Typ => Self));

   function Component_Location
     (Self : GPU_Type; Component_Index : Positive) return Locations.Location;

   function Component_Location
     (Self : GPU_Type; Component_Name : String) return Locations.Location;
private
   type GPU_Type_Internal is abstract tagged null record;
   type GPU_Type_Internal_Access is access all GPU_Type_Internal'Class;

   type GPU_Type is tagged record
      T : GPU_Type_Internal_Access;
   end record;

   function Alignment (X : GPU_Type_Internal) return Types.Size is abstract;
   function Size (X : GPU_Type_Internal) return Types.Size is abstract;

   function Component_Location
     (Self : GPU_Type_Internal;
      Component_Index : Positive) return Locations.Location;

   function Component_Location
     (Self : GPU_Type_Internal;
      Component_Name : String) return Locations.Location;

   function Alignment (X : GPU_Type) return Types.Size is
     (X.T.Alignment);

   function Size (X : GPU_Type) return Types.Size is
     (X.T.Size);

   function Component_Location
     (Self : GPU_Type; Component_Index : Positive) return Locations.Location
   is (Self.T.Component_Location (Component_Index));

   function Component_Location
     (Self : GPU_Type; Component_Name : String) return Locations.Location
   is (Self.T.Component_Location (Component_Name));

   procedure Pad (X : in out Types.Size; Amount : Types.Size);
end GPU_Types;
