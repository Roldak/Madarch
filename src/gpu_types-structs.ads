package GPU_Types.Structs is
   type Struct is new GPU_Type with private;

   overriding function Alignment (X : Struct) return Types.Size is (16);
   overriding function Size (X : Struct) return Types.Size;

   function Create
     (Component_Types : GPU_Type_Array) return Struct;

   overriding function Component_Location
     (Self : Struct; Index : Positive) return Locations.Location;
private

   type Struct is new GPU_Type with record
      Components : GPU_Type_Array_Access;
   end record;
end GPU_Types.Structs;
