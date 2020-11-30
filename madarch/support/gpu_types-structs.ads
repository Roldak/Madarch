package GPU_Types.Structs is
   function Create
     (Named_Components : Named_Component_Array) return GPU_Type;
private
   type Named_Component_Array_Access is access Named_Component_Array;

   type Struct is new GPU_Type_Internal with record
      Components : Named_Component_Array_Access;
   end record;

   overriding function Alignment (X : Struct) return Types.Size is (16);
   overriding function Size (X : Struct) return Types.Size;

   overriding function Component_Location
     (Self : Struct; Name : String) return Locations.Location;
end GPU_Types.Structs;
