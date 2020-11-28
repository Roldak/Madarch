package GPU_Types.Structs is
   type Struct is new GPU_Type with private;

   overriding function Alignment (X : Struct) return Types.Size is (16);
   overriding function Size (X : Struct) return Types.Size;

   function Create
     (Named_Components : Named_Component_Array) return Struct;

   overriding function Component_Location
     (Self : Struct; Name : String) return Locations.Location;
private
   type Named_Component_Array_Access is access Named_Component_Array;

   type Struct is new GPU_Type with record
      Components : Named_Component_Array_Access;
   end record;
end GPU_Types.Structs;
